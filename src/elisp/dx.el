;;; dx.el --- DNAnexus Platform Bindings

;; Copyright (C) 2012 DNAnexus, Inc.

;; Author: Phil Sung <psung@dnanexus.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Data browser and other tools for interacting with the DNAnexus
;; Platform.

;;; Code:

(require 'dx-api)

(defvar dx-current-project-context)

(defun dx-select-project (project)
  (interactive "sProject ID: ")
  (setq dx-current-project-context project))

;;; dx-list-projects-mode
;;; ---------------------

;; dx-list-projects-mode is suitable only for specially formatted data.
(put 'dx-list-projects-mode 'mode-class 'special)

(defvar dx-list-projects-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'dx-list-projects-refresh)
    (define-key map "q" 'bury-buffer)
    (define-key map "\C-m" 'dx-list-projects-browse-project)
    map))

(define-derived-mode dx-list-projects-mode fundamental-mode
  "dx List Projects"
  "Mode for listing projects."
  (kill-all-local-variables)
  (use-local-map dx-list-projects-mode-map)
  (setq major-mode 'dx-list-projects-mode
        mode-name "dx List Projects"
        buffer-read-only t))

(defun dx-list-projects-load-buffer (select-p)
  "Renders a list-projects buffer."
  (dx-api-system-find-projects
   `(:describe t)
   (lambda (data select-p)
     (let ((buf (get-buffer-create "*dx-list-projects*")))
       (with-current-buffer buf
         (let ((buffer-read-only nil))
           (erase-buffer)
           (insert (propertize "All visible projects:\n" 'face 'bold))
           (mapc
            (lambda (project)
              (let ((project-id (cdr (assoc 'id project)))
                    (project-name (cdr (assoc 'name (cdr (assoc 'describe project)))))
                    ;; TODO: align project names
                    (level (cdr (assoc 'level project))))
                (insert " " (propertize project-id 'face 'link) " " level " " project-name "\n")))
            (cdr (assoc 'results data)))
           ;; Go to the first entry.
           (goto-char (point-min))
           (forward-line))
         (dx-list-projects-mode))
       (and select-p
            (switch-to-buffer buf))))
   `(,select-p)))

(defun dx-list-projects ()
  (interactive)
  (dx-list-projects-load-buffer t))

(defun dx-list-projects-refresh ()
  (interactive)
  (dx-list-projects-load-buffer nil))

(defun dx-list-projects-browse-project ()
  "Browses the project whose entry is at point."
  (interactive)
  (save-excursion
    ;; TODO: I don't think this works with narrowing.
    (let (p1 p2)
      (beginning-of-line)
      ;; Expect line to look like this:
      ;;   project-XXXXXXXXXXXXXXXX [more stuff...]
      ;;
      ;; TODO: detect when we are not on a project line
      ;;
      ;; TODO: maybe higher specificity. Investigate use of "buttons".
      (setq p1 (progn
                 (search-forward "project")
                 (match-beginning 0)))
      (setq p2 (progn
                 (search-forward " ")
                 (match-beginning 0)))
      (let ((project (buffer-substring p1 p2)))
        ;; Remove the link face from project name
        (set-text-properties 0 (length project) nil project)
        (dx-browse-project project)))))

;;; dx-browse-project-mode
;;; ----------------------

(defun dx-browse-project (&optional project folder)
  (interactive)
  (let ((project (or project dx-current-project-context))
        ;; TODO: also be able to find an existing buffer for the project, if
        ;; folder is not given.
        (folder (or folder "/")))
    (dx-api-project-list-folder project
                                `(:folder ,folder :describe t)
                                (lambda (data project)
                                  (let ((buf (get-buffer-create (concat "*dx-browse-" project "*"))))
                                    (with-current-buffer buf
                                      (setq buffer-read-only t)
                                      (let ((buffer-read-only nil))
                                        (erase-buffer)
                                        (insert (propertize (concat "Contents of " project ":\n") 'face 'bold))
                                        ;; Show folders first, then other objects.
                                        (mapc
                                         (lambda (folder) (insert " (folder) " folder "\n"))
                                         (cdr (assoc 'folders data)))
                                        (mapc
                                         (lambda (object)
                                           (insert " "
                                                   (cdr (assoc 'id object))
                                                   " "
                                                   (cdr (assoc 'name (cdr (assoc 'describe object))))
                                                   "\n"))
                                         (cdr (assoc 'objects data)))))
                                    (switch-to-buffer buf)))
                                `(,project))))

(provide 'dx)
;;; dx.el ends here
