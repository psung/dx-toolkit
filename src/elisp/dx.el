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

;;; Buffer-local variables

(defvar dx-current-project-id)
(defvar dx-current-folder-name)

;;; dx-list-projects-mode
;;; ---------------------

(defvar dx-list-projects-mode-hook nil)

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
        buffer-read-only t)
  (run-mode-hooks 'dx-list-projects-mode-hook))

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

(defvar dx-browse-project-mode-hook nil)

;; dx-browse-project-mode is suitable only for specially formatted data.
(put 'dx-browse-project-mode 'mode-class 'special)

(defvar dx-browse-project-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'dx-browse-project-refresh)
    (define-key map "q" 'bury-buffer)
    (define-key map "\C-m" 'dx-browse-project-open-item)
    map))

(define-derived-mode dx-browse-project-mode fundamental-mode
  "dx Browse Project"
  "Mode for listing project contents."
  (kill-all-local-variables)
  (use-local-map dx-browse-project-mode-map)
  (setq major-mode 'dx-browse-project-mode
        mode-name "dx Browse Project"
        buffer-read-only t)
  (run-mode-hooks 'dx-browse-project-mode-hook))

(defun dx-browse-project-load-buffer (project folder select-p)
  "Renders a browse project buffer."
  (dx-api-project-list-folder
   project
   `(:folder ,folder :describe t)
   (lambda (data project folder select-p)
     (let ((buf (get-buffer-create (concat "*dx-browse-" project "*"))))
       (with-current-buffer buf
         (let ((buffer-read-only nil))
           (erase-buffer)
           (insert (propertize (concat "Contents of " project ":" folder "\n") 'face 'bold))
           ;; Show folders first, then other objects.
           (if (not (string= folder "/"))
               (insert " (folder) " (propertize ".." 'face 'link) "\n"))
           (mapc
            (lambda (subfolder)
              (let ((basename (substring subfolder (length folder))))
                (insert " (folder) " (propertize basename 'face 'link) "\n")))
            (cdr (assoc 'folders data)))
           (mapc
            (lambda (object)
              (let ((object-id (cdr (assoc 'id object)))
                    (object-name (cdr (assoc 'name (cdr (assoc 'describe object))))))
                (insert " " object-id " " object-name "\n")))
            (cdr (assoc 'objects data))))
         ;; Go to the first entry.
         (goto-char (point-min))
         (forward-line)
         (dx-browse-project-mode)
         (set (make-local-variable 'dx-current-project-id) project)
         (set (make-local-variable 'dx-current-folder-name) folder)
         (and select-p (switch-to-buffer buf)))))
   `(,project ,folder ,select-p)))

(defun dx-browse-project (&optional project folder)
  (interactive)
  (let ((project (or project dx-current-project-context))
        ;; TODO: also be able to find an existing buffer for the project, if
        ;; folder is not given.
        (folder (or folder "/")))
    (dx-browse-project-load-buffer project folder t)))

(defun dx-browse-project-refresh ()
  (interactive)
  (dx-browse-project-load-buffer dx-current-project-id dx-current-folder-name t))

(defun dx-browse-project-open-item ()
  "Opens the item whose entry is at point."
  (interactive)
  (save-excursion
    ;; TODO: I don't think this works with narrowing.
    (let (p1 p2)
      (beginning-of-line)
      ;; Expect line to look like this:
      ;;   (folder) NAME
      ;;
      ;; TODO: something meaningful with data objects too
      ;; TODO: detect when we are not on a folder or object line
      ;;
      ;; TODO: maybe higher specificity. Investigate use of "buttons".
      (setq p1 (progn
                 (search-forward "(folder) ")
                 (match-end 0)))
      (setq p2 (progn
                 (search-forward "\n")
                 (match-beginning 0)))
      (let ((folder-basename (buffer-substring p1 p2)))
        ;; Remove the link face from folder name
        (set-text-properties 0 (length folder-basename) nil folder-basename)
        (let ((real-folder-path (if (string= folder-basename "..")
                                    (progn
                                      (string-match "^\\(.*/\\).+/" dx-current-folder-name)
                                      (substring dx-current-folder-name 0 (match-end 1)))
                                    (concat dx-current-folder-name folder-basename "/"))))
          (dx-browse-project-load-buffer dx-current-project-id
                                         real-folder-path
                                         nil))))))

(provide 'dx)
;;; dx.el ends here
