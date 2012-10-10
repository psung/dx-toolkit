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

(defun dx-list-projects ()
  (interactive)
  (dx-api-system-find-projects `(:describe t)
                                (lambda (data)
                                  (let ((buf (get-buffer-create "*dx-list-projects*")))
                                    (with-current-buffer buf
                                      (setq buffer-read-only t)
                                      (let ((buffer-read-only nil))
                                        (erase-buffer)
                                        (mapc
                                         (lambda (project)
                                           (insert (cdr (assoc 'id project))
                                                   " "
                                                   (cdr (assoc 'name (cdr (assoc 'describe project))))
                                                   "\n"))
                                         (cdr (assoc 'results data)))))
                                    (switch-to-buffer buf)))))

(defun dx-browse-project (&optional project folder)
  (interactive)
  (let ((project (or project dx-current-project-context))
        (folder (or folder "/")))
    (dx-api-project-list-folder project
                                `(:folder ,folder :describe t)
                                (lambda (data project)
                                  (let ((buf (get-buffer-create (concat "*dx-browse-" project "*"))))
                                    (with-current-buffer buf
                                      (setq buffer-read-only t)
                                      (let ((buffer-read-only nil))
                                        (erase-buffer)
                                        (insert "Contents of " project ":\n")
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
