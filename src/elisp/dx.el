;;; dx.el --- DNAnexus Platform Bindings

;; Copyright (C) 2012, 2013 DNAnexus, Inc.

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

(defface dx-positive-strand
  '((((class color))
     (:foreground "color-31")))
  "Face used for reads mapped to the positive strand.")

(defface dx-negative-strand
  '((((class color))
     (:foreground "color-113")))
  "Face used for reads mapped to the negative strand.")

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

;;; dx-browse-mappings
;;; ------------------

(defvar dx-mapping-read-padding 1)

(defvar dx-browse-mappings-mode-hook nil)

;; dx-browse-mappings-mode is suitable only for specially formatted data.
(put 'dx-browse-mappings-mode 'mode-class 'special)

(defvar dx-browse-mappings-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'dx-browse-mappings-refresh)
    (define-key map "j" 'dx-browse-mappings-jump)
    (define-key map "q" 'bury-buffer)
    (define-key map "[" 'dx-browse-mappings-one-screen-left)
    (define-key map "]" 'dx-browse-mappings-one-screen-right)
    map))

(define-derived-mode dx-browse-mappings-mode fundamental-mode
  "dx Browse Mappings"
  "Mode for browsing mapped reads."
  (kill-all-local-variables)
  (use-local-map dx-browse-mappings-mode-map)
  (toggle-truncate-lines 1)
  (setq major-mode 'dx-browse-mappings-mode
        mode-name "dx Browse Mappings"
        buffer-read-only t)
  (run-mode-hooks 'dx-browse-mappings-mode-hook))

(defun dx-reverse-complement (s)
  (let ((output (make-string (length s) ? )))
    (dotimes (i (length s))
      (let ((c (aref s i)))
      (aset output
            (- (length s) 1 i)
            (cond ((char-equal ?G c) ?C)
                  ((char-equal ?C c) ?G)
                  ((char-equal ?A c) ?T)
                  ((char-equal ?T c) ?A)
                  (t c)))))
    output))

(defun dx-layout-mapping (layout-offsets start-offset read-length)
  "Maps a read starting at column START-OFFSET of length READ-LENGTH to the specified layout.

The current layout is given as a list of pairs (ROW-NUM .
FIRST-AVAILABLE-COLUMN), containing the index of the row and the
position of the first free column in that row.

Returns a pair (ROW . NEW-ROW) containing the index of the row on
which the read should appear, and t iff this is the first read to
appear on that row. LAYOUT-OFFSETS is then updated to reflect
the presence of the new read."
  (let ((candidate-dest-row nil))
    (let ((dest-row (dolist (entry layout-offsets candidate-dest-row)
                      (let ((row-index (car entry))
                            (first-available-column (cdr entry)))
                        (if (>= start-offset first-available-column)
                            (or candidate-dest-row
                                (progn
                                  (setq candidate-dest-row row-index)
                                  (setcdr entry (+ start-offset read-length dx-mapping-read-padding)))))))))
      (if dest-row
          (cons dest-row nil)
        ;; TODO: maybe get a handle to the end of the list so we don't
        ;; have to traverse it again (twice...)
        (let ((new-row (length layout-offsets)))
          (nconc layout-offsets (list (cons new-row (+ start-offset read-length dx-mapping-read-padding))))
          (cons new-row t))))))

(defun dx-browse-mappings-load-buffer (mappings-table-id chr lo select-p)
  "Renders a browse-mappings buffer."
  (dx-api-gtable-describe
   mappings-table-id
   `(:details t)
   (lambda (gtable-describe mappings-table-id chr lo select-p)
     (let ((details (cdr (assoc 'details gtable-describe))))
       ;; TODO: assert Mappings type
       (let ((original-contigset (cdadr (assoc 'original_contigset details)))
             ;; Is this ok? Perhaps we should get enough data to fill
             ;; the widest frame.
             (hi (+ lo (frame-width))))
         (dx-api-gtable-get
          mappings-table-id
          `(:limit 1000 :query (:index "gri" :parameters (:coords [,chr ,lo ,hi])) :columns ["chr" "lo" "hi" "sequence" "negative_strand"])
          (lambda (gtable-get mappings-table-id chr lo select-p)
            (let ((data (cdr (assoc 'data gtable-get)))
                  (buf (get-buffer-create (concat "*dx-genome-browse-" mappings-table-id "*"))))
              (with-current-buffer buf
                (let ((buffer-read-only nil))
                  (erase-buffer)
                  (insert chr ":" (number-to-string lo) "\n\n")
                  (insert (propertize (concat "mappings from " mappings-table-id) 'face 'underline) "\n\n")
                  ;; Don't quote layout-offsets here: since we'll nconc
                  ;; stuff to the end of it, it needs to be evaluated anew
                  ;; every time.
                  (let ((layout-offsets (list (cons 0 0))))
                    (mapc (lambda (read)
                            (let ((read-chr (aref read 0))
                                  (read-lo (aref read 1))
                                  (read-hi (aref read 2))
                                  (seq (aref read 3))
                                  (negative-strand (if (eq (aref read 4) t) t nil)))
                              (let ((seq (if negative-strand (dx-reverse-complement seq) seq))
                                    (start-offset (if (< read-lo lo) (- lo read-lo) 0))
                                    (padding (if (> read-lo lo) (- read-lo lo) 0)))
                                (let ((layout (dx-layout-mapping layout-offsets
                                                                 padding
                                                                 (- (length seq) start-offset))))
                                  (let ((destination-row (car layout))
                                        (is-new-line (cdr layout)))
                                    (goto-char (point-min))
                                    ;; Advance past header line, blank line, and section header
                                    (move-end-of-line (+ destination-row 4))
                                    (insert (make-string (- padding (current-column)) ? )
                                            (propertize (substring seq start-offset)
                                                        'face
                                                        (if negative-strand 'dx-negative-strand 'dx-positive-strand)))
                                    (and is-new-line (insert "\n")))))))
                          data))
                  (goto-char (point-min))
                  (forward-line 3)
                  (dx-browse-mappings-mode)
                  (set (make-local-variable 'dx-current-mappings-table-id) mappings-table-id)
                  (set (make-local-variable 'dx-current-chr) chr)
                  (set (make-local-variable 'dx-current-lo) lo)
                  (and select-p (switch-to-buffer buf))))))
          `(,mappings-table-id ,chr ,lo ,select-p)))))
   `(,mappings-table-id ,chr ,lo ,select-p)))

(defun dx-browse-mappings (mappings-table-id chr lo)
  (interactive "sMappings Table ID: \nschr: \nnlo: ")
  (dx-browse-mappings-load-buffer mappings-table-id chr lo t))

(defun dx-browse-mappings-refresh ()
  (interactive)
  (dx-browse-mappings-load-buffer dx-current-mappings-table-id dx-current-chr dx-current-lo nil))

(defun dx-browse-mappings-jump (chr lo)
  (interactive "schr: \nnlo: ")
  (dx-browse-mappings-load-buffer dx-current-mappings-table-id chr lo nil))

(defun dx-browse-mappings-one-screen-left ()
  (interactive)
  (let ((new-lo (+ (- dx-current-lo (window-width)) 8)))
    (dx-browse-mappings-load-buffer dx-current-mappings-table-id dx-current-chr new-lo nil)))

(defun dx-browse-mappings-one-screen-right ()
  (interactive)
  (let ((new-lo (+ dx-current-lo (window-width) -8)))
    (dx-browse-mappings-load-buffer dx-current-mappings-table-id dx-current-chr new-lo nil)))

(provide 'dx)
;;; dx.el ends here
