;;; dx-http.el --- Base HTTP request implementation for DNAnexus Platform use

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

;;

;;; Code:

(require 'json)
(require 'url-http)

(defvar dx-apiserver-host "api.dnanexus.com")
(defvar dx-apiserver-port 443)
(defvar dx-apiserver-protocol "https")

(defvar dx-auth-token)

(defun dx-http-request (route input cb &optional cbargs)
  (let ((security-context (concat "bearer " dx-auth-token)))
    (let ((url (concat dx-apiserver-protocol "://" dx-apiserver-host ":" (number-to-string dx-apiserver-port) route))
          (url-request-method "POST")
          (url-request-extra-headers
           `(("Content-Type" . "application/json")
             ("Authorization" . ,security-context)))
          (url-request-data (or (and input (json-encode input)) "{}")))
      (url-retrieve
       url
       (lambda (status cb &rest cbargs)
         ;; Response is in the current buffer. Skip past headers and
         ;; parse the body.
         ;;
         ;; TODO: detect response code and abort if not successful
         (search-forward "\n\n")
         (narrow-to-region (point) (point-max))
         (let ((output (buffer-string)))
           (apply cb (json-read-from-string output) cbargs)))
       ;; First cbarg to url-retrieve is reserved by us for the caller's
       ;; callback. The remaining cbargs can be used by the caller to
       ;; store whatever state it needs.
       `(,cb . ,cbargs)))))

(provide 'dx-http)
;;; dx-http.el ends here
