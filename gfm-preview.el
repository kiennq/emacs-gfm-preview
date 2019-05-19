;;; gfm-preview.el --- Markdown preview using Github API  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: Kien Nguyen <kien.n.quang@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Package-Requires: ((emacs "26.1") (request) (emacs-aio))

;;; Commentary:

;; 

;;; Code:

(require 'request)
(require 'emacs-aio)

(provide 'gfm-preview)

(defvar github-url "https://api.github.com"
  "Github API url.")

(defun gfm-preview--get-preview (text callback &optional context)
  "TEXT CALLBACK CONTEXT."
  (request (concat github-url "/markdown")
           :type "POST"
           :data (encode-coding-string (json-encode `((text . ,text)
                                                      (mode . "gfm")
                                                      (context . ,context)))
                                       'utf-8 'nocopy)
           :headers `(("content-type" . "application/json"))
           :parser 'buffer-string
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (if callback (funcall callback data))))
           :error
           (cl-function (lambda (&rest _ &key error-thrown &allow-other-keys)
                          (message "Got error: %S" error-thrown)))
           :complete (lambda (&rest _) (message "Finished!"))))

(defun gfm-preview-bufer ()
  (interactive)
  (gfm-preview--get-preview (buffer-string)
                            (lambda (data)
                              (save-excursion
                                (with-current-buffer (get-buffer-create "*gfm-preview*")
                                  (erase-buffer)
                                  (insert data)
                                  (browse-url-of-buffer))))))
;;; gfm-preview.el ends here
