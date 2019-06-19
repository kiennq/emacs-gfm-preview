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

;; Package-Requires: ((emacs "26.1") (request) (aio) (markdown-mode))

;;; Commentary:

;; 

;;; Code:

(require 'markdown-mode)
(require 'request)
(require 'aio)
(require 'json)

(defgroup gfm-preview nil
  "Minor mode for previewing GFM."
  :prefix "gfm-preview-"
  :group 'markdown)

(defcustom gfm-preview-github-url "https://api.github.com"
  "Github API url."
  :group 'gfm-preview
  :type 'string)

(defcustom gfm-preview-css-paths
  '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css")
  "Github markdown css paths."
  :group 'gfm-preview
  :type '(repeat string))

(aio-defun gfm-preview--get-preview (text &optional context)
  "TEXT CONTEXT."
  (let ((request-backend 'curl)
        (default-directory temporary-file-directory)
        (acallback (aio-make-callback)))
    (request (concat gfm-preview-github-url "/markdown")
             :type "POST"
             :data (encode-coding-string (json-encode `((text . ,text)
                                                        (mode . "gfm")
                                                        (context . ,context)))
                                         'utf-8 'nocopy)
             :headers `(("content-type" . "application/json"))
             :parser (lambda () (decode-coding-string (buffer-string) 'utf-8 'nocopy))
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall (car acallback) data)))
             :error
             (cl-function (lambda (&rest _ &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
             :complete (lambda (&rest _) (message "Finished!")))
    (car (aio-chain (cdr acallback)))))

;;;###autoload
(defun gfm-preview (&optional buffer-name)
  "Preview BUFFER-NAME using GFM in browser."
  (interactive)
  (aio-with-async
    (let* ((buffer-name (or buffer-name "*GFM preview*"))
           (data (aio-await (gfm-preview--get-preview (buffer-string))))
           (markdown-css-paths `(,@gfm-preview-css-paths ,@markdown-css-paths)))
      (save-excursion
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (insert data)
          (markdown-add-xhtml-header-and-footer "GFM preview")
          (browse-url-of-buffer))))))

(defun gfm-preview--init ()
  "Initialize."
  ;; better header generating
  (advice-add 'markdown-add-xhtml-header-and-footer
                :after
                #'(lambda (&rest _)
                    (goto-char (point-min))
                    (re-search-forward "<body")
                    (replace-match "<body class=\"markdown-body\"")
                    (goto-char (point-max)))
                '((name . markdown-xhtml-header-with-markdown-body-class)))
  (if (eq system-type 'windows-nt)
      (advice-add 'request--curl-command :around
                  (lambda (orig-func &rest args)
                    ;; Monkey-patch, windows curl doesnt support --compressed yet
                    (let ((r (apply orig-func args)))
                      (delete "--compressed" r)))
                  '((name . request--curl-no-compress)))))

(defun gfm-preview--exit ()
  "UnInitialize."
  (advice-remove 'markdown-add-xhtml-header-and-footer
                 'markdown-xhtml-header-with-markdown-body-class)
  (advice-remove 'request--curl-command
                 'request--curl-no-compress))

;;;###autoload
(define-minor-mode gfm-preview-mode
  "Global minor mode for previewing GFM."
  :init-value nil
  :group 'gfm-preview
  :global t
  :lighter " GFM-Preview"
  (if gfm-preview-mode
      (gfm-preview--init)
    (gfm-preview--exit)))

(provide 'gfm-preview)
;;; gfm-preview.el ends here
