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

;; Package-Requires: ((emacs "26.1") (request) (aio) (markdown-mode) (dash))

;;; Commentary:

;; 

;;; Code:

(require 'markdown-mode)
(require 'request)
(require 'aio)
(require 'json)
(require 'browse-url)
(require 'url-parse)
(require 'dash)
(require 'timer)

(defgroup gfm-preview nil
  "Minor mode for previewing GFM."
  :prefix "gfm-preview-"
  :group 'markdown)

(defcustom gfm-preview-github-url "https://api.github.com"
  "Github API url."
  :group 'gfm-preview
  :type 'string)

(defcustom gfm-preview-css-paths
  '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"
    "https://github.githubassets.com/assets/frameworks-b003e0a30d85cc60f5920a4b6ff04123.css")
  "Github markdown css paths."
  :group 'gfm-preview
  :type '(repeat string))

(aio-defun gfm-preview--get-preview-async (text &optional context)
  "TEXT CONTEXT."
  (-let ((request-backend 'curl)
         (default-directory temporary-file-directory)
         ((callback . promise) (aio-make-callback)))
    (request (concat gfm-preview-github-url "/markdown")
             :type "POST"
             :data (json-encode `((text . ,text)
                                  (mode . "gfm")
                                  (context . ,context)))
             :headers `(("content-type" . "application/json"))
             :parser (lambda () (decode-coding-string (buffer-string) 'utf-8 'nocopy))
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall callback data)))
             :error
             (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
             :complete (lambda (&rest _) (message "Finished!")))
    (car (aio-chain promise))))

(defun gfm-preview--browse-url-function (uri &optional new-window)
  "Customized `browse-url' function that works in WSL.
URI NEW-WINDOW"
  (if (executable-find "wslpath")
      (let* ((url (url-generic-parse-url (url-unhex-string uri)))
             (type (url-type url))
             (file (decode-coding-string (url-filename url) locale-coding-system))
             (default-directory temporary-file-directory))
        (if (string= type "file")
            (call-process-shell-command
             (format "cmd.exe /c start \"$(wslpath -w %s)\"" file)
             nil 0)))
    (funcall #'browse-url-default-browser uri new-window)))

(defvar gfm-preview--buffer nil)
(defvar gfm-preview--clean-timer nil
  "Timer to clean up  `gfm-preview--buffer'.")

(defvar browse-url-temp-file-name)

(defun gfm-preview--clean-buffer ()
  "Clean temporary generated buffer."
  (when gfm-preview--buffer
    (kill-buffer gfm-preview--buffer)))

(defun gfm-preview--clean-buffer-delayed ()
  "Delayed clean temporary generated buffer.
It's debounced."
  (when gfm-preview--clean-timer
    (cancel-timer gfm-preview--clean-timer))
  (setq gfm-preview--clean-timer
        (run-with-idle-timer 10 nil #'gfm-preview--clean-buffer)))

;;;###autoload
(defun gfm-preview-region (beg end)
  "Preview region (BEG END) using GFM in browser."
  (interactive "r")
  (aio-with-async
    (let* ((data (aio-await (gfm-preview--get-preview-async (buffer-substring beg end))))
           (markdown-css-paths `(,@gfm-preview-css-paths ,@markdown-css-paths))
           (browse-url-browser-function #'gfm-preview--browse-url-function))
      (save-excursion
        (gfm-preview--clean-buffer)
        (setq gfm-preview--buffer (get-buffer-create " *GFM preview*"))
        (with-current-buffer gfm-preview--buffer
          (erase-buffer)
          (insert data)
          (markdown-add-xhtml-header-and-footer "GFM preview")
          (setq browse-url-temp-file-name
                (convert-standard-filename
                 (make-temp-file
                  (expand-file-name ".burl")
                  nil ".html")))
          (browse-url-of-buffer)
          (gfm-preview--clean-buffer-delayed))))))

;;;###autoload
(defun gfm-preview-buffer ()
  "Preview current buffer using GFM in browser."
  (interactive)
  (gfm-preview-region (point-min) (point-max)))

(defun gfm-preview--init ()
  "Initialize."
  ;; better header generating
  (advice-add 'markdown-add-xhtml-header-and-footer
                :after
                (lambda (&rest _)
                    (goto-char (point-min))
                    (re-search-forward "<body")
                    (replace-match "<body class=\"markdown-body container-lg mt-3\"")
                    (goto-char (point-max)))
                '((name . markdown-xhtml-header-with-markdown-body-class)))
  (add-hook 'kill-emacs-hook #'gfm-preview--clean-buffer))

(defun gfm-preview--exit ()
  "UnInitialize."
  (advice-remove 'markdown-add-xhtml-header-and-footer
                 'markdown-xhtml-header-with-markdown-body-class)
  (remove-hook 'kill-emacs-hook #'gfm-preview--clean-buffer))

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
