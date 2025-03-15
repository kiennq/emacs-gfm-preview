;;; gfm-preview.el --- Markdown preview using Github API  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: Kien Nguyen <kien.n.quang@gmail.com>
;; Version: 1.0

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
(require 'browse-url)
(require 'url-parse)
(require 'dash)
(require 'timer)

(defgroup gfm-preview nil
  "Minor mode for previewing GFM."
  :prefix "gfm-preview-"
  :group 'markdown)

(defconst gfm-preview-assets-path
  (expand-file-name "assets/" (file-name-directory (or load-file-name (buffer-file-name))))
  "Assets path.")

(defcustom gfm-preview-github-url "https://api.github.com"
  "Github API url."
  :group 'gfm-preview
  :type 'string)

(defcustom gfm-preview-css-paths
  `(,(expand-file-name "github-markdown.min.css" gfm-preview-assets-path)
    ,(expand-file-name "primer.css" gfm-preview-assets-path))
  "Github markdown css paths."
  :group 'gfm-preview
  :type '(repeat string))

(defcustom gfm-preview-remap-languages
  '(("mermaid" . "mermaid-x"))
  "Alist to remap language tag in markdown to prevent Github render it incorrectly."
  :group 'gfm-preview
  :type '(alist :key-type string :value-type string))

(defcustom gfm-preview-prefer-pandoc t
  "Prefer to use `pandoc' if available."
  :group 'gfm-preview
  :type 'boolean)

(defcustom gfm-preview-pandoc-command "pandoc"
  "Pandoc command."
  :group 'gfm-preview
  :type 'string)

(defmacro gfm-preview--json-serialize (object)
  ""
  (if (fboundp 'json-serialize)
      `(json-serialize ,object
                       :null-object  json-null
                       :false-object json-false)
    (require 'json)
    `(json-encode object)))

(aio-defun gfm-preview--exec (&rest command)
  "Asynchronously execute command COMMAND and return its output string."
  (let ((promise (aio-promise))
        (buf (generate-new-buffer " *gfm-preview-command*")))
    (set-process-sentinel
     (apply #'start-process "gfm-preview-command" buf command)
     (lambda (proc _signal)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer buf
           (let ((data (buffer-string)))
             (aio-resolve promise
                          (-const (when (> (length data) 0) (substring data 0 -1))))))
         (kill-buffer buf))))
    (aio-await promise)))

(aio-defun gfm-preview--get-preview-web-async (text &optional context)
  "TEXT CONTEXT."
  (-let ((text (gfm-preview--preprocess text))
         (request-backend 'url-retrieve)
         (default-directory temporary-file-directory)
         ((callback . promise) (aio-make-callback)))
    (request (concat gfm-preview-github-url "/markdown")
             :type "POST"
             :data (encode-coding-string (gfm-preview--json-serialize `((text . ,text)
                                                                        (mode . "gfm")
                                                                        (context . ,context)))
                                         'utf-8 'nocopy)
             :headers `(("content-type" . "application/vnd.github+json")
                        ("Accept" . "application/vnd.github+json")
                        ("X-GitHub-Api-Version" . "2022-11-28"))
             :parser (lambda () (decode-coding-string (buffer-string) 'utf-8 'nocopy))
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall callback data)
                         (message "Finished!")))
             :error
             (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))
    (car (aio-chain promise))))

(aio-defun gfm-preview--get-preview-local-async (text)
  "Using `gfm-preview-pandoc-command' to render TEXT."
  (let* ((default-directory temporary-file-directory)
         (text (encode-coding-string text 'utf-8 'nocopy))
         (in-file (convert-standard-filename
                   (make-temp-file "gfm_" nil ".md" text))))
    (aio-await (gfm-preview--exec gfm-preview-pandoc-command
                                  "--standalone"
                                  "--quiet"
                                  "--mathjax"
                                  "-f" "gfm"
                                  "-t" "html5"
                                  (file-name-nondirectory in-file)))
    ))

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

(defun gfm-preview--preprocess (text)
  "Pre-processing TEXT."
  (with-temp-buffer
    (insert text)
    (mapc (lambda (arg)
            (-let [(lang . rep) arg]
              (goto-char (point-min))
              (while (re-search-forward (format "^```[ \t]*%s[ \t]*$" lang)
                                        nil 'noerror)
                (replace-match (format "``` %s" rep)))))
          gfm-preview-remap-languages)
    (buffer-string)))

(defun gfm-preview--use-pandoc-p ()
  "Check if `gfm-preview-pandoc-command' is available."
  (and gfm-preview-prefer-pandoc
       (executable-find gfm-preview-pandoc-command)))

;;;###autoload
(defun gfm-preview-region (beg end)
  "Preview region (BEG END) using GFM in browser."
  (interactive "r")
  (aio-with-async
    (let* ((use-pandoc-p (gfm-preview--use-pandoc-p))
           (data (if use-pandoc-p
                     (aio-await (gfm-preview--get-preview-local-async
                                 (buffer-substring-no-properties beg end)))
                   ;; use Github API
                   (aio-await (gfm-preview--get-preview-web-async
                               (buffer-substring-no-properties beg end)))))
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
