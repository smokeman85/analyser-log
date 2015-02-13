;;;analyser-log.el - speed changing from log output to source code
;;
;; Copyright (C) 2015 Dmitry Pichugin
;; Keywords: grep-find
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;    This module is used for fast finding log message into  source code which printed it.
;;    Key map:
;;       C-c f - find line or region of line in directory of source code
;;       C-c s - change path to directory of source code
;;; Code:
(defvar alog--source-path "~/"
  "Default path to directory of source code")

(defvar alog--find-key (kbd "C-c f")
  "Key for find")

(defvar alog--set-source-path-key (kbd "C-c s")
  "Key for set path")

(defun set-source-path (path)
  "Set path to directory of source code"
  (interactive "DSrc path:")
  (setq alog--source-path path))

(defun cmd-grep-find (path str)
  "Command grep-find"
  (grep-find (concat "find " path " -type f -exec grep -nH  " str " {} +")))

(defun find-source ()
  "Find line or region in src dir"
  (interactive)
  (setq find-str
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (cmd-grep-find alog--source-path (prin1-to-string find-str)))

(defun print-source-path ()
  (interactive)
  (message alog--source-path))

(define-minor-mode analyser-log-mode 
  "Analyser log functionality"
  :lighter " alog"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map alog--find-key 'find-source)
	    (define-key map alog--set-source-path-key 'set-source-path)
            map)
  (call-interactively 'set-source-path))

