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
(defvar source-path "~/")

(defun set-source-path (path)
  (interactive "DSrc path:")
  (setq source-path path))

(defun cmd-grep-find (path str)
  (grep-find (concat "find " path " -type f -exec grep -nH  " str " {} +")))

(defun find-source ()
  (interactive)
  (setq find-str
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (cmd-grep-find source-path (prin1-to-string find-str)))

(defun print-source-path ()
  (interactive)
  (message source-path))

(define-minor-mode analyser-log-mode 
  "Analyser log functionality"
  :lighter " alog"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'find-source)
	    (define-key map (kbd "C-c s") 'set-source-path)
            map)
  (call-interactively 'set-source-path))

