;;; comment-translate-docstring-python.el --- Python docstring detection -*- lexical-binding: t; -*-

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Python docstring detection utilities for comment-translate.
;;
;;; Code:

(require 'comment-translate-detect)
(require 'comment-translate-docstring)

(defcustom comment-translate-docstring-python-modes
  '(python-mode python-ts-mode)
  "Major modes where Python docstrings should be detected."
  :type '(repeat symbol)
  :group 'comment-translate)

(defun comment-translate--python-docstring-mode-p ()
  "Return non-nil if Python docstring detection is enabled in this buffer."
  (and comment-translate-include-docstrings
       (apply #'derived-mode-p comment-translate-docstring-python-modes)))

(defun comment-translate--python-statement-start ()
  "Return point at beginning of current Python statement."
  (cond
   ((fboundp 'python-nav-beginning-of-statement)
    (python-nav-beginning-of-statement)
    (point))
   (t
    (back-to-indentation)
    (point))))

(defun comment-translate--python-string-statement-p (pos)
  "Return non-nil if statement at POS starts with a string literal." 
  (save-excursion
    (goto-char pos)
    (looking-at-p
     "[ \\t]*[uUrRbBfF]*\\(?:\\\"\\\"\\\"\\|'''\\|\\\"\\|'\\)")))

(defun comment-translate--python-module-docstring-p (stmt-start)
  "Return non-nil if statement at STMT-START is module docstring." 
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) stmt-start))
        (cond
         ((looking-at-p "^[ \t]*$")
          (forward-line 1))
         ((looking-at-p "^[ \t]*#")
          (forward-line 1))
         (t
          (setq found t))))
      (not found))))

(defun comment-translate--python-block-docstring-p (stmt-start indent)
  "Return non-nil if statement at STMT-START is first in def/class block." 
  (save-excursion
    (goto-char stmt-start)
    (let ((done nil)
          (prev-same nil)
          (header nil))
      (while (and (not done) (not (bobp)))
        (forward-line -1)
        (cond
         ((looking-at-p "^[ \t]*$") nil)
         ((looking-at-p "^[ \t]*#") nil)
         ((< (current-indentation) indent)
          (setq header (point))
          (setq done t))
         ((= (current-indentation) indent)
          (setq prev-same t)
          (setq done t))
         (t nil)))
      (and (not prev-same)
           header
           (save-excursion
             (goto-char header)
             (back-to-indentation)
             (looking-at-p "\(?:async[ \t]+\)?\(?:def\|class\)\b"))))))

(defun comment-translate--python-docstring-p (string-start)
  "Return non-nil if STRING-START is a Python docstring." 
  (save-excursion
    (goto-char string-start)
    (let* ((stmt-start (comment-translate--python-statement-start))
           (indent (current-indentation)))
      (and (comment-translate--python-string-statement-p stmt-start)
           (if (zerop indent)
               (comment-translate--python-module-docstring-p stmt-start)
             (comment-translate--python-block-docstring-p stmt-start indent))))))

(defun comment-translate--python-strip-string (raw)
  "Strip Python string delimiters and prefixes from RAW." 
  (let* ((s (string-trim-left raw))
         (s (replace-regexp-in-string
             "\`[uUrRbBfF]+" "" s))
         (quote (cond
                 ((string-prefix-p "\"\"\"" s) "\"\"\"")
                 ((string-prefix-p "'''" s) "'''")
                 ((string-prefix-p "\"" s) "\"")
                 ((string-prefix-p "'" s) "'")
                 (t nil))))
    (when quote
      (setq s (substring s (length quote)))
      (when (string-suffix-p quote s)
        (setq s (substring s 0 (- (length quote))))))
    s))

(defun comment-translate--python-docstring-text (bounds)
  "Extract Python docstring text from BOUNDS." 
  (let* ((raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (text (comment-translate--python-strip-string raw)))
    (comment-translate--normalize-text text)))

(defun comment-translate--python-docstring-at (pos)
  "Return docstring info plist at POS for Python." 
  (when (comment-translate--python-docstring-mode-p)
    (save-excursion
      (goto-char pos)
      (let ((ppss (syntax-ppss)))
        (when (nth 3 ppss)
          (let* ((start (nth 8 ppss))
                 (end (ignore-errors (scan-sexps start 1))))
            (when (and end (comment-translate--python-docstring-p start))
              (list :bounds (cons start end)
                    :text (comment-translate--python-docstring-text
                           (cons start end))))))))))

(comment-translate-docstring-register
 comment-translate-docstring-python-modes
 #'comment-translate--python-docstring-at)

(provide 'comment-translate-docstring-python)

;;; comment-translate-docstring-python.el ends here
