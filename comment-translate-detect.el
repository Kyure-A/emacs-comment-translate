;;; comment-translate-detect.el --- Comment detection utilities -*- lexical-binding: t; -*-

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Comment detection utilities for comment-translate.
;;
;;; Code:

(require 'subr-x)
(require 'newcomment)

(defcustom comment-translate-max-chars 800
  "Maximum number of characters to translate from a comment/docstring."
  :type 'integer
  :group 'comment-translate)

(defun comment-translate--strip-prefix (line prefix)
  "Remove PREFIX from LINE if present.

This removes leading whitespace before PREFIX, then trims whitespace after it."
  (let ((prefix (string-trim prefix)))
    (if (string-empty-p prefix)
        line
      (let ((trimmed (string-trim-left line)))
        (if (string-prefix-p prefix trimmed)
            (string-trim-left (substring trimmed (length prefix)))
          line)))))

(defun comment-translate--strip-suffix (line suffix)
  "Remove SUFFIX from LINE if present.

This trims trailing whitespace before matching SUFFIX, then trims it again."
  (let ((suffix (string-trim suffix)))
    (if (string-empty-p suffix)
        line
      (let ((trimmed (string-trim-right line)))
        (if (string-suffix-p suffix trimmed)
            (string-trim-right (substring trimmed 0 (- (length suffix))))
          line)))))

(defun comment-translate--strip-comment (text)
  "Strip comment delimiters from TEXT."
  (comment-normalize-vars)
  (let* ((lines (split-string text "\n"))
         (start (or comment-start ""))
         (cont (or comment-continue ""))
         (end (or comment-end "")))
    (setq lines (mapcar (lambda (line)
                          (comment-translate--strip-prefix line start))
                        lines))
    (when (not (string-empty-p cont))
      (setq lines (mapcar (lambda (line)
                            (comment-translate--strip-prefix line cont))
                          lines)))
    (when (and lines (not (string-empty-p end)))
      (setf (car (last lines))
            (comment-translate--strip-suffix (car (last lines)) end)))
    (string-trim (string-join lines "\n"))))

(defun comment-translate--comment-bounds (pos)
  "Return (START . END) for the comment at POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((ppss (syntax-ppss)))
      (when (nth 4 ppss)
        (let ((start (nth 8 ppss)))
          (goto-char start)
          (comment-forward 1)
          (cons start (point)))))))

(defun comment-translate--normalize-text (text)
  "Trim TEXT and apply `comment-translate-max-chars`."
  (let ((trimmed (string-trim text)))
    (if (> (length trimmed) comment-translate-max-chars)
        (substring trimmed 0 comment-translate-max-chars)
      trimmed)))

(defun comment-translate--comment-text (bounds)
  "Extract and clean comment text from BOUNDS."
  (let* ((raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (text (comment-translate--strip-comment raw)))
    (comment-translate--normalize-text text)))

(provide 'comment-translate-detect)

;;; comment-translate-detect.el ends here
