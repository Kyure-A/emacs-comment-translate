;;; comment-translate-docstring-common-lisp.el --- Common Lisp docstring detection -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Common Lisp docstring detection utilities for comment-translate.
;;
;;; Code:

(require 'cl-lib)
(require 'comment-translate-detect)
(require 'comment-translate-docstring)

(defcustom comment-translate-docstring-common-lisp-modes
  '(lisp-mode common-lisp-mode)
  "Major modes where Common Lisp docstrings should be detected."
  :type '(repeat symbol)
  :group 'comment-translate)

(defcustom comment-translate-docstring-common-lisp-excluded-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "Modes excluded from Common Lisp docstring detection."
  :type '(repeat symbol)
  :group 'comment-translate)

(defconst comment-translate--common-lisp-docstring-forms
  '(("defun" . (4))
    ("defmacro" . (4))
    ("defgeneric" . (4))
    ("defmethod" . (4 5 6))
    ("defvar" . (3 4))
    ("defparameter" . (3 4))
    ("defconstant" . (3 4)))
  "Alist of Common Lisp forms and their docstring element indices.")

(defun comment-translate--common-lisp-mode-p ()
  "Return non-nil if Common Lisp docstring detection is enabled in this buffer." 
  (and comment-translate-include-docstrings
       (apply #'derived-mode-p comment-translate-docstring-common-lisp-modes)
       (not (apply #'derived-mode-p comment-translate-docstring-common-lisp-excluded-modes))))

(defun comment-translate--common-lisp-head-symbol (list-start)
  "Return the head symbol name of list at LIST-START, or nil." 
  (save-excursion
    (goto-char list-start)
    (forward-char 1)
    (skip-chars-forward " \t\n")
    (let ((sym (symbol-at-point)))
      (when sym
        (let* ((name (symbol-name sym))
               (pos (string-match "[^:]*$" name)))
          (downcase (if pos (substring name pos) name)))))))

(defun comment-translate--common-lisp-element-starts (list-start)
  "Return a list of element start positions for list at LIST-START." 
  (save-excursion
    (goto-char list-start)
    (forward-char 1)
    (let (starts done)
      (while (and (not done) (not (eobp)))
        (skip-chars-forward " \t\n")
        (when (or (eobp) (eq (char-after) ?\)))
          (setq done t))
        (unless done
          (let ((start (point)))
            (push start starts)
            (condition-case nil
                (forward-sexp 1)
              (error (setq done t))))))
      (nreverse starts))))

(defun comment-translate--common-lisp-docstring-indexes (head)
  "Return allowed docstring indexes for HEAD symbol name." 
  (cdr (assoc head comment-translate--common-lisp-docstring-forms)))

(defun comment-translate--common-lisp-string-docstring-p (string-start)
  "Return non-nil if STRING-START is in a docstring position." 
  (condition-case nil
      (save-excursion
        (goto-char string-start)
        (backward-up-list 1)
        (let* ((list-start (point))
               (head (comment-translate--common-lisp-head-symbol list-start))
               (starts (comment-translate--common-lisp-element-starts list-start))
               (index (cl-position string-start starts)))
          (when (and head index)
            (let ((allowed (comment-translate--common-lisp-docstring-indexes head)))
              (and allowed (memq (1+ index) allowed))))))
    (error nil)))

(defun comment-translate--common-lisp-docstring-bounds (pos)
  "Return (START . END) for Common Lisp docstring at POS, or nil." 
  (when (comment-translate--common-lisp-mode-p)
    (save-excursion
      (goto-char pos)
      (let ((ppss (syntax-ppss)))
        (when (nth 3 ppss)
          (let* ((start (nth 8 ppss))
                 (end (ignore-errors (scan-sexps start 1))))
            (when (and end (comment-translate--common-lisp-string-docstring-p start))
              (cons start end))))))))

(defun comment-translate--common-lisp-docstring-text (bounds)
  "Extract Common Lisp docstring text from BOUNDS." 
  (save-excursion
    (goto-char (car bounds))
    (let ((obj (ignore-errors (read (current-buffer)))))
      (comment-translate--normalize-text
       (cond
        ((stringp obj) obj)
        (t (buffer-substring-no-properties (car bounds) (cdr bounds))))))))

(defun comment-translate--common-lisp-docstring-at (pos)
  "Return docstring info plist at POS for Common Lisp." 
  (let ((bounds (comment-translate--common-lisp-docstring-bounds pos)))
    (when bounds
      (list :bounds bounds
            :text (comment-translate--common-lisp-docstring-text bounds)))))

(comment-translate-docstring-register
 comment-translate-docstring-common-lisp-modes
 #'comment-translate--common-lisp-docstring-at)

(provide 'comment-translate-docstring-common-lisp)

;;; comment-translate-docstring-common-lisp.el ends here
