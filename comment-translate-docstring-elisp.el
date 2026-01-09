;;; comment-translate-docstring-elisp.el --- Emacs Lisp docstring detection -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Emacs Lisp docstring detection utilities for comment-translate.
;;
;;; Code:

(require 'cl-lib)
(require 'comment-translate-detect)
(require 'comment-translate-docstring)

(defcustom comment-translate-docstring-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "Major modes where docstrings should be detected."
  :type '(repeat symbol)
  :group 'comment-translate)

(defconst comment-translate--elisp-docstring-forms
  '((defun . (4))
    (defmacro . (4))
    (defsubst . (4))
    (defalias . (4))
    (defvar . (3 4))
    (defvar-local . (3 4))
    (defconst . (3 4))
    (defcustom . (4))
    (defgroup . (4))
    (defgeneric . (4))
    (define-minor-mode . (3))
    (define-globalized-minor-mode . (5))
    (define-derived-mode . (5))
    (cl-defun . (4))
    (cl-defmacro . (4))
    (cl-defmethod . (4 5 6)))
  "Alist of forms and their docstring element indices.")

(defun comment-translate--elisp-docstring-mode-p ()
  "Return non-nil if docstring detection is enabled in this buffer."
  (and comment-translate-include-docstrings
       comment-translate-docstring-modes
       (apply #'derived-mode-p comment-translate-docstring-modes)))

(defun comment-translate--elisp-list-head-symbol (list-start)
  "Return the head symbol of list at LIST-START, or nil."
  (save-excursion
    (goto-char list-start)
    (forward-char 1)
    (skip-chars-forward " \t\n")
    (let ((obj (ignore-errors (read (current-buffer)))))
      (and (symbolp obj) obj))))

(defun comment-translate--elisp-list-element-starts (list-start)
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

(defun comment-translate--elisp-docstring-indexes (head)
  "Return allowed docstring indexes for HEAD symbol."
  (cdr (assq head comment-translate--elisp-docstring-forms)))

(defun comment-translate--elisp-string-docstring-p (string-start)
  "Return non-nil if STRING-START is in a docstring position."
  (condition-case nil
      (save-excursion
        (goto-char string-start)
        (backward-up-list 1)
        (let* ((list-start (point))
               (head (comment-translate--elisp-list-head-symbol list-start))
               (starts (comment-translate--elisp-list-element-starts list-start))
               (index (cl-position string-start starts)))
          (when (and head index)
            (let ((allowed (comment-translate--elisp-docstring-indexes head)))
              (and allowed (memq (1+ index) allowed))))))
    (error nil)))

(defun comment-translate--elisp-docstring-bounds (pos)
  "Return (START . END) for docstring at POS, or nil."
  (when (comment-translate--elisp-docstring-mode-p)
    (save-excursion
      (goto-char pos)
      (let ((ppss (syntax-ppss)))
        (when (nth 3 ppss)
          (let* ((start (nth 8 ppss))
                 (end (ignore-errors (scan-sexps start 1))))
            (when (and end (comment-translate--elisp-string-docstring-p start))
              (cons start end))))))))

(defun comment-translate--elisp-docstring-text (bounds)
  "Extract docstring text from BOUNDS."
  (save-excursion
    (goto-char (car bounds))
    (let ((obj (ignore-errors (read (current-buffer)))))
      (comment-translate--normalize-text
       (cond
        ((stringp obj) obj)
        (t (buffer-substring-no-properties (car bounds) (cdr bounds))))))))

(defun comment-translate--elisp-docstring-at (pos)
  "Return docstring info plist at POS for Emacs Lisp."
  (let ((bounds (comment-translate--elisp-docstring-bounds pos)))
    (when bounds
      (list :bounds bounds
            :text (comment-translate--elisp-docstring-text bounds)))))

(comment-translate-docstring-register
 comment-translate-docstring-modes
 #'comment-translate--elisp-docstring-at)

(provide 'comment-translate-docstring-elisp)

;;; comment-translate-docstring-elisp.el ends here
