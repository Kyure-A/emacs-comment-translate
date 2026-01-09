;;; comment-translate-docstring.el --- Docstring dispatch -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Docstring dispatch and registration for comment-translate.
;;
;;; Code:

(require 'cl-lib)

(defcustom comment-translate-include-docstrings t
  "When non-nil, translate docstrings on hover."
  :type 'boolean
  :group 'comment-translate)

(defvar comment-translate--docstring-detectors nil
  "List of (MODES . FN) docstring detectors.")

(defun comment-translate-docstring-register (modes fn)
  "Register docstring detector FN for MODES.

FN is called with POS and should return a plist:
  (:bounds (START . END) :text STRING)

MODES is a list of major modes passed to `derived-mode-p'."
  (when (and (listp modes) (functionp fn))
    (push (cons modes fn) comment-translate--docstring-detectors)))

(defun comment-translate-docstring-at (pos)
  "Return docstring info plist at POS, or nil."
  (when comment-translate-include-docstrings
    (cl-loop for (modes . fn) in comment-translate--docstring-detectors
             when (and modes (apply #'derived-mode-p modes))
             for result = (funcall fn pos)
             when result return result)))

(provide 'comment-translate-docstring)

;;; comment-translate-docstring.el ends here
