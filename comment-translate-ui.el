;;; comment-translate-ui.el --- UI helpers for comment-translate -*- lexical-binding: t; -*-

;; Author: Kyure-A
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; UI helpers (posframe display, formatting) for comment-translate.
;;
;;; Code:

(require 'posframe)

(defcustom comment-translate-fill-column 72
  "Column width used to wrap translated text."
  :type 'integer
  :group 'comment-translate)

(defcustom comment-translate-poshandler
  #'posframe-poshandler-point-bottom-left-corner
  "Posframe position handler."
  :type 'function
  :group 'comment-translate)

(defcustom comment-translate-max-width 80
  "Maximum width of the posframe in characters."
  :type 'integer
  :group 'comment-translate)

(defcustom comment-translate-max-height 20
  "Maximum height of the posframe in lines."
  :type 'integer
  :group 'comment-translate)

(defcustom comment-translate-posframe-parameters
  '(:internal-border-width 1)
  "Extra parameters passed to `posframe-show'."
  :type 'plist
  :group 'comment-translate)

(defcustom comment-translate-show-loading t
  "When non-nil, show a loading message while translating."
  :type 'boolean
  :group 'comment-translate)

(defcustom comment-translate-loading-text "Translating..."
  "Text shown while translation is in progress."
  :type 'string
  :group 'comment-translate)

(defconst comment-translate--posframe-buffer " *comment-translate-posframe*"
  "Posframe buffer name used by comment-translate.")

(defun comment-translate--posframe-available-p ()
  "Return non-nil if posframe can be used in the current frame."
  (and (featurep 'posframe) (posframe-workable-p)))

(defun comment-translate--format-translation (text)
  "Wrap TEXT to `comment-translate-fill-column' when configured." 
  (if (and (numberp comment-translate-fill-column)
           (> comment-translate-fill-column 0))
      (with-temp-buffer
        (insert text)
        (let ((fill-column comment-translate-fill-column))
          (fill-region (point-min) (point-max)))
        (buffer-string))
    text))

(defun comment-translate--posframe-show (text window position)
  "Show TEXT in a posframe at POSITION in WINDOW." 
  (when (comment-translate--posframe-available-p)
    (with-selected-window window
      (apply #'posframe-show
             comment-translate--posframe-buffer
             :string text
             :position position
             :poshandler comment-translate-poshandler
             :background-color (face-background 'tooltip nil t)
             :foreground-color (face-foreground 'tooltip nil t)
             :max-width comment-translate-max-width
             :max-height comment-translate-max-height
             comment-translate-posframe-parameters))))

(defun comment-translate-hide ()
  "Hide the translation posframe." 
  (interactive)
  (when (featurep 'posframe)
    (posframe-hide comment-translate--posframe-buffer)))

(provide 'comment-translate-ui)

;;; comment-translate-ui.el ends here
