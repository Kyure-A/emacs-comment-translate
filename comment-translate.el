;;; comment-translate.el --- Translate comments via posframe -*- lexical-binding: t; -*-

;; Author: Kyure-A
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.0.0"))
;; Keywords: convenience, tools, i18n
;; URL: https://github.com/Kyure-A/emacs-comment-translate

;;; Commentary:
;;
;; Show translations for comments in a posframe when hovering (idle at point or mouse position).
;; Default backend uses Google Translate's endpoint asynchronously; override with a custom translation function.
;;
;; Minimal usage:
;;   (require 'comment-translate)
;;   (comment-translate-mode 1)
;;
;; Optional (mouse hover):
;;   (setq comment-translate-hover-source 'mouse)
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'newcomment)
(require 'posframe)
(require 'url)
(require 'url-util)
(require 'json)

(defgroup comment-translate nil
  "Translate comments and show via posframe."
  :group 'tools
  :prefix "comment-translate-")

(defcustom comment-translate-idle-delay 0.6
  "Seconds to wait before showing translation."
  :type 'number)

(defcustom comment-translate-hover-source 'point
  "Where to detect hover.

`point' uses the current point after idle.
`mouse' uses the current mouse position (GUI only)."
  :type '(choice (const :tag "Point" point)
                 (const :tag "Mouse" mouse)))

(defcustom comment-translate-source-language "auto"
  "Source language code for translation."
  :type 'string)

(defcustom comment-translate-target-language "ja"
  "Target language code for translation."
  :type 'string)

(defcustom comment-translate-max-chars 800
  "Maximum number of characters to translate from a comment."
  :type 'integer)

(defcustom comment-translate-fill-column 72
  "Column width used to wrap translated text."
  :type 'integer)

(declare-function comment-translate-google-translate "comment-translate" (text callback))

(defcustom comment-translate-translate-function #'comment-translate-google-translate
  "Function used to translate comment text.

This function can be synchronous or asynchronous:
- Sync: (fn TEXT) => return translated string.
- Async: (fn TEXT CALLBACK) and call CALLBACK with (TRANSLATION ERROR).
When nil, translation is unavailable."
  :type '(choice (const :tag "Not configured" nil) function))

(defcustom comment-translate-google-endpoint
  "https://translate.googleapis.com/translate_a/single"
  "Google Translate unofficial endpoint URL."
  :type 'string)

(defcustom comment-translate-show-loading t
  "When non-nil, show a loading message while translating."
  :type 'boolean)

(defcustom comment-translate-loading-text "Translating..."
  "Text shown while translation is in progress."
  :type 'string)

(defcustom comment-translate-include-docstrings t
  "When non-nil, translate Emacs Lisp docstrings on hover."
  :type 'boolean)

(defcustom comment-translate-docstring-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "Major modes where docstrings should be detected."
  :type '(repeat symbol))

(defcustom comment-translate-show-unavailable t
  "When non-nil, show a message if translation backend is not available."
  :type 'boolean)

(defcustom comment-translate-unavailable-text
  "comment-translate: translator not configured"
  "Text shown when translation is unavailable."
  :type 'string)

(defcustom comment-translate-poshandler
  #'posframe-poshandler-point-bottom-left-corner
  "Posframe position handler."
  :type 'function)

(defcustom comment-translate-max-width 80
  "Maximum width of the posframe in characters."
  :type 'integer)

(defcustom comment-translate-max-height 20
  "Maximum height of the posframe in lines."
  :type 'integer)

(defcustom comment-translate-posframe-parameters
  '(:internal-border-width 8)
  "Extra parameters passed to `posframe-show'."
  :type 'plist)

(defcustom comment-translate-hide-when-not-in-comment t
  "When non-nil, hide the posframe when leaving a comment/docstring (point mode only)."
  :type 'boolean)

(defconst comment-translate--posframe-buffer " *comment-translate-posframe*"
  "Posframe buffer name used by comment-translate.")

(defvar comment-translate--cache (make-hash-table :test 'equal)
  "Cache for translations keyed by (source target text).")

(defvar comment-translate--last-error nil
  "Last translation error string, if any.")

(defvar-local comment-translate--idle-timer nil
  "Idle timer used by comment-translate in the current buffer.")

(defvar-local comment-translate--last-text nil
  "Last translated comment text in the current buffer.")

(defvar-local comment-translate--last-translation nil
  "Last translated result in the current buffer.")

(defvar-local comment-translate--request-id 0
  "Monotonic id for the latest translation request in the current buffer.")

(defvar-local comment-translate--pending-text nil
  "Text currently being translated in the current buffer.")

(defvar-local comment-translate--pending-window nil
  "Window used for the current translation request.")

(defvar-local comment-translate--pending-position nil
  "Position used for the current translation request.")

(defconst comment-translate--docstring-forms
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

(defun comment-translate--posframe-available-p ()
  "Return non-nil if posframe can be used in the current frame."
  (and (featurep 'posframe) (posframe-workable-p)))

(defun comment-translate--mouse-target ()
  "Return (WINDOW . POINT) at current mouse position, or nil."
  (let* ((pos (mouse-pixel-position))
         (frame (car pos))
         (xy (cdr pos)))
    (when (and frame xy (frame-live-p frame))
      (let* ((posn (posn-at-x-y (car xy) (cdr xy) frame))
             (window (and posn (posn-window posn)))
             (point (and posn (posn-point posn))))
        (when (and (window-live-p window)
                   (number-or-marker-p point))
          (cons window point))))))

(defun comment-translate--current-target ()
  "Return (WINDOW . POINT) based on hover source, or nil."
  (pcase comment-translate-hover-source
    ('mouse
     (or (comment-translate--mouse-target)
         (cons (selected-window) (point))))
    (_ (cons (selected-window) (point)))))

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

(defun comment-translate--docstring-mode-p ()
  "Return non-nil if docstring detection is enabled in this buffer."
  (and comment-translate-include-docstrings
       comment-translate-docstring-modes
       (apply #'derived-mode-p comment-translate-docstring-modes)))

(defun comment-translate--list-head-symbol (list-start)
  "Return the head symbol of list at LIST-START, or nil."
  (save-excursion
    (goto-char list-start)
    (forward-char 1)
    (skip-chars-forward " \t\n")
    (let ((obj (ignore-errors (read (current-buffer)))))
      (and (symbolp obj) obj))))

(defun comment-translate--list-element-starts (list-start)
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

(defun comment-translate--docstring-indexes (head)
  "Return allowed docstring indexes for HEAD symbol."
  (cdr (assq head comment-translate--docstring-forms)))

(defun comment-translate--string-docstring-p (string-start)
  "Return non-nil if STRING-START is in a docstring position."
  (condition-case nil
      (save-excursion
        (goto-char string-start)
        (backward-up-list 1)
        (let* ((list-start (point))
               (head (comment-translate--list-head-symbol list-start))
               (starts (comment-translate--list-element-starts list-start))
               (index (cl-position string-start starts)))
          (when (and head index)
            (let ((allowed (comment-translate--docstring-indexes head)))
              (and allowed (memq (1+ index) allowed))))))
    (error nil)))

(defun comment-translate--docstring-bounds (pos)
  "Return (START . END) for docstring at POS, or nil."
  (when (comment-translate--docstring-mode-p)
    (save-excursion
      (goto-char pos)
      (let ((ppss (syntax-ppss)))
        (when (nth 3 ppss)
          (let* ((start (nth 8 ppss))
                 (end (ignore-errors (scan-sexps start 1))))
            (when (and end (comment-translate--string-docstring-p start))
              (cons start end))))))))

(defun comment-translate--content-at (pos)
  "Return (TYPE . BOUNDS) for comment or docstring at POS."
  (let ((comment (comment-translate--comment-bounds pos)))
    (cond
     (comment (cons 'comment comment))
     (t
      (let ((doc (comment-translate--docstring-bounds pos)))
        (when doc (cons 'docstring doc)))))))

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

(defun comment-translate--docstring-text (bounds)
  "Extract docstring text from BOUNDS."
  (save-excursion
    (goto-char (car bounds))
    (let ((obj (ignore-errors (read (current-buffer)))))
      (comment-translate--normalize-text
       (cond
        ((stringp obj) obj)
        (t (buffer-substring-no-properties (car bounds) (cdr bounds))))))))

(defun comment-translate--content-text (type bounds)
  "Return cleaned text for TYPE and BOUNDS."
  (pcase type
    ('comment (comment-translate--comment-text bounds))
    ('docstring (comment-translate--docstring-text bounds))
    (_ "")))

(defun comment-translate--cache-key (text)
  "Return cache key for TEXT."
  (list comment-translate-source-language
        comment-translate-target-language
        text))

(defun comment-translate--cache-get (text)
  "Return cached translation for TEXT, or nil."
  (gethash (comment-translate--cache-key text) comment-translate--cache))

(defun comment-translate--cache-put (text translation)
  "Store TRANSLATION for TEXT in cache."
  (puthash (comment-translate--cache-key text) translation comment-translate--cache))

(defun comment-translate--function-accepts-2-p (fn)
  "Return non-nil if FN can accept two arguments."
  (when (functionp fn)
    (let ((arity (func-arity fn)))
      (when (consp arity)
        (let ((min (car arity))
              (max (cdr arity)))
          (and (<= min 2)
               (or (eq max 'many) (>= max 2))))))))

(defun comment-translate--translate-dispatch (text callback)
  "Translate TEXT and call CALLBACK with (TRANSLATION ERROR)."
  (setq comment-translate--last-error nil)
  (let ((fn comment-translate-translate-function))
    (cond
     ((null fn)
      (setq comment-translate--last-error
            "comment-translate: translation backend not configured")
      (funcall callback nil comment-translate--last-error))
     ((comment-translate--function-accepts-2-p fn)
      (condition-case err
          (let ((result (funcall fn text callback)))
            (when (stringp result)
              (funcall callback result nil)))
        (error
         (setq comment-translate--last-error
               (format "comment-translate: %s" (error-message-string err)))
         (funcall callback nil comment-translate--last-error))))
     (t
      (condition-case err
          (let ((translation (funcall fn text)))
            (funcall callback translation nil))
        (error
         (setq comment-translate--last-error
               (format "comment-translate: %s" (error-message-string err)))
         (funcall callback nil comment-translate--last-error)))))))

(defun comment-translate--parse-google-response (body)
  "Parse Google Translate response BODY and return translation or nil."
  (condition-case nil
      (let ((json-array-type 'list)
            (json-object-type 'alist)
            (json-false nil)
            (json-null nil))
        (let* ((decoded (if (multibyte-string-p body)
                            body
                          (decode-coding-string body 'utf-8)))
               (data (json-read-from-string decoded))
               (segments (and (listp data) (listp (car data)) (car data)))
               (result ""))
          (when (listp segments)
            (dolist (seg segments)
              (when (and (listp seg) (stringp (car seg)))
                (setq result (concat result (car seg)))))
            (unless (string-empty-p result) result))))
    (error nil)))

(defun comment-translate--google-callback (status callback)
  "Handle Google Translate response and invoke CALLBACK."
  (let ((buf (current-buffer)))
    (unwind-protect
        (if (plist-get status :error)
            (let ((err (plist-get status :error)))
              (funcall callback nil
                       (format "comment-translate: request failed (%s)"
                               (if (consp err) (cdr err) err))))
          (goto-char (point-min))
          (if (not (re-search-forward "\r?\n\r?\n" nil t))
              (funcall callback nil "comment-translate: empty response")
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (translation (comment-translate--parse-google-response body)))
              (if translation
                  (funcall callback translation nil)
                (funcall callback nil
                         "comment-translate: translation parse failed")))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun comment-translate-google-translate (text callback)
  "Translate TEXT via Google Translate unofficial endpoint.
CALLBACK is called with (TRANSLATION ERROR)."
  (if (string-empty-p text)
      (funcall callback "" nil)
    (let* ((encoded (url-hexify-string text))
           (url (format "%s?client=gtx&sl=%s&tl=%s&dt=t&q=%s"
                        comment-translate-google-endpoint
                        comment-translate-source-language
                        comment-translate-target-language
                        encoded)))
      (url-retrieve url #'comment-translate--google-callback (list callback) t t))))

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

(defun comment-translate--show-translation (text translation window position)
  "Display TRANSLATION for TEXT at POSITION in WINDOW."
  (setq comment-translate--last-text text
        comment-translate--last-translation translation)
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (comment-translate--posframe-show
     (comment-translate--format-translation translation)
     window
     position)))

(defun comment-translate--handle-translation (buffer window position text request-id translation error)
  "Handle translation result and update display."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and comment-translate-mode
                 (= request-id comment-translate--request-id)
                 (equal text comment-translate--pending-text))
        (setq comment-translate--pending-text nil
              comment-translate--pending-window nil
              comment-translate--pending-position nil)
        (cond
         ((and translation (not (string-empty-p translation)))
          (comment-translate--cache-put text translation)
          (comment-translate--show-translation text translation window position))
         (comment-translate-show-unavailable
          (setq comment-translate--last-error
                (or error comment-translate--last-error))
          (let ((msg (or error
                         comment-translate--last-error
                         comment-translate-unavailable-text)))
            (when (and (window-live-p window)
                       (eq (window-buffer window) (current-buffer)))
              (comment-translate--posframe-show
               (comment-translate--format-translation msg)
               window
               position))))
         (t
          (comment-translate-hide)))))))

(defun comment-translate--request-translation (text window position)
  "Request translation for TEXT and show at POSITION in WINDOW."
  (let ((cached (comment-translate--cache-get text)))
    (if cached
        (progn
          (comment-translate--cancel-pending)
          (comment-translate--show-translation text cached window position))
      (setq comment-translate--request-id (1+ comment-translate--request-id)
            comment-translate--pending-text text
            comment-translate--pending-window window
            comment-translate--pending-position position)
      (when comment-translate-show-loading
        (comment-translate--posframe-show
         (comment-translate--format-translation comment-translate-loading-text)
         window
         position))
      (let ((buffer (current-buffer))
            (req-id comment-translate--request-id))
        (comment-translate--translate-dispatch
         text
         (lambda (translation error)
           (comment-translate--handle-translation
            buffer window position text req-id translation error)))))))

(defun comment-translate--cancel-pending ()
  "Cancel any pending translation request in the current buffer."
  (setq comment-translate--pending-text nil
        comment-translate--pending-window nil
        comment-translate--pending-position nil
        comment-translate--request-id (1+ comment-translate--request-id)))

(defun comment-translate-hide ()
  "Hide the translation posframe."
  (interactive)
  (when (featurep 'posframe)
    (posframe-hide comment-translate--posframe-buffer)))

(defun comment-translate-clear-cache ()
  "Clear the translation cache."
  (interactive)
  (clrhash comment-translate--cache)
  (setq comment-translate--last-text nil
        comment-translate--last-translation nil)
  (comment-translate--cancel-pending))

(defun comment-translate--show-at (window position)
  "Show translation for comment or docstring at POSITION in WINDOW."
  (with-selected-window window
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char position)
        (let ((content (comment-translate--content-at position)))
          (if (not content)
              (progn
                (comment-translate--cancel-pending)
                (comment-translate-hide))
            (let* ((type (car content))
                   (bounds (cdr content))
                   (text (comment-translate--content-text type bounds))
                   (pending (and comment-translate--pending-text
                                 (string= text comment-translate--pending-text))))
              (cond
               ((string-empty-p text)
                (comment-translate--cancel-pending)
                (comment-translate-hide))
               (pending
                (when comment-translate-show-loading
                  (comment-translate--posframe-show
                   (comment-translate--format-translation comment-translate-loading-text)
                   window
                   position)))
               (t
                (comment-translate--request-translation text window position))))))))))

(defun comment-translate-show ()
  "Show translation for comment or docstring at current hover location."
  (interactive)
  (let ((target (comment-translate--current-target)))
    (if (not target)
        (comment-translate-hide)
      (comment-translate--show-at (car target) (cdr target)))))

(defun comment-translate--maybe-show (buffer window point)
  "Idle timer callback to show translation."
  (pcase comment-translate-hover-source
    ('mouse
     (let ((target (comment-translate--current-target)))
       (if (not target)
           (comment-translate-hide)
         (with-current-buffer (window-buffer (car target))
           (when comment-translate-mode
             (comment-translate--show-at (car target) (cdr target)))))))
    (_
     (when (and (buffer-live-p buffer)
                (window-live-p window)
                (eq (window-buffer window) buffer))
       (with-current-buffer buffer
         (when (and comment-translate-mode
                    (eq (selected-window) window)
                    (= point (point)))
           (comment-translate--show-at window point)))))))

(defun comment-translate--cancel-timer ()
  "Cancel the current buffer's idle timer if any."
  (when (timerp comment-translate--idle-timer)
    (cancel-timer comment-translate--idle-timer)
    (setq comment-translate--idle-timer nil)))

(defun comment-translate--schedule ()
  "Schedule idle translation timer in the current buffer." 
  (comment-translate--cancel-timer)
  (setq comment-translate--idle-timer
        (run-with-idle-timer comment-translate-idle-delay nil
                             #'comment-translate--maybe-show
                             (current-buffer)
                             (selected-window)
                             (point))))

(defun comment-translate--post-command ()
  "Post-command hook for comment-translate." 
  (let ((ppss (syntax-ppss)))
    (when (and comment-translate-hide-when-not-in-comment
               (eq comment-translate-hover-source 'point)
               (not (nth 4 ppss))
               (not (and (comment-translate--docstring-mode-p)
                         (nth 3 ppss)
                         (comment-translate--docstring-bounds (point)))))
      (comment-translate--cancel-pending)
      (comment-translate-hide)))
  (comment-translate--schedule))

;;;###autoload
(define-minor-mode comment-translate-mode
  "Translate comments on hover and show them in a posframe." 
  :lighter " CTrans"
  (if comment-translate-mode
      (progn
        (add-hook 'post-command-hook #'comment-translate--post-command nil t)
        (comment-translate--schedule))
    (remove-hook 'post-command-hook #'comment-translate--post-command t)
    (comment-translate--cancel-timer)
    (comment-translate-hide)))

(defun comment-translate--maybe-enable ()
  "Enable `comment-translate-mode' in programming buffers." 
  (when (derived-mode-p 'prog-mode)
    (comment-translate-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-comment-translate-mode
  comment-translate-mode comment-translate--maybe-enable)

(provide 'comment-translate)

;;; comment-translate.el ends here
