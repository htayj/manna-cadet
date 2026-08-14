;;;; read.lisp - constrained data reader for abstract keyboard layouts

(in-package #:manna-cadet)

(defun unsafe-text-code-point-p (code)
  "Return true for terminal controls, line separators, and Unicode format
characters that can alter or conceal rendered text."
  (or (<= #x00 code #x1F)
      (<= #x7F code #x9F)
      (= code #x061C)
      (<= #x200B code #x200F)
      (<= #x2028 code #x202E)
      (<= #x2060 code #x2069)
      (= code #xFEFF)))

(define-condition layout-read-error (error)
  ((message :initarg :message :reader layout-read-error-message)
   (context :initarg :context :initform nil :reader layout-read-error-context))
  (:report
   (lambda (condition stream)
      (format stream "Layout read error: ~A"
              (layout-read-error-message condition)))))

(defun read-error (message &optional context)
  (error 'layout-read-error :message message :context context))

(defconstant +layout-reader-max-input-length+ (* 1024 1024))
(defconstant +layout-reader-max-token-length+ 65536)
(defconstant +layout-reader-max-depth+ 1024)
(defconstant +layout-reader-max-node-count+ 200000)

(defstruct (layout-reader-state
            (:constructor make-layout-reader-state (stream)))
  stream
  (lookahead nil)
  (lookahead-p nil)
  (position 0 :type integer)
  (nodes 0 :type integer))

(defun reader-context (state)
  (format nil "character ~D" (layout-reader-state-position state)))

(defun layout-reader-raw-character (state)
  (let ((character (read-char (layout-reader-state-stream state) nil nil)))
    (when character
      (incf (layout-reader-state-position state))
      (when (> (layout-reader-state-position state)
               +layout-reader-max-input-length+)
        (read-error "Input exceeds the textual reader safety limit"
                    (reader-context state))))
    character))

(defun layout-reader-peek-character (state)
  (unless (layout-reader-state-lookahead-p state)
    (setf (layout-reader-state-lookahead state)
          (layout-reader-raw-character state)
          (layout-reader-state-lookahead-p state) t))
  (layout-reader-state-lookahead state))

(defun layout-reader-next-character (state)
  (if (layout-reader-state-lookahead-p state)
      (prog1 (layout-reader-state-lookahead state)
        (setf (layout-reader-state-lookahead state) nil
              (layout-reader-state-lookahead-p state) nil))
      (layout-reader-raw-character state)))

(defun layout-whitespace-p (character)
  (and character
       (member character '(#\Space #\Tab #\Newline #\Return #\Page)
               :test #'char=)))

(defun skip-layout-space-and-comments (state)
  (loop
    (loop while (layout-whitespace-p (layout-reader-peek-character state))
          do (layout-reader-next-character state))
    (unless (and (layout-reader-peek-character state)
                 (char= #\; (layout-reader-peek-character state)))
      (return))
    (loop for character = (layout-reader-next-character state)
          until (or (null character) (char= character #\Newline)))))

(defun note-layout-node (state)
  (incf (layout-reader-state-nodes state))
  (when (> (layout-reader-state-nodes state) +layout-reader-max-node-count+)
    (read-error "Input exceeds the textual reader node safety limit"
                (reader-context state))))

(defun parse-layout-hex-escape (state kind)
  (let ((digits '()))
    (loop for character = (layout-reader-next-character state)
          do (cond
               ((null character)
                (read-error (format nil "Malformed hex escape in ~A" kind)
                            (reader-context state)))
               ((char= character #\;)
                (unless digits
                  (read-error (format nil "Malformed hex escape in ~A" kind)
                              (reader-context state)))
                (let* ((code (parse-integer (coerce (nreverse digits) 'string)
                                            :radix 16))
                       (character (and (<= code #x10FFFF)
                                       (not (<= #xD800 code #xDFFF))
                                       (code-char code))))
                  (unless character
                    (read-error (format nil "Invalid code point escape in ~A" kind)
                                (reader-context state)))
                  (return character)))
               ((and (< (length digits) 6) (digit-char-p character 16))
                (push character digits))
               (t
                (read-error (format nil "Malformed hex escape in ~A" kind)
                            (reader-context state)))))))

(defun parse-layout-delimited-token (state terminator valid-escapes kind)
  (layout-reader-next-character state)
  (let ((characters '())
        (length 0))
    (loop for character = (layout-reader-next-character state)
          do (when (null character)
               (read-error (format nil "Unterminated ~A" kind)
                           (reader-context state)))
             (cond
               ((char= character terminator)
                (return (coerce (nreverse characters) 'string)))
                ((char= character #\\)
                 (let ((escaped (layout-reader-next-character state)))
                   (cond
                     ((and escaped (char= escaped #\x))
                      (push (parse-layout-hex-escape state kind) characters))
                     ((and escaped (member escaped valid-escapes :test #'char=))
                      (push escaped characters))
                     (t
                      (read-error (format nil "Malformed escape in ~A" kind)
                                  (reader-context state))))
                   (incf length)))
               (t
                (push character characters)
                (incf length)))
             (when (> length +layout-reader-max-token-length+)
               (read-error "Token exceeds the textual reader safety limit"
                           (reader-context state))))))

(defun bare-token-delimiter-p (character)
  (or (null character)
      (layout-whitespace-p character)
      (member character '(#\( #\) #\;) :test #'char=)))

(defun decimal-integer-token-p (token)
  (let ((start (if (and (> (length token) 1)
                        (member (char token 0) '(#\+ #\-) :test #'char=))
                   1
                   0)))
    (and (> (length token) start)
         (loop for index from start below (length token)
               always (digit-char-p (char token index) 10)))))

(defun parse-layout-bare-token (state)
  (let ((characters '())
        (length 0))
    (loop for character = (layout-reader-peek-character state)
          until (bare-token-delimiter-p character)
          do (when (member character '(#\# #\' #\` #\, #\: #\| #\\ #\")
                           :test #'char=)
               (read-error "Unsupported reader syntax in bare identifier"
                           (reader-context state)))
             (push (layout-reader-next-character state) characters)
             (incf length)
             (when (> length +layout-reader-max-token-length+)
               (read-error "Token exceeds the textual reader safety limit"
                           (reader-context state))))
    (let ((token (coerce (nreverse characters) 'string)))
      (when (string= token ".")
        (read-error "Dotted and improper lists are not allowed"
                    (reader-context state)))
      (if (decimal-integer-token-p token)
          (parse-integer token :radix 10)
          (make-symbol token)))))

(defun parse-layout-value (state depth)
  (skip-layout-space-and-comments state)
  (let ((character (layout-reader-peek-character state)))
    (unless character
      (read-error "Unexpected end of input" (reader-context state)))
    (note-layout-node state)
    (cond
      ((char= character #\()
       (when (>= depth +layout-reader-max-depth+)
         (read-error "Input exceeds the textual reader nesting safety limit"
                     (reader-context state)))
       (layout-reader-next-character state)
       (let ((values '()))
         (loop
           (skip-layout-space-and-comments state)
           (let ((next (layout-reader-peek-character state)))
             (cond
               ((null next)
                (read-error "Unterminated list" (reader-context state)))
               ((char= next #\))
                (layout-reader-next-character state)
                (return (nreverse values)))
               (t
                (push (parse-layout-value state (1+ depth)) values)))))))
      ((char= character #\))
       (read-error "Unexpected closing parenthesis" (reader-context state)))
      ((char= character #\")
       (parse-layout-delimited-token state #\" '(#\" #\\) "string"))
      ((char= character #\|)
       (make-symbol
        (parse-layout-delimited-token state #\| '(#\| #\\) "identifier")))
      ((member character '(#\# #\' #\` #\,) :test #'char=)
       (read-error "Unsupported Common Lisp reader syntax" (reader-context state)))
      (t
       (parse-layout-bare-token state)))))

(defun read-layout-form (stream)
  (let ((state (make-layout-reader-state stream)))
    (skip-layout-space-and-comments state)
    (unless (layout-reader-peek-character state)
      (read-error "Input is empty"))
    (let ((form (parse-layout-value state 0)))
      (skip-layout-space-and-comments state)
      (when (layout-reader-peek-character state)
        (read-error "Input contains more than one top-level form"
                    (reader-context state)))
      (unless (listp form)
        (read-error "The top-level form must be a proper list"))
      form)))
