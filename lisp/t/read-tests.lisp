;;;; read-tests.lisp - safety tests for the constrained layout reader

(in-package #:manna-cadet.test)

(defvar *reader-sentinel* nil)

(defstruct reader-side-effect
  (value (progn (setf *reader-sentinel* t) nil)))

(defun read-layout-string (source)
  (with-input-from-string (stream source)
    (read-layout stream)))

(defun signals-condition-p (condition-type thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (condition (condition)
      (typep condition condition-type))))

(deftest reader-preserves-identifier-case
  (let* ((layout (read-layout-string
                   "(layout case-test
                      (levels (q (selectors)) (Q (selectors))))"))
         (levels (layout-levels layout)))
    (and (string= "q" (symbol-name (level-name (first levels))))
         (string= "Q" (symbol-name (level-name (second levels))))
         (not (eq (level-name (first levels))
                  (level-name (second levels)))))))

(deftest reader-rejects-read-time-evaluation-without-execution
  (let ((*reader-sentinel* nil))
    (and (signals-condition-p
          'layout-read-error
          (lambda ()
            (read-layout-string
             "(layout unsafe #.(setf manna-cadet.test::*reader-sentinel* t))")))
          (null *reader-sentinel*))))

(deftest reader-rejects-structure-syntax-without-construction
  (let ((*reader-sentinel* nil))
    (and (signals-condition-p
          'layout-read-error
          (lambda ()
            (read-layout-string
             "#S(MANNA-CADET.TEST::READER-SIDE-EFFECT)")))
         (null *reader-sentinel*))))

(deftest reader-rejects-all-dispatch-and-reader-macro-syntax
  (every (lambda (source)
           (signals-condition-p
            'layout-read-error
            (lambda () (read-layout-string source))))
         '("#1=(a . #1#)"
           "#.(setf manna-cadet.test::*reader-sentinel* t)"
           "#A()"
           "#:uninterned"
           "#\\a"
           "#(vector)"
           "'quoted"
           "`quoted"
           ",quoted")))

(deftest reader-rejects-qualified-symbols
  (signals-condition-p
   'layout-read-error
    (lambda () (read-layout-string "(layout qualified foo:bar)"))))

(deftest reader-does-not-intern-novel-identifiers
  (let ((name (symbol-name (gensym "NOVEL-LAYOUT-IDENTIFIER-"))))
    (and (null (find-symbol name :keyword))
         (string= name
                  (symbol-name
                   (layout-name
                    (read-layout-string (format nil "(layout ~A)" name)))))
         (null (find-symbol name :keyword)))))

(deftest reader-rejects-empty-extra-and-non-data-input
  (every (lambda (source)
           (signals-condition-p
            'layout-read-error
            (lambda () (read-layout-string source))))
         '(""
           "(layout first) (layout second)"
           "(layout dotted . tail)"
           "#(layout vector)"
           "42")))

(deftest reader-rejects-malformed-delimited-tokens
  (every (lambda (source)
           (signals-condition-p
            'layout-read-error
            (lambda () (read-layout-string source))))
         '("(layout |unterminated)"
           "(layout |bad\\q|)"
           "(layout strings (symbols (name \"unterminated)))"
            "(layout strings (symbols (name \"bad\\q\")))")))

(deftest reader-decodes-hex-escapes-in-strings-and-identifiers
  (let* ((layout (read-layout-string
                  "(layout |name\\x1B;value|
                     (symbols (symbol \"before\\x1B;after\")))"))
         (display (layout-symbol-display (first (layout-symbols layout)))))
    (and (char= (code-char #x1B) (char (symbol-name (layout-name layout)) 4))
         (char= (code-char #x1B) (char display 6)))))

(deftest reader-rejects-malformed-hex-escapes
  (every (lambda (source)
           (signals-condition-p 'layout-read-error
                                (lambda () (read-layout-string source))))
         '("(layout |bad\\x;|)"
           "(layout |bad\\xGG;|)"
           "(layout |bad\\x110000;|)"
           "(layout strings (symbols (name \"bad\\x1B\")))")))

(deftest reader-supports-comments-and-decimal-integers-as-data
  (let ((form (manna-cadet::read-layout-form
               (make-string-input-stream
                (format nil
                        "; leading comment~%(items 42 -7 +3) ; trailing comment~%")))))
    (and (string= "items" (symbol-name (first form)))
         (equal '(42 -7 3) (rest form)))))

(deftest reader-does-not-create-scratch-packages
  (let ((before (sort (mapcar #'package-name (list-all-packages)) #'string<)))
    (read-layout-string "(layout cleanup)")
    (equal before (sort (mapcar #'package-name (list-all-packages)) #'string<))))

(deftest reader-source-never-evaluates-or-loads-input
  (let* ((path (asdf:system-relative-pathname :manna-cadet "src/read.lisp"))
         (source (string-downcase (uiop:read-file-string path))))
    (and (null (search "(eval" source))
         (null (search "(load" source))
         (null (search "(read " source))
         (null (search "read-from-string" source)))))
