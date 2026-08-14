;;;; cli-tests.lisp - command-line behavior tests

(in-package #:manna-cadet.test)

(defun run-cli (arguments)
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (let ((status (manna-cadet.cli:run-command
                   arguments :output output :error-output errors)))
      (list status (get-output-stream-string output)
            (get-output-stream-string errors)))))

(defun cli-fixture-path (name)
  (asdf:system-relative-pathname :manna-cadet/test
                                 (format nil "t/fixtures/~A" name)))

(deftest cli-shows-help-and-rejects-incomplete-commands
  (destructuring-bind (help-status help-output help-errors) (run-cli '())
    (destructuring-bind (usage-status usage-output usage-errors)
        (run-cli '("check"))
      (and (= 0 help-status)
           (search "Usage: manna-cadet COMMAND PATH" help-output)
           (string= "" help-errors)
           (= 2 usage-status)
           (string= "" usage-output)
           (search "error: expected COMMAND PATH." usage-errors)))))

(deftest cli-checks-valid-model-without-diagnostics
  (equal '(0 "" "")
         (run-cli (list "check" (namestring (manna-model-path))))))

(deftest cli-reports-invalid-fixture-deterministically
  (equal (list 1 ""
               (format nil "error: unknown-binding-layer: Binding references an undeclared layer~%"))
         (run-cli (list "check"
                        (namestring (cli-fixture-path "invalid-layout.layout"))))))

(deftest cli-normalizes-and-inspects-valid-model
  (destructuring-bind (normalize-status normalized normalize-errors)
      (run-cli (list "normalize" (namestring (manna-model-path))))
    (destructuring-bind (inspect-status inspection inspect-errors)
        (run-cli (list "inspect" (namestring (manna-model-path))))
      (and (= 0 normalize-status)
           (string= "" normalize-errors)
           (search "(layout |manna-cadet|" normalized)
           (= 0 inspect-status)
           (string= "" inspect-errors)
            (search "LAYOUT |manna-cadet|" inspection)))))

(deftest cli-wrapper-normalize-is-quiet-on-cold-cache
  (let* ((repo-root (uiop:pathname-parent-directory-pathname
                     (asdf:system-source-directory :manna-cadet)))
         (wrapper (namestring (merge-pathnames #p"bin/manna-cadet" repo-root)))
         (model (namestring (merge-pathnames #p"model/manna-cadet.lisp" repo-root)))
         (fresh-home (string-trim '(#\Newline)
                                  (uiop:run-program '("mktemp" "-d")
                                                    :output :string))))
    (unwind-protect
         (let ((env (list (format nil "HOME=~A" fresh-home)
                          (format nil "XDG_CACHE_HOME=~A/.cache" fresh-home))))
           (multiple-value-bind (output error-output status)
               (uiop:run-program (list wrapper "normalize" model)
                                 :output :string
                                 :error-output :string
                                 :ignore-error-status t
                                 :environment env)
             (and (= 0 status)
                  (string= "" error-output)
                  (char= #\( (char output 0))
                  (search "(layout |manna-cadet|" output)
                  (not (search "; compiling file" output)))))
      (uiop:run-program (list "rm" "-rf" fresh-home) :ignore-error-status t))))

(defun call-with-control-layout-file (source-function thunk)
  (let ((path (merge-pathnames
               (format nil "manna-cadet-control-~A.layout" (gensym))
               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (write-string (funcall source-function) stream))
           (funcall thunk path))
      (when (probe-file path) (delete-file path)))))

(deftest cli-rejects-control-display-with-zero-stdout-and-safe-stderr
  (call-with-control-layout-file
   (lambda ()
     (format nil
             "(layout display (levels (base (selectors))) (layers (root root)) (symbols (s \"before~Cafter\")))"
             (code-char #x1B)))
   (lambda (path)
     (destructuring-bind (status output errors)
         (run-cli (list "normalize" (namestring path)))
       (and (= 1 status)
            (string= "" output)
            (search "invalid-display-string" errors)
            (not (find (code-char #x1B) errors)))))))

(deftest cli-parse-errors-escape-control-identifiers-on-one-line
  (call-with-control-layout-file
   (lambda ()
     (format nil
             "(layout bad (keys (key k ((root base) (symbol |bad~Cid| extra)))))"
             (code-char #x1B)))
   (lambda (path)
     (destructuring-bind (status output errors)
         (run-cli (list "check" (namestring path)))
       (and (= 1 status)
            (string= "" output)
            (search "SYMBOL action requires one identifier" errors)
            (= 1 (count #\Newline errors))
            (not (find (code-char #x1B) errors)))))))

(deftest cli-rejects-oversized-inspection-before-stdout
  (destructuring-bind (status output errors)
      (run-cli (list "inspect"
                     (namestring (cli-fixture-path "inspection-limit.layout"))))
    (and (= 1 status)
         (string= "" output)
         (search "inspection limit exceeded" (string-downcase errors))
         (< (length errors) 256))))
