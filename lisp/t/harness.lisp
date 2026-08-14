;;;; harness.lisp - tiny self-test harness

(in-package #:manna-cadet.test)

(defvar *tests* '()
  "Registered tests as (name . thunk).")

(defmacro deftest (name &body body)
  `(push (cons ',name (lambda () ,@body)) *tests*))

(defun run-all ()
  "Run all registered tests. Return T if every test passes, otherwise NIL."
  (let ((failed '()))
    (dolist (entry *tests*)
      (destructuring-bind (name . thunk) entry
        (handler-case
            (unless (funcall thunk)
              (push name failed)
              (format *error-output* "FAIL: ~A~%" name))
          (error (condition)
            (push name failed)
            (format *error-output* "ERROR: ~A - ~A~%" name condition)))))
    (when failed
      (format *error-output* "~D test(s) failed.~%" (length failed)))
    (null failed)))

(deftest core-package-exists
  (find-package '#:manna-cadet))

(deftest realization-package-exists
  (find-package '#:manna-cadet.realization))

(deftest cli-package-exists
  (find-package '#:manna-cadet.cli))

(deftest cli-main-function-exists
  (fboundp (find-symbol "MAIN" '#:manna-cadet.cli)))
