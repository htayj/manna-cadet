;;;; realization-tests.lisp - abstract realization contract tests

(in-package #:manna-cadet.test)

(deftest realization-contract-types-and-generics-exist
  (and (find-class 'manna-cadet.realization:realization-profile nil)
       (find-class 'manna-cadet.realization:realization-not-implemented nil)
       (fboundp 'manna-cadet.realization:validate-profile)
       (fboundp 'manna-cadet.realization:compile-layout)))

(deftest abstract-realization-operations-signal-not-implemented
  (let ((profile (make-instance 'manna-cadet.realization:realization-profile)))
    (and (signals-condition-p
          'manna-cadet.realization:realization-not-implemented
          (lambda () (manna-cadet.realization:validate-profile profile)))
         (signals-condition-p
          'manna-cadet.realization:realization-not-implemented
          (lambda () (manna-cadet.realization:compile-layout profile nil))))))
