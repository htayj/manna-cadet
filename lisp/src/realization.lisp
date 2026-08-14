;;;; realization.lisp - abstract realization contract

(in-package #:manna-cadet.realization)

(defclass realization-profile () ()
  (:documentation
   "Abstract realization contract. A future profile owns all target-specific mapping,
mechanism, policy, and capability facts; the abstract layout never does."))

(define-condition realization-not-implemented (error)
  ((operation :initarg :operation
              :reader realization-not-implemented-operation))
  (:report
   (lambda (condition stream)
     (format stream "Realization operation ~A is not implemented"
             (realization-not-implemented-operation condition)))))

(defgeneric validate-profile (profile)
  (:documentation
   "Validate PROFILE-owned target-specific mapping, mechanism, policy, and capability facts.
The abstract layout never owns those facts."))

(defmethod validate-profile ((profile realization-profile))
  (declare (ignore profile))
  (error 'realization-not-implemented :operation 'validate-profile))

(defgeneric compile-layout (profile layout)
  (:documentation
   "Compile LAYOUT through PROFILE-owned target-specific mapping, mechanism, policy, and
capability facts. The abstract layout never owns those facts."))

(defmethod compile-layout ((profile realization-profile) layout)
  (declare (ignore profile layout))
  (error 'realization-not-implemented :operation 'compile-layout))
