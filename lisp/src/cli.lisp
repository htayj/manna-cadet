;;;; cli.lisp - noninteractive command-line interface

(in-package #:manna-cadet.cli)

(defun usage (stream)
  (format stream "manna-cadet - abstract keyboard-layout frontend~%~%")
  (format stream "Usage: manna-cadet COMMAND PATH~%~%")
  (format stream "Commands:~%")
  (format stream "  check PATH      read and validate a layout~%")
  (format stream "  normalize PATH  write canonical layout DSL~%")
  (format stream "  inspect PATH    write abstract layout inspection~%")
  (format stream "  --help, -h      show this help~%"))

(defun write-diagnostic (diagnostic stream)
  (format stream "~(~A~): ~(~A~): ~A~%"
          (manna-cadet:diagnostic-severity diagnostic)
          (manna-cadet:diagnostic-code diagnostic)
           (manna-cadet:diagnostic-message diagnostic)))

(defun safe-error-summary (condition)
  (let ((text (princ-to-string condition))
        (maximum-length 1024))
    (with-output-to-string (stream)
      (loop for character across text
            for index below maximum-length
            for code = (char-code character)
            do (if (manna-cadet:unsafe-text-code-point-p code)
                   (format stream "\\x~X;" code)
                   (write-char character stream)))
      (when (> (length text) maximum-length)
        (write-string "..." stream)))))

(defun write-command-error (condition stream)
  (format stream "error: ~A~%" (safe-error-summary condition)))

(defun run-layout-command (command path output error-output)
  (handler-case
      (with-open-file (stream path :direction :input)
        (let ((layout (manna-cadet:read-layout stream)))
          (multiple-value-bind (diagnostics valid-p)
              (manna-cadet:validate-layout layout)
            (dolist (diagnostic diagnostics)
              (write-diagnostic diagnostic error-output))
            (if valid-p
                (progn
                  (ecase command
                    (:check nil)
                    (:normalize (manna-cadet:normalize-layout layout output))
                    (:inspect (manna-cadet:inspect-layout layout output)))
                  0)
                1))))
    (manna-cadet:layout-read-error (condition)
      (write-command-error condition error-output)
      1)
    (manna-cadet:layout-parse-error (condition)
      (write-command-error condition error-output)
      1)
    (manna-cadet:inspection-limit-exceeded (condition)
      (write-command-error condition error-output)
      1)
    (file-error (condition)
      (write-command-error condition error-output)
      1)
    (error (condition)
      (write-command-error condition error-output)
      1)))

(defun run-command (arguments &key (output *standard-output*)
                                 (error-output *error-output*))
  "Run a CLI command from ARGUMENTS and return its exit status."
  (let ((command (first arguments)))
    (cond
      ((or (null arguments)
           (string= command "--help")
           (string= command "-h"))
        (usage output)
        0)
      ((or (/= (length arguments) 2)
           (not (member command '("check" "normalize" "inspect")
                        :test #'string=)))
       (format error-output "error: expected COMMAND PATH.~%")
       (usage error-output)
       2)
      (t
       (run-layout-command (intern (string-upcase command) :keyword)
                           (second arguments) output error-output)))))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Entry point for the CLI wrapper. Returns an exit status integer."
  (run-command arguments))
