;;;; manna-cadet.asd - zero-external-dependency ASDF systems

(asdf:defsystem "manna-cadet"
  :description "Pure abstract keyboard-layout frontend."
  :author "Tay"
  :license "MIT"
  :version "0.0.1"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")
                 (:file "ir")
                 (:file "read")
                  (:file "parse")
                   (:file "validate")
                   (:file "normalize")
                   (:file "realization"))
  :in-order-to ((test-op (test-op "manna-cadet/test"))))

(asdf:defsystem "manna-cadet/cli"
  :description "CLI entry point for manna-cadet."
  :author "Tay"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("manna-cadet")
  :pathname "src"
  :components ((:file "cli")))

(asdf:defsystem "manna-cadet/test"
  :description "Tiny self-test harness for manna-cadet."
  :author "Tay"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("manna-cadet" "manna-cadet/cli")
  :pathname "t"
  :serial t
  :components ((:file "harness")
                 (:file "ir-tests")
                  (:file "read-tests")
                  (:file "parse-tests")
                   (:file "validate-tests")
                    (:file "model-tests")
                    (:file "normalize-tests")
                    (:file "realization-tests")
                     (:file "cli-tests")
                     (:static-file "fixtures/large-layout.layout")
                     (:static-file "fixtures/inspection-limit.layout")
                     (:static-file "fixtures/invalid-layout.layout"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (unless (uiop:symbol-call '#:manna-cadet.test '#:run-all)
               (error "manna-cadet test suite failed"))))
