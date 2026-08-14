;;;; normalize-tests.lisp - canonical serialization and inspection tests

(in-package #:manna-cadet.test)

(defun read-normalized-layout (layout)
  (with-input-from-string (stream (normalize-layout layout))
    (read-layout stream)))

(defun round-trip-valid-p (layout)
  (let* ((normalized (normalize-layout layout))
         (round-tripped (with-input-from-string (stream normalized)
                          (read-layout stream))))
    (multiple-value-bind (diagnostics valid-p) (validate-layout round-tripped)
      (and valid-p
           (null diagnostics)
           (layout-equal-p layout round-tripped)
           (string= normalized (normalize-layout layout))
           (string= normalized (normalize-layout round-tripped))))))

(defun exhaustive-layout-source ()
  (format nil
          "(layout CaseMix
             (dimensions (plane (default base) (states base selected)))
             (levels
               (base (selectors (plane base)))
               (selected (selectors (plane selected)) (fallback base)))
             (layers (root root) (layer fun))
             (modifiers (one two))
             (commands (Run))
             (symbols (Foo ~S) foo a b c)
             (keys
               (key a ((root base) (symbol a)))
               (key b ((root base) b))
               (key c ((root base) c))
               (key command-key ((root base) (command Run)))
               (key mods-key ((root base) (mods one two)))
               (key selector-key ((root base) (hold-selector plane selected)))
               (key layer-key ((root base) (layer-hold fun)))
               (key simultaneous-key
                 ((root base)
                  (simultaneous (mods one) (hold-selector plane selected))))
               (key tap-hold-key ((root base) (tap-hold Foo (mods two))))
               (key none-key ((root base) none))
               (key transparent-key ((fun selected) transparent)))
             (combos
               (combo (a b) ((root base) (command Run)))
               (combo (b c) ((root base) none))))"
          "quote: \" and slash: \\"))

(deftest normalize-round-trips-every-grammar-construct
  (let* ((layout (read-layout-string (exhaustive-layout-source)))
         (normalized (normalize-layout layout))
         (round-tripped (read-normalized-layout layout)))
    (and (round-trip-valid-p layout)
         (search "|CaseMix|" normalized)
         (search "|Foo|" normalized)
         (search "|foo|" normalized)
         (search "\\\"" normalized)
         (search "\\\\" normalized)
         (layout-equal-p layout round-tripped)
          (not (eq layout round-tripped)))))

(deftest normalize-round-trips-escaped-identifiers-and-displays
  (let* ((root-name (make-symbol "root|layer\\name"))
         (level-name (make-symbol "level|with\\backslash"))
         (symbol-name (make-symbol "symbol | and \\"))
         (display "quote: \" and slash: \\")
         (layout (make-layout
                  :name (make-symbol "layout | \\ name")
                  :levels (list (make-level :name level-name))
                  :layers (list (make-layer :name root-name))
                  :root-layer (make-symbol "root|layer\\name")
                  :symbols (list (make-layout-symbol :name symbol-name
                                                     :display display)))))
    (let* ((normalized (normalize-layout layout))
           (round-tripped (read-normalized-layout layout)))
      (and (search "\\|" normalized)
           (search "\\\\" normalized)
           (search "\\\"" normalized)
           (string= display
                    (layout-symbol-display
                     (first (layout-symbols round-tripped))))
            (layout-equal-p layout round-tripped)))))

(deftest normalize-escapes-unsafe-controls-and-round-trips-them
  (let* ((control-codes (append '(#x00 #x09 #x0A #x1B #x7F #x85 #x2028 #x2029)
                                (unsafe-format-code-points)))
         (display (coerce (mapcar #'code-char control-codes) 'string))
         (identifier (make-symbol
                      (concatenate 'string "name" display "end")))
         (layout (make-layout
                  :name identifier
                  :levels (list (make-level :name :base))
                  :layers (list (make-layer :name :root))
                  :root-layer :root
                  :symbols (list (make-layout-symbol :name :symbol
                                                     :display display))))
         (normalized (normalize-layout layout))
         (round-tripped (read-normalized-layout layout)))
    (and (notany (lambda (code) (find (code-char code) normalized))
                 (remove #x0A control-codes))
         (every (lambda (code) (search (format nil "\\x~X;" code) normalized))
                control-codes)
         (layout-equal-p layout round-tripped))))

(deftest normalize-stream-contract-writes-and-returns-layout
  (let ((layout (read-layout-string (exhaustive-layout-source))))
    (let ((text (with-output-to-string (stream)
                  (and (eq layout (normalize-layout layout stream))
                       (eq layout (inspect-layout layout stream))))))
      (and (search "(layout |CaseMix|" text)
            (search "LAYOUT |CaseMix|" text)))))

(deftest normalize-round-trips-twenty-level-fixture
  (let* ((path (asdf:system-relative-pathname
                :manna-cadet/test "t/fixtures/large-layout.layout"))
         (layout (with-open-file (stream path) (read-layout stream))))
    (and (= 20 (length (layout-levels layout)))
         (round-trip-valid-p layout))))

(deftest normalize-round-trips-manna-model
  (round-trip-valid-p (read-manna-model)))

(deftest normalize-serializes-root-by-exact-name-not-layer-position
  (let* ((root (make-symbol "ROOT"))
         (layout (make-layout :name :reordered
                              :levels (list (make-level :name :base))
                              :layers (list (make-layer :name :fun)
                                            (make-layer :name root)
                                            (make-layer :name :nav))
                               :root-layer (make-symbol "ROOT")))
         (normalized (normalize-layout layout))
         (round-tripped (read-normalized-layout layout)))
    (and (search "(layers (root |ROOT|) (layer |FUN|) (layer |NAV|))" normalized)
         (validates-p layout)
         (layout-equal-p layout round-tripped)
         (layout-equal-p
          (read-layout-string
           "(layout |REORDERED| (levels (|BASE| (selectors)))
              (layers (root |ROOT|) (layer |FUN|) (layer |NAV|)))")
           round-tripped)
         (< (search "CONTEXT |ROOT| (ROOT)" (inspect-layout layout))
             (search "CONTEXT |FUN| (OVER |ROOT|)" (inspect-layout layout))))))

(deftest invalid-nil-layout-name-is-not-equal-after-normalization
  (let* ((layout (make-layout
                  :name nil
                  :levels (list (make-level :name :base))
                  :layers (list (make-layer :name :root))
                  :root-layer :root))
         (round-tripped (read-normalized-layout layout)))
    (and (not (validates-p layout))
         (valid-identifier-p (layout-name round-tripped))
         (not (layout-equal-p layout round-tripped)))))

(defun inspection-test-layout ()
  (read-layout-string
   "(layout inspection
      (levels
        (base (selectors))
        (shift (selectors) (fallback base)))
      (layers (root root) (layer fun))
      (commands (launch))
      (symbols root-result fun-result)
      (keys
        (key fallback ((root base) root-result))
        (key blocked ((root base) root-result) ((fun shift) none))
        (key passthrough
          ((root base) root-result) ((fun shift) transparent))
        (key command ((root base) root-result)
                     ((fun base) (command launch)))
        (key missing))
      (combos
        (combo (fallback blocked) ((root base) root-result))))"))

(deftest inspection-shows-resolution-provenance-and-distinct-absence
  (let ((inspection (inspect-layout (inspection-test-layout))))
    (and (search "CONTEXT |root| (ROOT)" inspection)
          (search "CONTEXT |fun| (OVER |root|)" inspection)
          (search "KEY |fallback| | LEVEL |shift| | SYMBOL |root-result| | FALLBACK |root|/|base|"
                  inspection)
          (search "KEY |blocked| | LEVEL |shift| | NONE | DIRECT |fun|/|shift|" inspection)
          (search "KEY |passthrough| | LEVEL |shift| | SYMBOL |root-result| | FALLBACK |root|/|base| | AFTER TRANSPARENT |fun|/|shift|"
                  inspection)
          (search "KEY |missing| | LEVEL |base| | UNRESOLVED" inspection)
          (search "COMBO (|fallback| + |blocked|) | LEVEL |shift| | SYMBOL |root-result| | FALLBACK |root|/|base|"
                  inspection))))

(deftest inspection-shows-manna-fallback-and-fun-command-resolution
  (let ((inspection (inspect-layout (read-manna-model))))
    (and (search "KEY |q| | LEVEL |top+greek| | NONE | DIRECT |main|/|top+greek|"
                  inspection)
          (search "CONTEXT |fun| (OVER |main|)" inspection)
          (search "KEY |q| | LEVEL |normal| | COMMAND |quote| | DIRECT |fun|/|normal|"
                  inspection))))

(deftest inspection-escapes-hostile-identifiers-on-one-line
  (let* ((hostile (make-symbol (format nil "row~%|\\name")))
         (root (make-symbol "root"))
         (layout (make-layout
                  :name hostile
                  :levels (list (make-level :name hostile))
                  :layers (list (make-layer :name root))
                  :root-layer root
                  :symbols (list (make-layout-symbol :name hostile))
                  :keys (list
                         (make-abstract-key
                          :name hostile
                          :bindings
                          (list (make-binding
                                 :layer root :level hostile
                                 :action (make-symbol-action :symbol hostile)))))))
         (inspection (inspect-layout layout))
         (escaped "|row\\n\\|\\\\name|"))
    (and (= 3 (count #\Newline inspection))
         (search (format nil "LAYOUT ~A" escaped) inspection)
         (search (format nil "KEY ~A | LEVEL ~A | SYMBOL ~A"
                         escaped escaped escaped)
                 inspection))))

(deftest inspection-is-deterministic-and-abstract
  (let* ((layout (read-manna-model))
         (first (inspect-layout layout))
         (second (inspect-layout layout))
         (lower (string-downcase first)))
    (and (string= first second)
         (notany (lambda (term) (search term lower))
                 '("backend" "device" "profile" "timing" "transport"
                     "hardware" "keycode" "matrix" "coordinate")))))

(defun make-fallback-chain-inspection-layout (level-count)
  (let* ((level-names
           (loop for index below level-count
                 collect (make-symbol (format nil "level-~D" index))))
         (levels
           (loop for name in level-names
                 for fallback = nil then previous
                 for previous = name
                 collect (make-level :name name :fallback fallback)))
         (action (make-symbol-action :symbol :result))
         (key (make-abstract-key
               :name :key
               :bindings (list (make-binding :layer :root
                                              :level (first level-names)
                                              :action action)))))
    (make-layout :name :fallback-chain
                 :levels levels
                 :layers (list (make-layer :name :root))
                 :root-layer :root
                 :symbols (list (make-layout-symbol :name :result))
                 :keys (list key))))

(deftest inspection-resolves-each-owner-layer-level-at-most-once
  (let* ((layout (make-fallback-chain-inspection-layout 120))
         (maximum-computations
           (* (length (layout-keys layout))
              (length (layout-layers layout))
              (length (layout-levels layout))))
         (manna-cadet::*inspection-resolution-computation-count* 0))
    (inspect-layout layout)
    (<= manna-cadet::*inspection-resolution-computation-count*
        maximum-computations)))

(deftest inspection-two-thousand-level-fallback-chain-completes-promptly
  (let* ((layout (make-fallback-chain-inspection-layout 2000))
         (started (get-internal-real-time))
         (inspection (inspect-layout layout))
         (elapsed (/ (- (get-internal-real-time) started)
                     internal-time-units-per-second)))
    (and (< elapsed 2)
         (search "KEY |KEY| | LEVEL |level-0| | SYMBOL |RESULT| | DIRECT |ROOT|/|level-0|"
                 inspection)
         (search "KEY |KEY| | LEVEL |level-1999| | SYMBOL |RESULT| | FALLBACK |ROOT|/|level-0|"
                 inspection))))

(deftest inspection-terminates-cycles-and-keeps-non-first-root-semantics
  (let* ((action (make-symbol-action :symbol :result))
         (combo (make-combo
                 :keys '(:cycle :transparent)
                 :bindings (list (make-binding :layer :root :level :b
                                               :action action))))
         (layout
           (make-layout
            :name :programmatic-cycle
            :levels (list (make-level :name :a :fallback :b)
                          (make-level :name :b :fallback :a))
            :layers (list (make-layer :name :fun)
                          (make-layer :name :root))
            :root-layer :root
            :symbols (list (make-layout-symbol :name :result))
            :keys
            (list (make-abstract-key :name :cycle)
                  (make-abstract-key
                   :name :transparent
                   :bindings
                   (list (make-binding :layer :fun :level :a
                                       :action (make-transparent-action))
                         (make-binding :layer :root :level :b
                                       :action action))))
            :combos (list combo)))
         (inspection (inspect-layout layout)))
    (and (search "CONTEXT |ROOT| (ROOT)" inspection)
         (search "KEY |CYCLE| | LEVEL |A| | UNRESOLVED" inspection)
         (search "KEY |TRANSPARENT| | LEVEL |A| | SYMBOL |RESULT| | FALLBACK |ROOT|/|B| | AFTER TRANSPARENT |FUN|/|A|"
                 inspection)
         (search "COMBO (|CYCLE| + |TRANSPARENT|) | LEVEL |A| | SYMBOL |RESULT| | FALLBACK |ROOT|/|B|"
                 inspection))))

(defun make-inspection-byte-limit-layout ()
  (let* ((command-name (make-symbol (make-string 25000 :initial-element #\x)))
         (levels (list (make-level :name :base)))
         (layers (list (make-layer :name :root)))
         (commands (list (make-command :name command-name)))
         (keys
           (loop for index below 200
                 collect
                 (make-abstract-key
                  :name (make-symbol (format nil "key-~D" index))
                  :bindings
                  (list (make-binding
                         :layer :root :level :base
                         :action (make-command-action :command command-name)))))))
    (make-layout :name :byte-limit :levels levels :layers layers
                 :root-layer :root :commands commands :keys keys)))

(deftest inspection-rejects-row-budget-before-writing
  (let* ((path (asdf:system-relative-pathname
                :manna-cadet/test "t/fixtures/inspection-limit.layout"))
         (layout (with-open-file (stream path) (read-layout stream)))
         (output (make-string-output-stream))
         (condition
           (handler-case
               (progn (inspect-layout layout output) nil)
             (inspection-limit-exceeded (value) value))))
    (and (validates-p layout)
         condition
         (eq :rows (inspection-limit-exceeded-kind condition))
         (> (inspection-limit-exceeded-estimate condition)
            (inspection-limit-exceeded-limit condition))
         (string= "" (get-output-stream-string output)))))

(deftest inspection-rejects-estimated-byte-budget-before-writing
  (let* ((layout (make-inspection-byte-limit-layout))
         (output (make-string-output-stream))
         (condition
           (handler-case
               (progn (inspect-layout layout output) nil)
             (inspection-limit-exceeded (value) value))))
    (and (validates-p layout)
         condition
         (eq :estimated-bytes (inspection-limit-exceeded-kind condition))
         (> (inspection-limit-exceeded-estimate condition)
            (inspection-limit-exceeded-limit condition))
         (string= "" (get-output-stream-string output)))))

(deftest inspection-keeps-twenty-and-twenty-one-level-layouts-available
  (let* ((twenty-path (asdf:system-relative-pathname
                       :manna-cadet/test "t/fixtures/large-layout.layout"))
         (twenty (with-open-file (stream twenty-path) (read-layout stream)))
         (twenty-one
           (make-layout
            :name :twenty-one
            :levels (loop for index below 21
                          collect (make-level
                                   :name (make-symbol (format nil "level-~D" index))))
            :layers (list (make-layer :name :root))
            :root-layer :root
            :keys (list (make-abstract-key :name :key)))))
    (and (search "LAYOUT" (inspect-layout twenty))
         (search "LEVEL |level-20|" (inspect-layout twenty-one)))))
