;;;; validate-tests.lisp - semantic validation tests

(in-package #:manna-cadet.test)

(defun validation-codes (layout)
  (mapcar #'diagnostic-code (validate-layout layout)))

(defun validates-p (layout)
  (multiple-value-bind (diagnostics ok-p) (validate-layout layout)
    (and ok-p (null diagnostics))))

(defun invalid-layout-names ()
  (list nil
        (make-symbol "")
        (make-symbol (string (code-char #x85)))
        (make-symbol (string (code-char #x2028)))
         (make-symbol (string (code-char #x2029)))))

(defun unsafe-format-code-points ()
  '(#x061C #x200E #x200F #x202A #x202B #x202C #x202D #x202E
    #x2066 #x2067 #x2068 #x2069 #xFEFF))

(defun make-fallback-chain-layout (count &key cycle-p)
  (let* ((names (coerce
                 (loop for index below count
                       collect (make-symbol (format nil "level-~D" index)))
                 'vector))
         (states (coerce
                  (loop for index below count
                        collect (make-symbol (format nil "state-~D" index)))
                  'vector))
         (levels
           (loop for index downfrom (1- count) to 0
                 collect
                 (make-level
                  :name (aref names index)
                  :selectors (list (cons :chain (aref states index)))
                  :fallback (cond
                              ((plusp index) (aref names (1- index)))
                              (cycle-p (aref names 1)))))))
    (make-layout
     :name :fallback-chain
     :dimensions
     (list (make-dimension :name :chain
                           :states (coerce states 'list)
                           :default (aref states 0)))
     :levels levels
     :layers (list (make-layer :name :root))
     :root-layer :root)))

(deftest diagnostic-api-and-required-declarations
  (multiple-value-bind (diagnostics ok-p)
      (validate-layout (make-layout :name :empty))
    (and (not ok-p)
         (equal '(:no-levels :no-layers :invalid-identifier)
                (mapcar #'diagnostic-code diagnostics))
         (every #'diagnostic-p diagnostics)
         (every (lambda (diagnostic)
                  (eq :error (diagnostic-severity diagnostic)))
                 diagnostics))))

(deftest invalid-programmatic-layout-names-fail-validation
  (every
   (lambda (name)
     (equal '(:invalid-identifier)
            (validation-codes
             (make-layout
              :name name
              :levels (list (make-level :name :base))
              :layers (list (make-layer :name :root))
              :root-layer :root))))
    (invalid-layout-names)))

(deftest bidi-and-format-controls-are-invalid-identifiers
  (every
   (lambda (code)
     (not (valid-identifier-p
           (make-symbol (format nil "safe~Cunsafe" (code-char code))))))
   (unsafe-format-code-points)))

(deftest unsafe-symbol-display-strings-fail-validation-deterministically
  (every
   (lambda (code)
     (equal '(:invalid-display-string)
            (validation-codes
             (make-layout
              :name :display-controls
              :levels (list (make-level :name :base))
              :layers (list (make-layer :name :root))
              :root-layer :root
              :symbols
              (list (make-layout-symbol
                     :name :unsafe
                     :display (format nil "before~Cafter" (code-char code))))))))
   (append '(#x1B #x7F #x85 #x2028 #x2029)
           (unsafe-format-code-points))))

(deftest invalid-layout-name-precedes-declaration-diagnostics
  (equal '(:invalid-identifier :duplicate-level :duplicate-selector-tuple)
         (validation-codes
          (make-layout
           :name nil
           :levels (list (make-level :name :base)
                         (make-level :name :base))
           :layers (list (make-layer :name :root))
           :root-layer :root))))

(deftest printable-symbol-named-nil-is-a-valid-layout-name
  (validates-p
   (make-layout :name (make-symbol "NIL")
                :levels (list (make-level :name :base))
                :layers (list (make-layer :name :root))
                :root-layer :root)))

(deftest dimension-states-are-nonempty-unique-and-defaulted
  (let ((layout
          (read-layout-string
           "(layout dimensions
              (dimensions
                (empty (default absent) (states))
                (duplicate (default one) (states one one))
                (bad-default (default absent) (states present)))
              (levels (base (selectors)))
              (layers (root root)))")))
    (equal '(:empty-dimension-states :unknown-dimension-default
             :duplicate-dimension-state :unknown-dimension-default)
           (validation-codes layout))))

(deftest declarations-and-layer-root-are-validated
  (let ((layout
          (read-layout-string
           "(layout duplicates
              (dimensions (d (default a) (states a))
                          (d (default a) (states a)))
              (levels (l (selectors)) (l (selectors (d a))))
              (layers (root missing) (layer layer) (layer layer))
              (modifiers (m m)) (commands (c c)) (symbols s s)
              (keys (key k) (key k)))")))
    (equal '(:duplicate-dimension :duplicate-level :duplicate-layer
             :duplicate-modifier :duplicate-command :duplicate-symbol
             :duplicate-key :duplicate-selector-tuple)
           (validation-codes layout))))

(deftest programmatic-root-layer-must-resolve
  (equal '(:unknown-root-layer)
         (validation-codes
          (make-layout :name :bad-root
                       :levels (list (make-level :name :base))
                       :layers (list (make-layer :name :root))
                       :root-layer :missing))))

(deftest selector-assignments-resolve-and-dimensions-do-not-repeat
  (let ((layout
          (read-layout-string
           "(layout selectors
              (dimensions (known (default off) (states off on)))
              (levels
                (base (selectors))
                (unknown-dimension (selectors (missing on)))
                (unknown-state (selectors (known absent)))
                (repeated (selectors (known on) (known off))))
              (layers (root root)))")))
    (equal '(:unknown-selector-dimension :unknown-selector-state
             :duplicate-level-selector)
           (validation-codes layout))))

(deftest duplicate-selector-tuples-are-rejected-but-partial-tuples-are-valid
  (let ((invalid
          (read-layout-string
           "(layout duplicate-tuples
              (dimensions (a (default off) (states off on))
                          (b (default off) (states off on)))
              (levels (one (selectors (a on)))
                      (two (selectors (a on))))
              (layers (root root)))"))
        (valid
          (read-layout-string
           "(layout partial
              (dimensions (a (default off) (states off on))
                          (b (default off) (states off on)))
              (levels (base (selectors))
                      (a-only (selectors (a on)))
                      (b-only (selectors (b on))))
              (layers (root root)))")))
    (and (equal '(:duplicate-selector-tuple) (validation-codes invalid))
         (validates-p valid))))

(deftest omitted-selectors-canonicalize-to-dimension-defaults
  (let ((layout
          (read-layout-string
           "(layout default-tuples
              (dimensions (case (default normal) (states normal shifted))
                          (script (default latin) (states latin greek)))
              (levels (implicit (selectors))
                      (explicit (selectors (case normal) (script latin))))
              (layers (root root)))")))
    (equal '(:duplicate-selector-tuple) (validation-codes layout))))

(deftest selector-assignment-order-does-not-affect-tuple-identity
  (let ((layout
          (read-layout-string
           "(layout reordered-tuples
              (dimensions (case (default normal) (states normal shifted))
                          (script (default latin) (states latin greek)))
              (levels (case-first (selectors (case shifted) (script greek)))
                      (script-first (selectors (script greek) (case shifted))))
              (layers (root root)))")))
    (equal '(:duplicate-selector-tuple) (validation-codes layout))))

(deftest fallback-references-and-cycles-are-rejected
  (let ((layout
          (read-layout-string
           "(layout fallback
              (dimensions (d (default zero) (states zero one two three)))
              (levels
                (root (selectors))
                (bad (selectors (d one)) (fallback absent))
                (a (selectors (d two)) (fallback b))
                (b (selectors (d three)) (fallback a)))
              (layers (root root)))")))
    (equal '(:unknown-fallback-level :fallback-cycle)
            (validation-codes layout))))

(deftest two-thousand-level-acyclic-fallback-chain-validates-promptly
  (let ((started (get-internal-real-time)))
    (prog1
        (validates-p (make-fallback-chain-layout 2000))
      (assert (< (/ (- (get-internal-real-time) started)
                    internal-time-units-per-second)
                 10)))))

(deftest long-chain-into-cycle-reports-one-cycle
  (let ((diagnostics (validate-layout
                      (make-fallback-chain-layout 2000 :cycle-p t))))
    (and (= 1 (count :fallback-cycle diagnostics :key #'diagnostic-code))
         (null (remove :fallback-cycle diagnostics
                       :key #'diagnostic-code)))))

(deftest disjoint-fallback-cycles-follow-root-declaration-order
  (let* ((state-a (make-symbol "state-a"))
         (state-b (make-symbol "state-b"))
         (state-c (make-symbol "state-c"))
         (state-d (make-symbol "state-d"))
         (a (make-symbol "a"))
         (b (make-symbol "b"))
         (c (make-symbol "c"))
         (d (make-symbol "d"))
         (layout
           (make-layout
            :name :disjoint-cycles
            :dimensions
            (list (make-dimension :name :cycle
                                  :states (list state-a state-b state-c state-d)
                                  :default state-a))
            :levels
            (list (make-level :name a :selectors (list (cons :cycle state-a))
                              :fallback b)
                  (make-level :name b :selectors (list (cons :cycle state-b))
                              :fallback a)
                  (make-level :name c :selectors (list (cons :cycle state-c))
                              :fallback d)
                  (make-level :name d :selectors (list (cons :cycle state-d))
                              :fallback c))
            :layers (list (make-layer :name :root))
            :root-layer :root))
         (cycles (remove-if-not
                  (lambda (diagnostic)
                    (eq :fallback-cycle (diagnostic-code diagnostic)))
                  (validate-layout layout))))
    (equal (list (list a b a) (list c d c))
           (mapcar (lambda (diagnostic)
                     (getf (diagnostic-context diagnostic) :cycle))
                   cycles))))

(deftest binding-addresses-resolve-and-are-unique-per-owner
  (let ((layout
          (read-layout-string
           "(layout bindings
              (levels (base (selectors)))
              (layers (root root))
              (keys
                (key a ((missing base) none) ((root missing) none)
                       ((root base) none) ((root base) transparent))
                (key b))
              (combos
                (combo (a b) ((missing base) none) ((root missing) none)
                             ((root base) none) ((root base) transparent))))")))
    (equal '(:unknown-binding-layer :unknown-binding-level
             :duplicate-key-binding :unknown-binding-layer
             :unknown-binding-level :duplicate-combo-binding)
           (validation-codes layout))))

(deftest action-references-and-types-are-enforced
  (let ((layout
          (read-layout-string
           "(layout actions
              (dimensions (case (default normal) (states normal shift)))
              (levels (base (selectors)))
              (layers (root root) (layer fun))
              (modifiers (shift)) (commands (run)) (symbols alpha)
              (keys
                (key references
                  ((root base)
                   (simultaneous (mods missing)
                     (hold-selector missing state)
                     (hold-selector case missing)
                     (layer-hold missing))))
                (key too-few ((root base) (simultaneous (mods shift))))
                (key wrong-simultaneous
                  ((root base) (simultaneous alpha (mods shift))))
                (key wrong-tap
                  ((root base) (tap-hold (mods shift) alpha)))))")))
    (equal '(:unknown-modifier :unknown-selector-dimension
             :unknown-selector-state :unknown-action-layer
             :simultaneous-too-few-actions :simultaneous-action-type
             :tap-hold-instant-type :tap-hold-continuous-type)
           (validation-codes layout))))

(deftest simultaneous-shift-and-selector-is-valid
  (validates-p
   (read-layout-string
    "(layout shift
       (dimensions (case (default normal) (states normal shift)))
       (levels (normal (selectors)))
       (layers (root root))
       (modifiers (shift)) (symbols alpha)
       (keys (key shift
         ((root normal)
          (tap-hold alpha
            (simultaneous (mods shift) (hold-selector case shift)))))))")))

(deftest combo-invariants-are-enforced
  (let ((layout
          (read-layout-string
           "(layout combos
              (levels (base (selectors)))
              (layers (root root))
               (keys (key a) (key b) (key c))
               (combos
                 (combo (a a missing) ((root base) none))
                 (combo (a) ((root base) none))
                 (combo (a b) ((root base) none))
                 (combo (b a) ((root base) transparent))))")))
    (equal '(:duplicate-combo-key :unknown-combo-key :combo-too-few-keys
              :combo-too-few-keys :duplicate-combo)
            (validation-codes layout))))

(deftest combo-actions-accept-all-abstract-action-kinds
  (validates-p
   (read-layout-string
    "(layout combo-actions
       (dimensions
         (mode (default zero) (states zero one two three four)))
       (levels
         (modifier (selectors (mode zero)))
         (layer (selectors (mode one)))
         (selector (selectors (mode two)))
         (simultaneous (selectors (mode three)))
         (tap-hold (selectors (mode four))))
       (layers (root root) (layer fun))
       (modifiers (shift))
       (symbols alpha)
       (keys (key a) (key b))
       (combos
         (combo (a b)
           ((root modifier) (mods shift))
           ((root layer) (layer-hold fun))
           ((root selector) (hold-selector mode one))
           ((root simultaneous)
            (simultaneous (mods shift) (hold-selector mode one)))
           ((root tap-hold) (tap-hold alpha (mods shift))))))")))

(deftest combo-actions-retain-recursive-reference-and-type-validation
  (let ((layout
          (read-layout-string
           "(layout combo-action-errors
              (levels (base (selectors)))
              (layers (root root))
              (modifiers (shift))
              (symbols alpha)
              (keys (key a) (key b) (key c))
              (combos
                (combo (a b)
                  ((root base) (simultaneous (mods missing) alpha)))
                (combo (a c)
                  ((root base) (tap-hold (mods shift) alpha)))))")))
    (equal '(:unknown-modifier :simultaneous-action-type
             :tap-hold-instant-type :tap-hold-continuous-type)
           (validation-codes layout))))

(deftest large-textual-layout-validates-without-fixed-count-rules
  (let* ((path (asdf:system-relative-pathname
                :manna-cadet/test "t/fixtures/large-layout.layout"))
         (layout (with-open-file (stream path) (read-layout stream))))
    (and (validates-p layout)
         (= 20 (length (layout-levels layout)))
         (= 5 (length (layout-dimensions layout)))
         (= 8 (length (layout-modifiers layout)))
         (= 2 (length (layout-layers layout))))))

(deftest pure-layout-may-have-only-its-root-layer
  (validates-p
   (read-layout-string
    "(layout pure (levels (base (selectors))) (layers (root root)))")))

(deftest programmatic-invalid-identifiers-fail-validation
  "Failing-first regression: layers FUN and NIL, root NIL must fail with
:invalid-identifier. Normalization/round-trip must not claim semantic equality."
  (let* ((fun (make-layer :name :fun))
         (nil-layer (make-layer :name nil))
         (layout (make-layout
                  :name :bad-identifiers
                  :levels (list (make-level :name :base))
                  :layers (list fun nil-layer)
                  :root-layer nil
                  :symbols (list (make-layout-symbol :name :alpha))
                  :keys (list (make-abstract-key
                               :name :k
                               :bindings (list (make-binding
                                                :layer :root
                                                :level :base
                                                :action (make-symbol-action :symbol :alpha))))))))
    (multiple-value-bind (diagnostics valid-p)
        (validate-layout layout)
      (and (not valid-p)
           (member :invalid-identifier (mapcar #'diagnostic-code diagnostics))
           (not (validates-p layout))
           ;; programmatic invalid layout should not round-trip as equal
           (let ((normalized-text (normalize-layout layout)))
             (not (search "(root NIL)" normalized-text)))))))

(deftest parsed-and-programmatic-inspection-with-control-chars
  "Inspection hardening for C1 and Unicode line separators. Raw chars absent."
  (let ((c1 (make-symbol (format nil "c1~Cname" (code-char #x85))))
        (line-sep (make-symbol (format nil "line~Csep" (code-char #x2028)))))
    (let ((ins-c1 (inspection-identifier c1))
          (ins-sep (inspection-identifier line-sep)))
      (and (search "\\x85;" ins-c1)
           (search "\\x2028;" ins-sep)
           (not (search (string (code-char #x85)) ins-c1))
           (not (search (string (code-char #x2028)) ins-sep))
           (search "|c1" ins-c1)
           (search "name|" ins-c1)
           (search "|line" ins-sep)
            (search "sep|" ins-sep)))))

(deftest inspection-escapes-bidi-and-format-controls
  (every
   (lambda (code)
     (let* ((character (code-char code))
            (escaped (inspection-identifier
                      (make-symbol (format nil "before~Cafter" character)))))
       (and (search (format nil "\\x~X;" code) escaped)
            (not (find character escaped)))))
   (unsafe-format-code-points)))

(deftest validator-source-has-no-realization-feasibility-concepts
  (let* ((path (asdf:system-relative-pathname :manna-cadet "src/validate.lisp"))
         (source (string-downcase (uiop:read-file-string path)))
         (forbidden '("backend" "device" "transport" "bit budget"
                      "bit-budget" "fixed limit" "physical feasibility"
                      "keycode" "matrix" "coordinate")))
    (notany (lambda (term) (search term source)) forbidden)))
