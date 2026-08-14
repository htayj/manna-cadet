;;;; parse-tests.lisp - grammar tests for abstract layout parsing

(in-package #:manna-cadet.test)

(defun identifier-names (objects accessor)
  (mapcar (lambda (object) (symbol-name (funcall accessor object))) objects))

(defun binding-named (layer level bindings)
  (find-if (lambda (binding)
             (and (string= layer (symbol-name (binding-layer binding)))
                  (string= level (symbol-name (binding-level binding)))))
           bindings))

(deftest parses-complete-large-textual-layout
  (let* ((path (asdf:system-relative-pathname
                :manna-cadet/test "t/fixtures/large-layout.layout"))
         (layout (with-open-file (stream path) (read-layout stream)))
         (levels (layout-levels layout))
         (primary (first (layout-keys layout)))
         (selector (second (layout-keys layout)))
         (custom (binding-action
                  (binding-named "root" "custom"
                                 (abstract-key-bindings primary))))
         (tap-hold (binding-action
                    (binding-named "root" "plain"
                                   (abstract-key-bindings selector))))
         (combo (first (layout-combos layout))))
    (and (= 20 (length levels))
         (= 5 (length (layout-dimensions layout)))
         (= 8 (length (layout-modifiers layout)))
         (equal '("root" "fun")
                (identifier-names (layout-layers layout) #'layer-name))
         (string= "root" (symbol-name (layout-root-layer layout)))
         (equal '("case" "alphabet" "surface" "mode" "group")
                (mapcar (lambda (selector) (symbol-name (car selector)))
                        (level-selectors (car (last levels)))))
         (mods-action-p custom)
         (tap-hold-action-p tap-hold)
         (symbol-action-p (tap-hold-action-instant tap-hold))
         (simultaneous-action-p (tap-hold-action-continuous tap-hold))
         (hold-selector-action-p
          (second (simultaneous-action-actions
                   (tap-hold-action-continuous tap-hold))))
         (equal '("primary" "selector")
                (mapcar #'symbol-name (combo-keys combo)))
         (command-action-p
          (binding-action (first (combo-bindings combo)))))))

(deftest parses-every-action-production
  (let* ((layout
           (read-layout-string
            "(layout actions
               (keys (key all
                 ((root a) bare)
                 ((root b) (symbol explicit))
                 ((root c) (command run))
                 ((root d) (mods one two))
                 ((root e) (hold-selector plane selected))
                 ((root f) (layer-hold fun))
                 ((root g) (simultaneous (mods one) (layer-hold fun)))
                 ((root h) (tap-hold none (mods one)))
                 ((root i) none)
                 ((root j) transparent))))"))
         (actions (mapcar #'binding-action
                          (abstract-key-bindings (first (layout-keys layout))))))
    (and (symbol-action-p (nth 0 actions))
         (symbol-action-p (nth 1 actions))
         (command-action-p (nth 2 actions))
         (mods-action-p (nth 3 actions))
         (hold-selector-action-p (nth 4 actions))
         (layer-hold-action-p (nth 5 actions))
         (simultaneous-action-p (nth 6 actions))
         (tap-hold-action-p (nth 7 actions))
         (none-action-p (tap-hold-action-instant (nth 7 actions)))
         (none-action-p (nth 8 actions))
         (transparent-action-p (nth 9 actions)))))

(deftest parser-preserves-declaration-and-assignment-order
  (let ((layout
          (read-layout-string
           "(layout order
              (dimensions
                (left (default low) (states low middle high))
                (right (default off) (states off on)))
              (levels
                (first (selectors))
                (second (selectors (right on) (left high))))
              (layers (root base) (layer fun) (layer nav))
              (modifiers (one two))
              (commands (start stop))
              (symbols z y x)
              (keys (key b) (key a)))")))
    (and (equal '("left" "right")
                (identifier-names (layout-dimensions layout) #'dimension-name))
         (equal '("low" "middle" "high")
                (mapcar #'symbol-name
                        (dimension-states (first (layout-dimensions layout)))))
         (equal '("right" "left")
                (mapcar (lambda (selector) (symbol-name (car selector)))
                        (level-selectors (second (layout-levels layout)))))
         (equal '("base" "fun" "nav")
                (identifier-names (layout-layers layout) #'layer-name)))))

(deftest parser-does-not-enforce-semantic-references-or-uniqueness
  (let ((layout
          (read-layout-string
           "(layout structural
              (dimensions (d (default missing) (states one one)))
              (levels (duplicate (selectors (d absent)))
                      (duplicate (selectors))
                      (orphan (selectors) (fallback missing)))
              (layers (root root))
              (keys (key k ((missing-layer missing-level) missing-symbol)))
              (combos
                (combo (missing-key other-missing-key)
                       ((root duplicate) (command missing-command)))))")))
    (and (= 3 (length (layout-levels layout)))
         (= 1 (length (layout-keys layout)))
         (= 1 (length (layout-combos layout))))))

(deftest parser-rejects-malformed-layout-productions
  (every
   (lambda (source)
     (signals-condition-p 'layout-parse-error
                          (lambda () (read-layout-string source))))
   '("(not-layout name)"
     "(layout)"
     "(layout name atom-section)"
     "(layout name (unknown value))"
     "(layout name (dimensions d))"
     "(layout name (dimensions (d (states a) (default a))))"
     "(layout name (levels plain))"
     "(layout name (levels (plain (fallback low))))"
     "(layout name (levels (plain (selectors (d)))))"
     "(layout name (levels (plain (selectors) (fallback))))"
     "(layout name (layers))"
     "(layout name (layers (layer root)))"
     "(layout name (layers (root root) fun))"
     "(layout name (keys bad))"
     "(layout name (keys (key)))"
     "(layout name (keys (key k (root level))))"
     "(layout name (combos ((a b) none)))"
     "(layout name (combos (combo a)))")))

(deftest parser-rejects-malformed-action-productions
  (every
   (lambda (action)
     (signals-condition-p
      'layout-parse-error
      (lambda ()
        (read-layout-string
         (format nil "(layout bad (keys (key k ((root base) ~A))))" action)))))
   '("42" "()" "(unknown x)" "(symbol)" "(symbol x y)"
     "(command)" "(mods good 1)" "(hold-selector)"
     "(hold-selector x)" "(hold-selector x y z)" "(layer-hold)"
      "(layer-hold x y)" "(tap x)" "(hold x)" "(tap-hold x)"
      "(tap-hold x y z)")))

(deftest parse-error-report-omits-raw-hostile-context
  (let* ((hostile (make-symbol (format nil "bad~Cid" (code-char #x1B))))
         (context (list (make-symbol "symbol") hostile (make-symbol "extra")))
         (condition
           (handler-case
               (manna-cadet::parse-action context)
             (layout-parse-error (value) value)))
         (report (princ-to-string condition)))
    (and (equal context (layout-parse-error-context condition))
         (search "SYMBOL action requires one identifier" report)
         (not (find (code-char #x1B) report))
         (not (find #\Newline report)))))
