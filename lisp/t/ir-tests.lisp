;;;; ir-tests.lisp - tests for the pure abstract layout IR

(in-package #:manna-cadet.test)

(defun names (prefix count)
  (loop for index from 1 to count
        collect (intern (format nil "~A-~2,'0D" prefix index) :keyword)))

(defun address (layer level action)
  (make-binding :layer layer :level level :action action))

(deftest large-abstract-layout
  (let* ((state-names (names "STATE" 20))
         (dimension (make-dimension :name :selection :states state-names
                                    :default (first state-names)))
         (level-names (names "LEVEL" 20))
         (levels (loop for name in level-names for state in state-names
                       collect (make-level :name name
                                           :selectors (list (cons :selection state)))))
         (action (make-symbol-action :symbol :alpha))
         (key (make-abstract-key
               :name :primary
               :bindings (list (address :root (car (last level-names)) action))))
         (layout (make-layout :name :large :levels levels
                              :layers (list (make-layer :name :root))
                              :root-layer :root :dimensions (list dimension)
                              :symbols (list (make-layout-symbol :name :alpha))
                              :keys (list key))))
    (multiple-value-bind (resolved found-p)
        (key-action-at key (car (last level-names)) layout)
      (and (= 20 (length (layout-levels layout)))
           (= 20 (length (dimension-states dimension)))
           (eq action resolved) found-p))))

(deftest arbitrary-modifier-count
  (let* ((modifier-names (names "MODIFIER" 8))
         (action (make-mods-action :modifiers modifier-names))
         (layout (make-layout
                  :name :many-modifiers
                  :modifiers (mapcar (lambda (name) (make-modifier :name name))
                                     modifier-names))))
    (and (= 8 (length (layout-modifiers layout)))
         (equal modifier-names (mods-action-modifiers action)))))

(deftest exactly-eight-manna-levels-and-separate-fun-layer
  (let* ((level-names '(:normal :shift :greek :greek-shift :top :top-shift
                         :top-greek :top-greek-shift))
         (layout (make-layout
                  :name :manna-shape
                  :levels (mapcar (lambda (name) (make-level :name name)) level-names)
                  :layers (list (make-layer :name :root) (make-layer :name :fun))
                  :root-layer :root)))
    (and (equal level-names (mapcar #'level-name (layout-levels layout)))
         (equal '(:root :fun) (mapcar #'layer-name (layout-layers layout)))
         (not (member :fun level-names)))))

(deftest layer-major-resolution-distinguishes-none-transparent-and-absence
  (let* ((root-action (make-symbol-action :symbol :root-result))
         (fallback-action (make-symbol-action :symbol :fallback-result))
         (levels (list (make-level :name :base)
                       (make-level :name :shift :fallback :base)))
         (layout (make-layout :name :resolution :levels levels
                              :layers (list (make-layer :name :root)
                                            (make-layer :name :fun))
                              :root-layer :root))
         (absent (make-abstract-key
                  :name :absent
                  :bindings (list (address :root :shift root-action))))
         (fallback (make-abstract-key
                    :name :fallback
                    :bindings (list (address :fun :base fallback-action)
                                    (address :root :shift root-action))))
         (transparent (make-abstract-key
                       :name :transparent
                       :bindings (list (address :fun :shift (make-transparent-action))
                                       (address :fun :base fallback-action)
                                       (address :root :shift root-action))))
         (none (make-abstract-key
                :name :none
                :bindings (list (address :fun :shift (make-none-action))
                                (address :root :shift root-action))))
         (unbound (make-abstract-key :name :unbound)))
    (multiple-value-bind (none-result none-found-p)
        (key-action-at none :shift layout '(:fun))
      (multiple-value-bind (unbound-result unbound-found-p)
          (key-action-at unbound :shift layout '(:fun))
        (and (eq root-action (key-action-at absent :shift layout '(:fun)))
             (eq fallback-action (key-action-at fallback :shift layout '(:fun)))
             (eq root-action (key-action-at transparent :shift layout '(:fun)))
             (null none-result) none-found-p
             (null unbound-result) (not unbound-found-p))))))

(deftest root-is-appended-once
  (let* ((action (make-symbol-action :symbol :root-result))
         (key (make-abstract-key :name :key
                                 :bindings (list (address :root :base action))))
         (layout (make-layout :name :root
                              :levels (list (make-level :name :base))
                              :layers (list (make-layer :name :root))
                              :root-layer :root)))
    (and (eq action (key-action-at key :base layout))
         (eq action (key-action-at key :base layout '(:root))))))

(deftest root-is-always-last-after-active-layer-normalization
  (let* ((root-action (make-symbol-action :symbol :root-result))
         (top-action (make-symbol-action :symbol :top-result))
         (lower-action (make-symbol-action :symbol :lower-result))
         (key (make-abstract-key
               :name :key
               :bindings (list (address :root :base root-action)
                               (address :top :base top-action)
                               (address :lower :base lower-action))))
         (layout (make-layout
                  :name :root-last
                  :levels (list (make-level :name :base))
                  :layers (list (make-layer :name :root)
                                (make-layer :name :top)
                                (make-layer :name :lower))
                  :root-layer :root)))
    (and (eq top-action
             (key-action-at key :base layout
                            '(:root :top :top :lower :root)))
         (eq lower-action
             (key-action-at key :base layout
                            '(:root :lower :top :lower :root))))))

(deftest combo-resolution-matches-key-resolution
  (let* ((root-action (make-command-action :command :root))
         (fun-action (make-command-action :command :fun))
         (combo (make-combo :keys '(:a :b)
                            :bindings (list (address :root :base root-action)
                                            (address :fun :base fun-action))))
         (layout (make-layout :name :combo
                              :levels (list (make-level :name :base))
                              :layers (list (make-layer :name :root)
                                            (make-layer :name :fun))
                              :root-layer :root)))
    (and (eq root-action (combo-action-at combo :base layout))
          (eq fun-action (combo-action-at combo :base layout '(:fun))))))

(deftest exact-name-identifiers-resolve-across-distinct-symbols
  (let* ((declared-root (make-symbol "root"))
         (referenced-root (make-symbol "root"))
         (declared-base (make-symbol "base"))
         (referenced-base (make-symbol "base"))
         (declared-alpha (make-symbol "alpha"))
         (referenced-alpha (make-symbol "alpha"))
         (action (make-symbol-action :symbol referenced-alpha))
         (key (make-abstract-key
               :name (make-symbol "key")
               :bindings (list (address referenced-root referenced-base action))))
         (layout (make-layout
                  :name (make-symbol "programmatic")
                  :levels (list (make-level :name declared-base))
                  :layers (list (make-layer :name declared-root))
                  :root-layer (make-symbol "root")
                  :symbols (list (make-layout-symbol :name declared-alpha))
                  :keys (list key))))
    (multiple-value-bind (diagnostics valid-p) (validate-layout layout)
      (multiple-value-bind (resolved found-p)
          (key-action-at key (make-symbol "base") layout)
        (and valid-p
             (null diagnostics)
             found-p
             (eq action resolved))))))

(deftest exact-name-identifier-semantics-remain-case-sensitive
  (let* ((dimension-name (make-symbol "case"))
         (lower (make-symbol "q"))
         (upper (make-symbol "Q"))
         (layout (make-layout
                  :name (make-symbol "case-sensitive")
                  :dimensions
                  (list (make-dimension :name dimension-name
                                        :states (list lower upper)
                                        :default lower))
                  :levels
                  (list (make-level :name (make-symbol "q")
                                    :selectors
                                    (list (cons (make-symbol "case")
                                                (make-symbol "q"))))
                        (make-level :name (make-symbol "Q")
                                    :selectors
                                    (list (cons (make-symbol "case")
                                                (make-symbol "Q")))))
                  :layers (list (make-layer :name (make-symbol "root")))
                  :root-layer (make-symbol "root"))))
    (multiple-value-bind (diagnostics valid-p) (validate-layout layout)
      (and valid-p (null diagnostics)))))

(deftest nil-is-not-an-identifier-named-nil
  (let ((named-nil (make-symbol "NIL")))
    (and (not (manna-cadet::identifier-equal-p nil named-nil))
         (not (manna-cadet::identifier-equal-p named-nil nil))
         (manna-cadet::identifier-equal-p named-nil (make-symbol "NIL"))
         (not (manna-cadet::identifier-equal-p
               (make-symbol "nil") named-nil)))))

(deftest simultaneous-shift-semantics
  (let* ((modifier (make-mods-action :modifiers '(:shift)))
         (selector (make-hold-selector-action :dimension :case :state :shift))
         (action (make-simultaneous-action :actions (list modifier selector)))
         (tap-hold (make-tap-hold-action
                    :instant (make-symbol-action :symbol :alpha)
                    :continuous action)))
    (and (simultaneous-action-p action)
         (equal (list modifier selector) (simultaneous-action-actions action))
         (eq :shift (hold-selector-action-state selector))
         (eq action (tap-hold-action-continuous tap-hold)))))

(deftest ir-source-is-pure-and-unbounded
  (let* ((path (asdf:system-relative-pathname :manna-cadet "src/ir.lisp"))
         (source (string-downcase (uiop:read-file-string path)))
         (forbidden '("make-array" "simple-vector" "bitmask" "keycode"
                      "linux" "backend" "device" "transport" "matrix"
                      "coordinate" "xkb" "kanata" "qmk")))
    (and (notany (lambda (character) (digit-char-p character)) source)
         (notany (lambda (term) (search term source)) forbidden))))
