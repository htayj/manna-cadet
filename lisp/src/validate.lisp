;;;; validate.lisp - semantic validation for the abstract layout IR

(in-package #:manna-cadet)

(defstruct diagnostic
  (severity :error :type (member :error :warning) :read-only t)
  (code nil :type keyword :read-only t)
  (message "" :type string :read-only t)
  (context '() :type list :read-only t))

(defun invalid-identifier-code-point-p (code)
  (unsafe-text-code-point-p code))

(defun valid-identifier-p (value)
  "Return true when VALUE is a non-NIL symbol whose name contains only
printable characters that cannot disrupt line-oriented text records."
  (and (symbolp value)
       (not (null value))
       (let ((name (symbol-name value)))
         (and (> (length name) 0)
              (notany #'invalid-identifier-code-point-p
                      (map 'list #'char-code name))))))

(defun validate-layout (layout)
  "Return ordered semantic diagnostics for LAYOUT and whether it has no errors."
  (let ((diagnostics '())
        (dimensions (layout-dimensions layout))
        (levels (layout-levels layout))
        (layers (layout-layers layout))
        (modifiers (layout-modifiers layout))
        (commands (layout-commands layout))
        (symbols (layout-symbols layout))
        (keys (layout-keys layout))
        (combos (layout-combos layout)))
    (labels ((report (code message &rest context)
               (push (make-diagnostic :code code :message message :context context)
                     diagnostics))
               (table-for (objects accessor)
                 (let ((table (make-identifier-table)))
                   (dolist (object objects table)
                     (let ((name (funcall accessor object)))
                       (when (and name (not (valid-identifier-p name)))
                         (report :invalid-identifier
                                 "Declaration name is not a valid identifier"
                                 :name name))
                       (when name
                         (setf (identifier-table-value name table)
                               object))))))
             (reserved-p (name)
               (or (token-named-p name "none")
                   (token-named-p name "transparent")))
             (validate-declarations (objects accessor kind duplicate-code)
                 (let ((seen (make-identifier-table)))
                  (loop for object in objects
                        for index from 0
                        for name = (funcall accessor object)
                        do (if (valid-identifier-p name)
                               (progn
                                 (when (reserved-p name)
                                   (report :reserved-identifier
                                           "NONE and TRANSPARENT are reserved action identifiers"
                                           :kind kind :index index :name name))
                                 (if (identifier-table-value name seen)
                                     (report duplicate-code "Declaration name is not unique"
                                             :kind kind :index index :name name)
                                     (setf (identifier-table-value name seen) t)))
                               (report :invalid-identifier
                                       "Declaration name is not a valid identifier"
                                       :kind kind :index index :name name))))))
      (unless (valid-identifier-p (layout-name layout))
        (report :invalid-identifier "Layout name is not a valid identifier"
                :layout-name (layout-name layout)))
      (unless levels
        (report :no-levels "A layout must declare at least one level"
                :layout (layout-name layout)))
      (unless layers
        (report :no-layers "A layout must declare at least one layer"
                :layout (layout-name layout)))
      (validate-declarations dimensions #'dimension-name :dimension
                             :duplicate-dimension)
      (validate-declarations levels #'level-name :level :duplicate-level)
      (validate-declarations layers #'layer-name :layer :duplicate-layer)
      (validate-declarations modifiers #'modifier-name :modifier
                             :duplicate-modifier)
      (validate-declarations commands #'command-name :command :duplicate-command)
       (validate-declarations symbols #'layout-symbol-name :symbol :duplicate-symbol)
       (validate-declarations keys #'abstract-key-name :key :duplicate-key)
       (loop for layout-symbol in symbols
             for symbol-index from 0
             for display = (layout-symbol-display layout-symbol)
             when (and display
                       (some #'unsafe-text-code-point-p
                             (map 'list #'char-code display)))
               do (report :invalid-display-string
                          "Symbol display contains an unsafe control or format character"
                          :symbol (layout-symbol-name layout-symbol)
                          :symbol-index symbol-index))

       (let ((dimension-table (table-for dimensions #'dimension-name))
            (level-table (table-for levels #'level-name))
            (layer-table (table-for layers #'layer-name))
            (modifier-table (table-for modifiers #'modifier-name))
            (command-table (table-for commands #'command-name))
            (symbol-table (table-for symbols #'layout-symbol-name))
            (key-table (table-for keys #'abstract-key-name)))
         (unless (valid-identifier-p (layout-root-layer layout))
           (report :invalid-identifier "Root layer is not a valid identifier"
                   :root-layer (layout-root-layer layout)))
         (when (and (valid-identifier-p (layout-root-layer layout))
                    (not (identifier-table-value (layout-root-layer layout) layer-table)))
           (report :unknown-root-layer "Root layer must name a declared layer"
                   :root-layer (layout-root-layer layout)))

        (loop for dimension in dimensions
              for dimension-index from 0
              do (let ((states (dimension-states dimension))
                       (seen (make-identifier-table)))
                   (unless states
                     (report :empty-dimension-states
                             "Dimension must declare at least one state"
                             :dimension (dimension-name dimension)
                             :dimension-index dimension-index))
                   (loop for state in states
                         for state-index from 0
                          do (if (valid-identifier-p state)
                                 (if (identifier-table-value state seen)
                                     (report :duplicate-dimension-state
                                             "Dimension states must be unique"
                                             :dimension (dimension-name dimension)
                                             :state-index state-index :state state)
                                     (setf (identifier-table-value state seen) t))
                                 (report :invalid-identifier
                                         "Dimension state is not a valid identifier"
                                         :dimension (dimension-name dimension)
                                         :state-index state-index :state state)))
                    (let ((default (dimension-default dimension)))
                      (cond
                        ((not (valid-identifier-p default))
                         (report :invalid-identifier
                                 "Dimension default is not a valid identifier"
                                 :dimension (dimension-name dimension)
                                 :default default))
                        ((not (member default states :test #'identifier-equal-p))
                         (report :unknown-dimension-default
                                 "Dimension default must name one of its states"
                                 :dimension (dimension-name dimension)
                                 :default default))))))

        (let ((selector-tuples (make-hash-table :test #'equal)))
          (loop for level in levels
                for level-index from 0
                 do (let ((assigned (make-identifier-table))
                         (selectors-valid-p t))
                     (loop for selector in (level-selectors level)
                           for selector-index from 0
                           for dimension-name = (car selector)
                           for state = (cdr selector)
                           do (unless (valid-identifier-p dimension-name)
                                (setf selectors-valid-p nil)
                                (report :invalid-identifier
                                        "Level selector dimension is not a valid identifier"
                                        :level (level-name level)
                                        :selector-index selector-index
                                        :dimension dimension-name))
                               (unless (valid-identifier-p state)
                                 (setf selectors-valid-p nil)
                                 (report :invalid-identifier
                                         "Level selector state is not a valid identifier"
                                         :level (level-name level)
                                         :selector-index selector-index
                                         :state state))
                               (when (and (valid-identifier-p dimension-name)
                                          (valid-identifier-p state))
                                 (let ((dimension (identifier-table-value dimension-name
                                                                          dimension-table)))
                                   (unless dimension
                                     (setf selectors-valid-p nil)
                                     (report :unknown-selector-dimension
                                             "Level selector references an undeclared dimension"
                                             :level (level-name level)
                                             :selector-index selector-index
                                             :dimension dimension-name))
                                   (when (and dimension
                                              (not (member state (dimension-states dimension)
                                                           :test #'identifier-equal-p)))
                                     (setf selectors-valid-p nil)
                                     (report :unknown-selector-state
                                             "Level selector references an undeclared state"
                                             :level (level-name level)
                                             :selector-index selector-index
                                             :dimension dimension-name :state state))
                                   (multiple-value-bind (value present-p)
                                       (identifier-table-value dimension-name assigned)
                                     (declare (ignore value))
                                     (if present-p
                                         (progn
                                           (setf selectors-valid-p nil)
                                           (report :duplicate-level-selector
                                                   "Level assigns a dimension more than once"
                                                   :level (level-name level)
                                                   :selector-index selector-index
                                                   :dimension dimension-name))
                                         (setf (identifier-table-value dimension-name assigned)
                                               state))))))
                     (when selectors-valid-p
                       (let ((tuple
                               (mapcar
                                (lambda (dimension)
                                  (multiple-value-bind (state assigned-p)
                                       (identifier-table-value (dimension-name dimension)
                                                               assigned)
                                    (cons (dimension-name dimension)
                                          (if assigned-p state
                                              (dimension-default dimension)))))
                                dimensions)))
                          (let ((tuple-key
                                  (mapcar (lambda (selector)
                                            (cons (identifier-key (car selector))
                                                  (identifier-key (cdr selector))))
                                          tuple)))
                            (if (gethash tuple-key selector-tuples)
                              (report :duplicate-selector-tuple
                                      "Levels must not have duplicate selector tuples"
                                      :level (level-name level) :selectors tuple)
                                (setf (gethash tuple-key selector-tuples) t)))))
                       (let ((fallback (level-fallback level)))
                         (when fallback
                           (if (valid-identifier-p fallback)
                               (unless (identifier-table-value fallback level-table)
                                 (report :unknown-fallback-level
                                         "Level fallback references an undeclared level"
                                         :level (level-name level) :level-index level-index
                                         :fallback fallback))
                               (report :invalid-identifier
                                       "Level fallback is not a valid identifier"
                                       :level (level-name level) :level-index level-index
                                       :fallback fallback)))))))

        (let ((completed (make-identifier-table)))
          (dolist (root levels)
            (let ((root-name (level-name root)))
              (when (and (valid-identifier-p root-name)
                         (not (identifier-table-value root-name completed)))
                (let ((path (make-array 8 :adjustable t :fill-pointer 0))
                      (positions (make-identifier-table))
                      (current root-name))
                  (loop
                    (unless (valid-identifier-p current)
                      (return))
                    (let ((level (identifier-table-value current level-table)))
                      (unless level
                        (return))
                      (when (identifier-table-value current completed)
                        (return))
                      (multiple-value-bind (start present-p)
                          (identifier-table-value current positions)
                        (when present-p
                          (let ((cycle
                                  (loop for index from start below (length path)
                                        collect (aref path index))))
                            (report :fallback-cycle
                                    "Level fallback chain is cyclic"
                                    :level (first cycle)
                                    :cycle (append cycle (list (first cycle)))))
                          (return)))
                      (setf (identifier-table-value current positions)
                            (length path))
                      (vector-push-extend current path)
                      (setf current (level-fallback level))))
                  (loop for name across path
                        do (setf (identifier-table-value name completed) t)))))))

        (labels ((validate-action (action context)
                   (cond
                      ((symbol-action-p action)
                       (let ((name (symbol-action-symbol action)))
                         (if (valid-identifier-p name)
                             (unless (identifier-table-value name symbol-table)
                               (apply #'report :unknown-symbol
                                      "Action references an undeclared symbol"
                                      (append context (list :symbol name))))
                             (apply #'report :invalid-identifier
                                    "Action symbol is not a valid identifier"
                                    (append context (list :symbol name)))))
                       :instant)
                      ((command-action-p action)
                       (let ((name (command-action-command action)))
                         (if (valid-identifier-p name)
                             (unless (identifier-table-value name command-table)
                               (apply #'report :unknown-command
                                      "Action references an undeclared command"
                                      (append context (list :command name))))
                             (apply #'report :invalid-identifier
                                    "Action command is not a valid identifier"
                                    (append context (list :command name)))))
                       :instant)
                      ((mods-action-p action)
                       (loop for modifier in (mods-action-modifiers action)
                             for index from 0
                             do (if (valid-identifier-p modifier)
                                    (unless (identifier-table-value modifier modifier-table)
                                      (apply #'report :unknown-modifier
                                             "Action references an undeclared modifier"
                                             (append context (list :modifier-index index
                                                                   :modifier modifier))))
                                    (apply #'report :invalid-identifier
                                           "Action modifier is not a valid identifier"
                                           (append context (list :modifier-index index
                                                                 :modifier modifier)))))
                       :continuous)
                      ((hold-selector-action-p action)
                       (let* ((name (hold-selector-action-dimension action))
                              (state (hold-selector-action-state action))
                              (dimension-valid-p (valid-identifier-p name))
                              (state-valid-p (valid-identifier-p state)))
                         (unless dimension-valid-p
                           (apply #'report :invalid-identifier
                                  "Selector dimension is not a valid identifier"
                                  (append context (list :dimension name))))
                         (unless state-valid-p
                           (apply #'report :invalid-identifier
                                  "Selector state is not a valid identifier"
                                  (append context (list :state state))))
                         (when (and dimension-valid-p state-valid-p)
                           (let ((dimension (identifier-table-value name dimension-table)))
                             (unless dimension
                               (apply #'report :unknown-selector-dimension
                                      "Selector references an undeclared dimension"
                                      (append context (list :dimension name))))
                             (when (and dimension
                                        (not (member state (dimension-states dimension)
                                                     :test #'identifier-equal-p)))
                               (apply #'report :unknown-selector-state
                                      "Selector references an undeclared state"
                                      (append context (list :dimension name :state state)))))))
                       :continuous)
                      ((layer-hold-action-p action)
                       (let ((name (layer-hold-action-layer action)))
                         (if (valid-identifier-p name)
                             (unless (identifier-table-value name layer-table)
                               (apply #'report :unknown-action-layer
                                      "Layer hold references an undeclared layer"
                                      (append context (list :layer name))))
                             (apply #'report :invalid-identifier
                                    "Layer hold target is not a valid identifier"
                                    (append context (list :layer name)))))
                       :continuous)
                     ((simultaneous-action-p action)
                      (let ((actions (simultaneous-action-actions action)))
                        (when (< (length actions) 2)
                          (apply #'report :simultaneous-too-few-actions
                                 "SIMULTANEOUS requires at least two actions" context))
                        (loop for child in actions
                              for index from 0
                              for kind = (validate-action
                                          child (append context
                                                        (list :simultaneous index)))
                              unless (eq kind :continuous)
                                do (apply #'report :simultaneous-action-type
                                          "SIMULTANEOUS accepts only continuous actions"
                                          (append context (list :action-index index))))
                        :continuous))
                     ((tap-hold-action-p action)
                      (let ((instant-kind
                              (validate-action
                               (tap-hold-action-instant action)
                               (append context '(:tap-hold :instant))))
                            (continuous-kind
                              (validate-action
                               (tap-hold-action-continuous action)
                               (append context '(:tap-hold :continuous)))))
                        (unless (member instant-kind '(:instant :neutral) :test #'eq)
                          (apply #'report :tap-hold-instant-type
                                 "TAP-HOLD instant side must be instant or neutral"
                                 context))
                        (unless (eq continuous-kind :continuous)
                          (apply #'report :tap-hold-continuous-type
                                 "TAP-HOLD continuous side must be continuous"
                                 context))
                        :dual))
                     ((or (none-action-p action) (transparent-action-p action))
                      :neutral)
                     (t
                      (apply #'report :invalid-action "Expected a concrete action" context)
                      :invalid)))
                 (validate-bindings (bindings owner owner-name owner-index combo-p)
                   (let ((seen (make-hash-table :test #'equal)))
                     (loop for binding in bindings
                           for binding-index from 0
                           for layer = (binding-layer binding)
                           for level = (binding-level binding)
                           for context = (list owner owner-name
                                               (intern (format nil "~A-INDEX"
                                                               (symbol-name owner))
                                                       :keyword)
                                               owner-index
                                               :binding-index binding-index)
                           for layer-valid-p = (valid-identifier-p layer)
                           for level-valid-p = (valid-identifier-p level)
                           do (unless layer-valid-p
                                (apply #'report :invalid-identifier
                                       "Binding layer is not a valid identifier"
                                       (append context (list :layer layer))))
                               (unless level-valid-p
                                 (apply #'report :invalid-identifier
                                        "Binding level is not a valid identifier"
                                        (append context (list :level level))))
                               (when layer-valid-p
                                 (unless (identifier-table-value layer layer-table)
                                   (apply #'report :unknown-binding-layer
                                          "Binding references an undeclared layer"
                                          (append context (list :layer layer)))))
                               (when level-valid-p
                                 (unless (identifier-table-value level level-table)
                                   (apply #'report :unknown-binding-level
                                          "Binding references an undeclared level"
                                          (append context (list :level level)))))
                               (when (and layer-valid-p level-valid-p)
                                 (let ((address (list (identifier-key layer)
                                                      (identifier-key level))))
                                   (if (gethash address seen)
                                       (apply #'report
                                              (if combo-p :duplicate-combo-binding
                                                  :duplicate-key-binding)
                                              "Binding address must be unique"
                                              (append context (list :layer layer :level level)))
                                       (setf (gethash address seen) t))))
                               (validate-action (binding-action binding) context)))))
          (loop for key in keys for index from 0
                do (validate-bindings (abstract-key-bindings key) :key
                                      (abstract-key-name key) index nil))

          (let ((seen-combos (make-hash-table :test #'equal)))
            (loop for combo in combos
                  for combo-index from 0
                  for combo-keys = (combo-keys combo)
                   do (let ((members (make-identifier-table))
                           (distinct '())
                           (declared-distinct '()))
                         (loop for key-name in combo-keys
                               for key-index from 0
                                do (if (valid-identifier-p key-name)
                                       (progn
                                         (if (identifier-table-value key-name key-table)
                                             (pushnew key-name declared-distinct
                                                      :test #'identifier-equal-p)
                                             (report :unknown-combo-key
                                                     "Combo references an undeclared key"
                                                     :combo-index combo-index :key-index key-index
                                                     :key key-name))
                                         (if (identifier-table-value key-name members)
                                             (report :duplicate-combo-key
                                                     "Combo keys must be distinct"
                                                     :combo-index combo-index :key-index key-index
                                                     :key key-name)
                                             (progn (setf (identifier-table-value key-name members) t)
                                                    (push key-name distinct))))
                                       (report :invalid-identifier
                                               "Combo key is not a valid identifier"
                                               :combo-index combo-index :key-index key-index
                                               :key key-name)))
                       (when (< (length declared-distinct) 2)
                         (report :combo-too-few-keys
                                 "Combo must contain at least two distinct keys"
                                 :combo-index combo-index))
                        (let* ((canonical (sort (copy-list distinct) #'string<
                                                :key #'symbol-name))
                               (canonical-key (mapcar #'identifier-key canonical)))
                          (if (gethash canonical-key seen-combos)
                             (report :duplicate-combo
                                     "Combo duplicates an earlier key set"
                                     :combo-index combo-index :keys canonical)
                              (setf (gethash canonical-key seen-combos) t)))
                        (validate-bindings (combo-bindings combo) :combo combo-keys
                                           combo-index t))))))
      (let ((ordered (nreverse diagnostics)))
        (values ordered
                (notany (lambda (diagnostic)
                          (eq :error (diagnostic-severity diagnostic)))
                         ordered))))))
