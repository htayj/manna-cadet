;;;; normalize.lisp - canonical serialization and abstract layout inspection

(in-package #:manna-cadet)

(defun identifier-list-equal-p (left right)
  (and (= (length left) (length right))
       (every #'identifier-equal-p left right)))

(defun action-equal-p (left right)
  (cond
    ((symbol-action-p left)
     (and (symbol-action-p right)
          (identifier-equal-p (symbol-action-symbol left)
                              (symbol-action-symbol right))))
    ((command-action-p left)
     (and (command-action-p right)
          (identifier-equal-p (command-action-command left)
                              (command-action-command right))))
    ((mods-action-p left)
     (and (mods-action-p right)
          (identifier-list-equal-p (mods-action-modifiers left)
                                   (mods-action-modifiers right))))
    ((hold-selector-action-p left)
     (and (hold-selector-action-p right)
          (identifier-equal-p (hold-selector-action-dimension left)
                              (hold-selector-action-dimension right))
          (identifier-equal-p (hold-selector-action-state left)
                              (hold-selector-action-state right))))
    ((layer-hold-action-p left)
     (and (layer-hold-action-p right)
          (identifier-equal-p (layer-hold-action-layer left)
                              (layer-hold-action-layer right))))
    ((simultaneous-action-p left)
     (and (simultaneous-action-p right)
          (= (length (simultaneous-action-actions left))
             (length (simultaneous-action-actions right)))
          (every #'action-equal-p
                 (simultaneous-action-actions left)
                 (simultaneous-action-actions right))))
    ((tap-hold-action-p left)
     (and (tap-hold-action-p right)
          (action-equal-p (tap-hold-action-instant left)
                          (tap-hold-action-instant right))
          (action-equal-p (tap-hold-action-continuous left)
                          (tap-hold-action-continuous right))))
    ((none-action-p left) (none-action-p right))
    ((transparent-action-p left) (transparent-action-p right))
    (t nil)))

(defun selector-equal-p (left right)
  (and (identifier-equal-p (car left) (car right))
       (identifier-equal-p (cdr left) (cdr right))))

(defun binding-equal-p (left right)
  (and (identifier-equal-p (binding-layer left) (binding-layer right))
       (identifier-equal-p (binding-level left) (binding-level right))
       (action-equal-p (binding-action left) (binding-action right))))

(defun ordered-list-equal-p (left right predicate)
  (and (= (length left) (length right))
       (every predicate left right)))

(defun canonical-layer-names (layout)
  (let ((root (layout-root-layer layout)))
    (append (when root (list root))
            (loop for layer in (layout-layers layout)
                  for name = (layer-name layer)
                  unless (and root (identifier-equal-p name root))
                    collect name))))

(defun layout-equal-p (left right)
  "Return true when two layouts have the same ordered abstract semantics."
  (labels ((named-list-equal-p (left-items right-items accessor)
             (ordered-list-equal-p
              left-items right-items
              (lambda (left-item right-item)
                (identifier-equal-p (funcall accessor left-item)
                                    (funcall accessor right-item)))))
           (dimension-equal-p (left-dimension right-dimension)
             (and (identifier-equal-p (dimension-name left-dimension)
                                      (dimension-name right-dimension))
                  (identifier-list-equal-p (dimension-states left-dimension)
                                           (dimension-states right-dimension))
                  (identifier-equal-p (dimension-default left-dimension)
                                      (dimension-default right-dimension))))
           (level-equal-p (left-level right-level)
             (and (identifier-equal-p (level-name left-level)
                                      (level-name right-level))
                  (ordered-list-equal-p (level-selectors left-level)
                                        (level-selectors right-level)
                                        #'selector-equal-p)
                  (if (level-fallback left-level)
                      (and (level-fallback right-level)
                           (identifier-equal-p (level-fallback left-level)
                                               (level-fallback right-level)))
                      (null (level-fallback right-level)))))
           (layout-symbol-equal-p (left-symbol right-symbol)
             (and (identifier-equal-p (layout-symbol-name left-symbol)
                                      (layout-symbol-name right-symbol))
                  (equal (layout-symbol-display left-symbol)
                         (layout-symbol-display right-symbol))))
           (key-equal-p (left-key right-key)
             (and (identifier-equal-p (abstract-key-name left-key)
                                      (abstract-key-name right-key))
                  (ordered-list-equal-p (abstract-key-bindings left-key)
                                        (abstract-key-bindings right-key)
                                        #'binding-equal-p)))
           (combo-equal-p (left-combo right-combo)
             (and (identifier-list-equal-p (combo-keys left-combo)
                                           (combo-keys right-combo))
                  (ordered-list-equal-p (combo-bindings left-combo)
                                        (combo-bindings right-combo)
                                        #'binding-equal-p))))
    (and (layout-p left)
         (layout-p right)
         (identifier-equal-p (layout-name left) (layout-name right))
         (ordered-list-equal-p (layout-dimensions left)
                               (layout-dimensions right)
                               #'dimension-equal-p)
         (ordered-list-equal-p (layout-levels left) (layout-levels right)
                               #'level-equal-p)
          (identifier-list-equal-p (canonical-layer-names left)
                                   (canonical-layer-names right))
         (if (layout-root-layer left)
             (and (layout-root-layer right)
                  (identifier-equal-p (layout-root-layer left)
                                      (layout-root-layer right)))
             (null (layout-root-layer right)))
         (named-list-equal-p (layout-modifiers left) (layout-modifiers right)
                             #'modifier-name)
         (named-list-equal-p (layout-commands left) (layout-commands right)
                             #'command-name)
         (ordered-list-equal-p (layout-symbols left) (layout-symbols right)
                               #'layout-symbol-equal-p)
         (ordered-list-equal-p (layout-keys left) (layout-keys right)
                               #'key-equal-p)
         (ordered-list-equal-p (layout-combos left) (layout-combos right)
                               #'combo-equal-p))))

(defun write-escaped-character (character delimiter stream)
  (let ((code (char-code character)))
    (cond
      ((unsafe-text-code-point-p code)
       (format stream "\\x~X;" code))
      ((or (char= character #\\) (char= character delimiter))
       (write-char #\\ stream)
       (write-char character stream))
      (t
       (write-char character stream)))))

(defun write-identifier (identifier stream)
  (write-char #\| stream)
  (loop for character across (symbol-name identifier)
        do (write-escaped-character character #\| stream))
  (write-char #\| stream))

(defun write-data-string (value stream)
  (write-char #\" stream)
  (loop for character across value
        do (write-escaped-character character #\" stream))
  (write-char #\" stream))

(defun write-identifiers (identifiers stream)
  (loop for identifier in identifiers
        for first-p = t then nil
        do (unless first-p (write-char #\Space stream))
           (write-identifier identifier stream)))

(defun write-action (action stream)
  (cond
    ((symbol-action-p action)
     (write-string "(symbol " stream)
     (write-identifier (symbol-action-symbol action) stream)
     (write-char #\) stream))
    ((command-action-p action)
     (write-string "(command " stream)
     (write-identifier (command-action-command action) stream)
     (write-char #\) stream))
    ((mods-action-p action)
     (write-string "(mods" stream)
     (when (mods-action-modifiers action)
       (write-char #\Space stream)
       (write-identifiers (mods-action-modifiers action) stream))
     (write-char #\) stream))
    ((hold-selector-action-p action)
     (write-string "(hold-selector " stream)
     (write-identifier (hold-selector-action-dimension action) stream)
     (write-char #\Space stream)
     (write-identifier (hold-selector-action-state action) stream)
     (write-char #\) stream))
    ((layer-hold-action-p action)
     (write-string "(layer-hold " stream)
     (write-identifier (layer-hold-action-layer action) stream)
     (write-char #\) stream))
    ((simultaneous-action-p action)
     (write-string "(simultaneous" stream)
     (dolist (child (simultaneous-action-actions action))
       (write-char #\Space stream)
       (write-action child stream))
     (write-char #\) stream))
    ((tap-hold-action-p action)
     (write-string "(tap-hold " stream)
     (write-action (tap-hold-action-instant action) stream)
     (write-char #\Space stream)
     (write-action (tap-hold-action-continuous action) stream)
     (write-char #\) stream))
    ((none-action-p action) (write-string "none" stream))
    ((transparent-action-p action) (write-string "transparent" stream))
    (t (error "Cannot normalize unknown action ~S" action))))

(defun write-normalized-layout (layout stream)
  (write-string "(layout " stream)
  (write-identifier (layout-name layout) stream)
  (terpri stream)
  (write-string "  (dimensions" stream)
  (dolist (dimension (layout-dimensions layout))
    (terpri stream)
    (write-string "    (" stream)
    (write-identifier (dimension-name dimension) stream)
    (write-string " (default " stream)
    (write-identifier (dimension-default dimension) stream)
    (write-string ") (states" stream)
    (when (dimension-states dimension)
      (write-char #\Space stream)
      (write-identifiers (dimension-states dimension) stream))
    (write-string "))" stream))
  (write-char #\) stream)
  (terpri stream)
  (write-string "  (levels" stream)
  (dolist (level (layout-levels layout))
    (terpri stream)
    (write-string "    (" stream)
    (write-identifier (level-name level) stream)
    (write-string " (selectors" stream)
    (dolist (selector (level-selectors level))
      (write-string " (" stream)
      (write-identifier (car selector) stream)
      (write-char #\Space stream)
      (write-identifier (cdr selector) stream)
      (write-char #\) stream))
    (write-char #\) stream)
    (when (level-fallback level)
      (write-string " (fallback " stream)
      (write-identifier (level-fallback level) stream)
      (write-char #\) stream))
    (write-char #\) stream))
  (write-char #\) stream)
  (when (layout-layers layout)
    (terpri stream)
    (write-string "  (layers" stream)
    (loop for name in (canonical-layer-names layout)
          for root-p = t then nil
          do (if root-p
                 (write-string " (root " stream)
                 (write-string " (layer " stream))
             (write-identifier name stream)
             (write-char #\) stream))
    (write-char #\) stream))
  (terpri stream)
  (write-string "  (modifiers (" stream)
  (write-identifiers (mapcar #'modifier-name (layout-modifiers layout)) stream)
  (write-string "))" stream)
  (terpri stream)
  (write-string "  (commands (" stream)
  (write-identifiers (mapcar #'command-name (layout-commands layout)) stream)
  (write-string "))" stream)
  (terpri stream)
  (write-string "  (symbols" stream)
  (dolist (layout-symbol (layout-symbols layout))
    (terpri stream)
    (write-string "    " stream)
    (if (layout-symbol-display layout-symbol)
        (progn
          (write-char #\( stream)
          (write-identifier (layout-symbol-name layout-symbol) stream)
          (write-char #\Space stream)
           (write-data-string (layout-symbol-display layout-symbol) stream)
          (write-char #\) stream))
        (write-identifier (layout-symbol-name layout-symbol) stream)))
  (write-char #\) stream)
  (terpri stream)
  (write-string "  (keys" stream)
  (dolist (key (layout-keys layout))
    (terpri stream)
    (write-string "    (key " stream)
    (write-identifier (abstract-key-name key) stream)
    (dolist (binding (abstract-key-bindings key))
      (terpri stream)
      (write-string "      ((" stream)
      (write-identifier (binding-layer binding) stream)
      (write-char #\Space stream)
      (write-identifier (binding-level binding) stream)
      (write-string ") " stream)
      (write-action (binding-action binding) stream)
      (write-char #\) stream))
    (write-char #\) stream))
  (write-char #\) stream)
  (terpri stream)
  (write-string "  (combos" stream)
  (dolist (combo (layout-combos layout))
    (terpri stream)
    (write-string "    (combo (" stream)
    (write-identifiers (combo-keys combo) stream)
    (write-char #\) stream)
    (dolist (binding (combo-bindings combo))
      (terpri stream)
      (write-string "      ((" stream)
      (write-identifier (binding-layer binding) stream)
      (write-char #\Space stream)
      (write-identifier (binding-level binding) stream)
      (write-string ") " stream)
      (write-action (binding-action binding) stream)
      (write-char #\) stream))
    (write-char #\) stream))
  (write-char #\) stream)
  (terpri stream)
  (write-char #\) stream)
  (terpri stream))

(defun normalize-layout (layout &optional (stream nil stream-supplied-p))
  "Return canonical DSL text, or write it to STREAM and return LAYOUT."
  (if stream-supplied-p
      (progn (write-normalized-layout layout stream) layout)
      (with-output-to-string (output)
        (write-normalized-layout layout output))))

(defvar *inspection-resolution-computation-count* nil)

(defstruct inspection-layout-index
  level-table
  root
  contexts)

(defstruct inspection-layer-resolution
  source
  transparent-p)

(defstruct inspection-resolution
  action
  resolved-p
  source
  fallback-p
  transparent-addresses)

(defstruct inspection-owner-index
  layout-index
  binding-table
  memo-table)

(defun build-inspection-layout-index (layout context-names)
  (let ((level-table (make-identifier-table))
        (root (layout-root-layer layout)))
    (dolist (level (layout-levels layout))
      (multiple-value-bind (existing present-p)
          (gethash (identifier-key (level-name level)) level-table)
        (declare (ignore existing))
        (unless present-p
          (setf (identifier-table-value (level-name level) level-table) level))))
    (make-inspection-layout-index
     :level-table level-table
     :root root
     :contexts
     (mapcar (lambda (name)
               (cons name (if (identifier-equal-p name root)
                              (list root)
                              (list name root))))
             context-names))))

(defun build-inspection-owner-index (bindings layout-index)
  (let ((binding-table (make-identifier-table)))
    (dolist (binding bindings)
      (let* ((layer (binding-layer binding))
             (level (binding-level binding))
             (layer-table
               (or (identifier-table-value layer binding-table)
                   (setf (identifier-table-value layer binding-table)
                         (make-identifier-table)))))
        (multiple-value-bind (existing present-p)
            (gethash (identifier-key level) layer-table)
          (declare (ignore existing))
          (unless present-p
            (setf (identifier-table-value level layer-table) binding)))))
    (make-inspection-owner-index
     :layout-index layout-index
     :binding-table binding-table
     :memo-table (make-identifier-table))))

(defun inspection-layer-table (layer table)
  (or (identifier-table-value layer table)
      (setf (identifier-table-value layer table) (make-identifier-table))))

(defun resolve-inspection-layer (owner-index layer requested-level)
  (let* ((memo-table
           (inspection-layer-table layer
                                   (inspection-owner-index-memo-table owner-index)))
         (binding-table
           (identifier-table-value
            layer (inspection-owner-index-binding-table owner-index)))
         (level-table
           (inspection-layout-index-level-table
            (inspection-owner-index-layout-index owner-index))))
    (multiple-value-bind (cached present-p)
        (gethash (identifier-key requested-level) memo-table)
      (when present-p
        (return-from resolve-inspection-layer cached)))
    (let ((current requested-level)
          (path '())
          (seen (make-identifier-table))
          (result nil))
      (loop while current
            do (multiple-value-bind (cached present-p)
                   (gethash (identifier-key current) memo-table)
                 (when present-p
                   (setf result cached)
                   (return)))
               (when (identifier-table-value current seen)
                 (return))
               (setf (identifier-table-value current seen) t)
               (push current path)
               (when *inspection-resolution-computation-count*
                 (incf *inspection-resolution-computation-count*))
               (let ((binding
                       (and binding-table
                            (identifier-table-value current binding-table))))
                 (when binding
                   (setf result
                         (make-inspection-layer-resolution
                          :source binding
                          :transparent-p
                          (transparent-action-p (binding-action binding))))
                   (return)))
               (let ((level (identifier-table-value current level-table)))
                 (setf current (and level (level-fallback level)))))
      (dolist (level-name path)
        (setf (gethash (identifier-key level-name) memo-table) result))
      result)))

(defun resolve-inspection-row (owner-index requested-level search-layers)
  (let ((transparent-addresses '()))
    (dolist (layer search-layers)
      (let ((layer-resolution
              (resolve-inspection-layer owner-index layer requested-level)))
        (when layer-resolution
          (let ((source (inspection-layer-resolution-source layer-resolution)))
            (if (inspection-layer-resolution-transparent-p layer-resolution)
                (push (cons (binding-layer source) (binding-level source))
                      transparent-addresses)
                (return-from resolve-inspection-row
                  (make-inspection-resolution
                   :action (binding-action source)
                   :resolved-p t
                   :source source
                   :fallback-p
                   (not (identifier-equal-p requested-level
                                            (binding-level source)))
                   :transparent-addresses
                   (nreverse transparent-addresses))))))))
    (make-inspection-resolution
     :transparent-addresses (nreverse transparent-addresses))))

(defun action-description (action)
  (labels ((identifier (value) (inspection-identifier value))
           (describe-actions (actions)
             (format nil "(~{~A~^; ~})" (mapcar #'describe-action actions)))
           (describe-action (value)
             (cond
               ((symbol-action-p value)
                (format nil "SYMBOL ~A" (identifier (symbol-action-symbol value))))
               ((command-action-p value)
                (format nil "COMMAND ~A" (identifier (command-action-command value))))
               ((mods-action-p value)
                (format nil "MODS (~{~A~^ ~})"
                        (mapcar #'identifier (mods-action-modifiers value))))
               ((hold-selector-action-p value)
                (format nil "HOLD-SELECTOR ~A ~A"
                        (identifier (hold-selector-action-dimension value))
                        (identifier (hold-selector-action-state value))))
               ((layer-hold-action-p value)
                (format nil "LAYER-HOLD ~A"
                        (identifier (layer-hold-action-layer value))))
               ((simultaneous-action-p value)
                (format nil "SIMULTANEOUS ~A"
                        (describe-actions (simultaneous-action-actions value))))
               ((tap-hold-action-p value)
                (format nil "TAP-HOLD (~A; ~A)"
                        (describe-action (tap-hold-action-instant value))
                        (describe-action (tap-hold-action-continuous value))))
               ((none-action-p value) "NONE")
               ((transparent-action-p value) "TRANSPARENT")
               (t "UNRESOLVED"))))
    (describe-action action)))

(defun inspection-identifier (identifier)
  "Escape unsafe identifier characters with \\xHEX; style for one-line output."
  (with-output-to-string (stream)
    (write-char #\| stream)
    (loop for character across (symbol-name identifier)
          for code = (char-code character)
          do (cond
               ((or (char= character #\Newline)
                    (char= character #\Return)
                    (char= character #\Tab))
                (format stream "\\~A" (case character
                                        (#\Newline "n")
                                        (#\Return "r")
                                        (#\Tab "t"))))
               ((unsafe-text-code-point-p code)
                (format stream "\\x~X;" code))
               (t
                (when (or (char= character #\\) (char= character #\|))
                  (write-char #\\ stream))
                (write-char character stream))))
    (write-char #\| stream)))

(defconstant +inspection-maximum-rows+ 50000
  "Maximum number of rows produced by one inspection operation.")

(defconstant +inspection-maximum-estimated-bytes+ (* 4 1024 1024)
  "Maximum conservative UTF-8 output estimate for one inspection operation.")

(define-condition inspection-limit-exceeded (error)
  ((kind :initarg :kind :reader inspection-limit-exceeded-kind)
   (estimate :initarg :estimate :reader inspection-limit-exceeded-estimate)
   (limit :initarg :limit :reader inspection-limit-exceeded-limit))
  (:report
   (lambda (condition stream)
     (format stream "Inspection limit exceeded: ~A estimate ~D exceeds ~D"
             (ecase (inspection-limit-exceeded-kind condition)
               (:rows "row")
               (:estimated-bytes "output byte"))
             (inspection-limit-exceeded-estimate condition)
             (inspection-limit-exceeded-limit condition)))))

(defun hexadecimal-digit-count (value)
  (max 1 (ceiling (integer-length value) 4)))

(defun utf-8-code-point-length (code)
  (cond
    ((<= code #x7F) 1)
    ((<= code #x7FF) 2)
    ((<= code #xFFFF) 3)
    (t 4)))

(defun escaped-character-byte-length (character delimiter)
  (let ((code (char-code character)))
    (cond
      ((unsafe-text-code-point-p code)
       (+ 3 (hexadecimal-digit-count code)))
      ((or (char= character #\\) (char= character delimiter))
       (1+ (utf-8-code-point-length code)))
      (t
       (utf-8-code-point-length code)))))

(defun inspection-identifier-byte-length (identifier)
  (+ 2 (loop for character across (symbol-name identifier)
             sum (escaped-character-byte-length character #\|))))

(defun action-description-byte-length (action)
  (labels ((joined-length (actions)
             (+ 2
                (loop for child in actions
                      for first-p = t then nil
                      sum (+ (if first-p 0 2) (description-length child)))))
           (identifier-length (identifier)
             (inspection-identifier-byte-length identifier))
           (description-length (value)
             (cond
               ((symbol-action-p value)
                (+ (length "SYMBOL ")
                   (identifier-length (symbol-action-symbol value))))
               ((command-action-p value)
                (+ (length "COMMAND ")
                   (identifier-length (command-action-command value))))
               ((mods-action-p value)
                (+ (length "MODS ()")
                   (loop for modifier in (mods-action-modifiers value)
                         for first-p = t then nil
                         sum (+ (if first-p 0 1)
                                (identifier-length modifier)))))
               ((hold-selector-action-p value)
                (+ (length "HOLD-SELECTOR  ")
                   (identifier-length (hold-selector-action-dimension value))
                   (identifier-length (hold-selector-action-state value))))
               ((layer-hold-action-p value)
                (+ (length "LAYER-HOLD ")
                   (identifier-length (layer-hold-action-layer value))))
               ((simultaneous-action-p value)
                (+ (length "SIMULTANEOUS ")
                   (joined-length (simultaneous-action-actions value))))
               ((tap-hold-action-p value)
                (+ (length "TAP-HOLD (; )")
                   (description-length (tap-hold-action-instant value))
                   (description-length (tap-hold-action-continuous value))))
               ((none-action-p value) (length "NONE"))
               ((transparent-action-p value) (length "TRANSPARENT"))
               (t (length "UNRESOLVED")))))
    (description-length action)))

(defun inspection-owner-byte-length (owner)
  (if (combo-p owner)
      (+ 2
         (loop for key in (combo-keys owner)
               for first-p = t then nil
               sum (+ (if first-p 0 3)
                      (inspection-identifier-byte-length key))))
      (inspection-identifier-byte-length (abstract-key-name owner))))

(defun inspection-output-byte-estimate (layout contexts row-estimate)
  (let* ((levels (layout-levels layout))
         (keys (layout-keys layout))
         (combos (layout-combos layout))
         (owners (+ (length keys) (length combos)))
         (maximum-action-length (length "UNRESOLVED"))
         (maximum-layer-length
           (loop for layer in (layout-layers layout)
                 maximize (inspection-identifier-byte-length (layer-name layer))
                   into maximum
                 finally (return (or maximum 2))))
         (maximum-level-length
           (loop for level in levels
                 maximize (inspection-identifier-byte-length (level-name level))
                   into maximum
                 finally (return (or maximum 2))))
         (provenance-length
           (+ (length " | FALLBACK ") maximum-layer-length 1 maximum-level-length
              (* 2 (+ (length " | AFTER TRANSPARENT ")
                      maximum-layer-length 1 maximum-level-length)))))
    (labels ((note-action-length (action)
               (setf maximum-action-length
                     (max maximum-action-length
                          (action-description-byte-length action)))
               (let ((lower-bound (* row-estimate maximum-action-length)))
                 (when (> lower-bound +inspection-maximum-estimated-bytes+)
                   (error 'inspection-limit-exceeded
                          :kind :estimated-bytes :estimate lower-bound
                          :limit +inspection-maximum-estimated-bytes+)))))
      (dolist (key keys)
        (dolist (binding (abstract-key-bindings key))
          (note-action-length (binding-action binding))))
      (dolist (combo combos)
        (dolist (binding (combo-bindings combo))
          (note-action-length (binding-action binding)))))
    (let* ((level-length-sum
             (loop for level in levels
                   sum (inspection-identifier-byte-length (level-name level))))
           (owner-base-sum
             (+ (loop for key in keys
                      sum (+ (length "KEY ")
                             (inspection-owner-byte-length key)
                             (length " | LEVEL  | ")
                             maximum-action-length provenance-length 1))
                (loop for combo in combos
                      sum (+ (length "COMBO ")
                             (inspection-owner-byte-length combo)
                             (length " | LEVEL  | ")
                             maximum-action-length provenance-length 1))))
           (header-length
             (+ (length "LAYOUT ")
                (inspection-identifier-byte-length (layout-name layout)) 1
                (loop for context in contexts
                      for root-p = (identifier-equal-p
                                    context (layout-root-layer layout))
                      sum (+ (length "CONTEXT ")
                             (inspection-identifier-byte-length context)
                             (if root-p
                                 (length " (ROOT)\n")
                                 (+ (length " (OVER )\n")
                                    (inspection-identifier-byte-length
                                     (layout-root-layer layout))))))))
           (rows-per-context
             (+ (* (length levels) owner-base-sum)
                (* owners level-length-sum))))
      (+ header-length (* (length contexts) rows-per-context)))))

(defun ensure-inspection-budget (layout)
  (let* ((contexts (canonical-layer-names layout))
         (row-estimate (* (length contexts)
                          (+ (length (layout-keys layout))
                             (length (layout-combos layout)))
                          (length (layout-levels layout)))))
    (when (> row-estimate +inspection-maximum-rows+)
      (error 'inspection-limit-exceeded
             :kind :rows :estimate row-estimate
             :limit +inspection-maximum-rows+))
    (let ((byte-estimate
            (inspection-output-byte-estimate layout contexts row-estimate)))
      (when (> byte-estimate +inspection-maximum-estimated-bytes+)
        (error 'inspection-limit-exceeded
               :kind :estimated-bytes :estimate byte-estimate
               :limit +inspection-maximum-estimated-bytes+)))
    contexts))

(defun write-inspection-row (kind owner owner-index level search-layers stream)
  (let* ((level-name (level-name level))
         (resolution
           (resolve-inspection-row owner-index level-name search-layers))
         (source (inspection-resolution-source resolution)))
    (format stream "~A ~A | LEVEL ~A | ~A"
            kind
            (if (combo-p owner)
                (format nil "(~{~A~^ + ~})"
                        (mapcar #'inspection-identifier (combo-keys owner)))
                (inspection-identifier (abstract-key-name owner)))
            (inspection-identifier level-name)
            (if (inspection-resolution-resolved-p resolution)
                (action-description (inspection-resolution-action resolution))
                "UNRESOLVED"))
    (when source
      (format stream " | ~A ~A/~A"
              (if (inspection-resolution-fallback-p resolution)
                  "FALLBACK"
                  "DIRECT")
              (inspection-identifier (binding-layer source))
              (inspection-identifier (binding-level source))))
    (dolist (address (inspection-resolution-transparent-addresses resolution))
      (format stream " | AFTER TRANSPARENT ~A/~A"
              (inspection-identifier (car address))
              (inspection-identifier (cdr address))))
    (terpri stream)))

(defun write-layout-inspection (layout stream)
  (let* ((context-names (ensure-inspection-budget layout))
         (layout-index (build-inspection-layout-index layout context-names))
         (key-indexes
           (mapcar (lambda (key)
                     (cons key
                           (build-inspection-owner-index
                            (abstract-key-bindings key) layout-index)))
                   (layout-keys layout)))
         (combo-indexes
           (mapcar (lambda (combo)
                     (cons combo
                           (build-inspection-owner-index
                            (combo-bindings combo) layout-index)))
                   (layout-combos layout))))
    (format stream "LAYOUT ~A~%" (inspection-identifier (layout-name layout)))
    (dolist (context (inspection-layout-index-contexts layout-index))
      (let* ((context-name (car context))
             (search-layers (cdr context))
             (root-p (identifier-equal-p
                      context-name (inspection-layout-index-root layout-index))))
        (if root-p
            (format stream "CONTEXT ~A (ROOT)~%"
                    (inspection-identifier context-name))
            (format stream "CONTEXT ~A (OVER ~A)~%"
                    (inspection-identifier context-name)
                    (inspection-identifier (layout-root-layer layout))))
        (dolist (entry key-indexes)
          (dolist (level (layout-levels layout))
            (write-inspection-row "KEY" (car entry) (cdr entry) level
                                  search-layers stream)))
        (dolist (entry combo-indexes)
          (dolist (level (layout-levels layout))
            (write-inspection-row "COMBO" (car entry) (cdr entry) level
                                  search-layers stream)))))))

(defun inspect-layout (layout &optional (stream nil stream-supplied-p))
  "Return abstract resolved-action text, or write it to STREAM and return LAYOUT."
  (if stream-supplied-p
      (progn (write-layout-inspection layout stream) layout)
      (with-output-to-string (output)
        (write-layout-inspection layout output))))
