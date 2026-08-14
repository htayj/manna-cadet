;;;; ir.lisp - pure abstract keyboard-layout intermediate representation

(in-package #:manna-cadet)

(defun identifier-equal-p (left right)
  (and left right (symbolp left) (symbolp right)
       (string= (symbol-name left) (symbol-name right))))

(defun identifier-key (identifier)
  (symbol-name identifier))

(defun make-identifier-table ()
  (make-hash-table :test #'equal))

(defun identifier-table-value (identifier table)
  (gethash (identifier-key identifier) table))

(defun (setf identifier-table-value) (value identifier table)
  (setf (gethash (identifier-key identifier) table) value))

(defstruct modifier
  (name nil :type symbol :read-only t))

(defstruct dimension
  (name nil :type symbol :read-only t)
  (states '() :type list :read-only t)
  (default nil :type symbol :read-only t))

(defstruct layout-symbol
  (name nil :type symbol :read-only t)
  (display nil :type (or null string) :read-only t))

(defstruct command
  (name nil :type symbol :read-only t))

(defstruct level
  (name nil :type symbol :read-only t)
  (selectors '() :type list :read-only t)
  (fallback nil :type (or null symbol) :read-only t))

(defstruct layer
  (name nil :type symbol :read-only t))

(defstruct action)

(defstruct (symbol-action (:include action))
  (symbol nil :type symbol :read-only t))

(defstruct (command-action (:include action))
  (command nil :type symbol :read-only t))

(defstruct (mods-action (:include action))
  (modifiers '() :type list :read-only t))

(defstruct (hold-selector-action (:include action))
  (dimension nil :type symbol :read-only t)
  (state nil :type symbol :read-only t))

(defstruct (layer-hold-action (:include action))
  (layer nil :type symbol :read-only t))

(defstruct (simultaneous-action (:include action))
  (actions '() :type list :read-only t))

(defstruct (tap-hold-action (:include action))
  (instant nil :type (or null action) :read-only t)
  (continuous nil :type (or null action) :read-only t))

(defstruct (none-action (:include action)))

(defstruct (transparent-action (:include action)))

(defstruct binding
  (layer nil :type symbol :read-only t)
  (level nil :type symbol :read-only t)
  (action nil :type (or null action) :read-only t))

(defstruct abstract-key
  (name nil :type symbol :read-only t)
  (bindings '() :type list :read-only t))

(defstruct combo
  (keys '() :type list :read-only t)
  (bindings '() :type list :read-only t))

(defstruct layout
  (name nil :type symbol :read-only t)
  (levels '() :type list :read-only t)
  (layers '() :type list :read-only t)
  (root-layer nil :type symbol :read-only t)
  (modifiers '() :type list :read-only t)
  (dimensions '() :type list :read-only t)
  (symbols '() :type list :read-only t)
  (commands '() :type list :read-only t)
  (keys '() :type list :read-only t)
  (combos '() :type list :read-only t))

(defun contextual-action-at (bindings requested-level layout active-layers)
  (let* ((levels (layout-levels layout))
         (root (layout-root-layer layout))
         (level-name (if (level-p requested-level)
                         (level-name requested-level)
                         requested-level))
         (layer-names
           (mapcar (lambda (item) (if (layer-p item) (layer-name item) item))
                   active-layers))
         (search-layers
           (loop with seen = '()
                 for layer in layer-names
                  unless (or (identifier-equal-p layer root)
                             (member layer seen :test #'identifier-equal-p))
                   do (push layer seen)
                   and collect layer into non-root-layers
                 finally (return (append non-root-layers (list root))))))
    (labels ((declared-level (name)
               (find name levels :key #'level-name :test #'identifier-equal-p))
             (binding-at (layer level)
               (find-if (lambda (binding)
                           (and (identifier-equal-p layer (binding-layer binding))
                                (identifier-equal-p level (binding-level binding))))
                        bindings))
             (resolve-layer (layer)
               (let ((seen '())
                     (current level-name))
                  (loop while (and current
                                   (not (member current seen
                                                :test #'identifier-equal-p)))
                       do (push current seen)
                          (let ((binding (binding-at layer current)))
                            (when binding
                              (let ((action (binding-action binding)))
                                (cond
                                  ((none-action-p action)
                                   (return-from resolve-layer (values nil t nil)))
                                  ((transparent-action-p action)
                                   (return-from resolve-layer (values nil nil t)))
                                  (t
                                   (return-from resolve-layer
                                     (values action t nil)))))))
                          (let ((level (declared-level current)))
                            (setf current (and level (level-fallback level)))))
                 (values nil nil nil))))
      (dolist (layer search-layers (values nil nil))
        (multiple-value-bind (action resolved-p transparent-p)
            (resolve-layer layer)
          (declare (ignore transparent-p))
          (when resolved-p
            (return (values action t))))))))

(defun key-action-at (key requested-level layout &optional (active-layers '()))
  "Resolve KEY in top-to-bottom active non-root layers, appending root once."
  (contextual-action-at (abstract-key-bindings key) requested-level layout
                        active-layers))

(defun combo-action-at (combo requested-level layout &optional (active-layers '()))
  "Resolve COMBO in top-to-bottom active non-root layers, appending root once."
  (contextual-action-at (combo-bindings combo) requested-level layout active-layers))
