;;;; parse.lisp - structural parser for the abstract keyboard-layout language

(in-package #:manna-cadet)

(define-condition layout-parse-error (error)
  ((message :initarg :message :reader layout-parse-error-message)
   (context :initarg :context :initform nil :reader layout-parse-error-context))
  (:report
   (lambda (condition stream)
      (format stream "Layout parse error: ~A"
              (layout-parse-error-message condition)))))

(defun signal-layout-parse-error (message &optional context)
  (error 'layout-parse-error :message message :context context))

(defun token-named-p (token name)
  (and (symbolp token) (string= (symbol-name token) name)))

(defun require-name (value context)
  (unless (and (symbolp value) (not (null value)))
    (signal-layout-parse-error "Expected an identifier" context))
  value)

(defun require-names (values context)
  (mapcar (lambda (value) (require-name value context)) values))

(defun parse-action (form)
  (cond
    ((token-named-p form "none") (make-none-action))
    ((token-named-p form "transparent") (make-transparent-action))
    ((and (symbolp form) form) (make-symbol-action :symbol form))
    ((not (consp form)) (signal-layout-parse-error "Expected an action" form))
    ((token-named-p (first form) "symbol")
     (unless (= 2 (length form))
       (signal-layout-parse-error "SYMBOL action requires one identifier" form))
     (make-symbol-action :symbol (require-name (second form) form)))
    ((token-named-p (first form) "command")
     (unless (= 2 (length form))
       (signal-layout-parse-error "COMMAND action requires one identifier" form))
     (make-command-action :command (require-name (second form) form)))
    ((token-named-p (first form) "mods")
     (make-mods-action :modifiers (require-names (rest form) form)))
    ((token-named-p (first form) "hold-selector")
      (unless (= 3 (length form))
        (signal-layout-parse-error
         "HOLD-SELECTOR action requires a dimension and state" form))
      (make-hold-selector-action :dimension (require-name (second form) form)
                                 :state (require-name (third form) form)))
    ((token-named-p (first form) "layer-hold")
      (unless (= 2 (length form))
        (signal-layout-parse-error "LAYER-HOLD action requires one layer" form))
      (make-layer-hold-action :layer (require-name (second form) form)))
    ((token-named-p (first form) "simultaneous")
      (make-simultaneous-action :actions (mapcar #'parse-action (rest form))))
    ((token-named-p (first form) "tap-hold")
      (unless (= 3 (length form))
        (signal-layout-parse-error
         "TAP-HOLD action requires instant and continuous actions" form))
      (make-tap-hold-action :instant (parse-action (second form))
                            :continuous (parse-action (third form))))
    (t (signal-layout-parse-error "Unknown action form" form))))

(defun parse-dimension (form)
  (unless (and (listp form) (= 3 (length form)))
    (signal-layout-parse-error
     "Expected (DIM (default STATE) (states STATE...))" form))
  (destructuring-bind (name default-form states-form) form
    (unless (and (listp default-form) (= 2 (length default-form))
                 (token-named-p (first default-form) "default"))
      (signal-layout-parse-error "Expected (default STATE)" default-form))
    (unless (and (listp states-form) (consp states-form)
                 (token-named-p (first states-form) "states"))
      (signal-layout-parse-error "Expected (states STATE...)" states-form))
    (make-dimension :name (require-name name form)
                    :default (require-name (second default-form) default-form)
                    :states (require-names (rest states-form) states-form))))

(defun parse-selector (form)
  (unless (and (listp form) (= 2 (length form)))
    (signal-layout-parse-error "Expected (DIM STATE) selector assignment" form))
  (cons (require-name (first form) form)
        (require-name (second form) form)))

(defun parse-level (form)
  (unless (and (listp form) (<= 2 (length form) 3))
    (signal-layout-parse-error
     "Expected (LEVEL (selectors (DIM STATE)...) (fallback LEVEL)?)" form))
  (let ((name (require-name (first form) form))
        (selectors-form (second form))
        (fallback-form (third form)))
    (unless (and (listp selectors-form) (consp selectors-form)
                 (token-named-p (first selectors-form) "selectors"))
      (signal-layout-parse-error "Expected (selectors (DIM STATE)...)"
                                 selectors-form))
    (when (and fallback-form
               (not (and (listp fallback-form) (= 2 (length fallback-form))
                         (token-named-p (first fallback-form) "fallback"))))
      (signal-layout-parse-error "Expected (fallback LEVEL)" fallback-form))
    (make-level :name name
                :selectors (mapcar #'parse-selector (rest selectors-form))
                :fallback (and fallback-form
                               (require-name (second fallback-form)
                                             fallback-form)))))

(defun parse-symbol-item (form)
  (cond
    ((symbolp form) (make-layout-symbol :name (require-name form form)))
    ((and (listp form) (= 2 (length form))
          (symbolp (first form)) (stringp (second form)))
     (make-layout-symbol :name (require-name (first form) form)
                         :display (second form)))
    (t (signal-layout-parse-error "Expected a symbol name or (NAME display)" form))))

(defun parse-binding (form)
  (unless (and (listp form) (= 2 (length form))
               (listp (first form)) (= 2 (length (first form))))
    (signal-layout-parse-error "Expected ((LAYER LEVEL) ACTION) binding" form))
  (make-binding :layer (require-name (first (first form)) form)
                :level (require-name (second (first form)) form)
                :action (parse-action (second form))))

(defun parse-key (form)
  (unless (and (listp form) (>= (length form) 2)
               (token-named-p (first form) "key"))
    (signal-layout-parse-error
     "Expected (key NAME ((LAYER LEVEL) ACTION)...)" form))
  (make-abstract-key
   :name (require-name (second form) form)
   :bindings
    (mapcar #'parse-binding (cddr form))))

(defun parse-combo (form)
  (unless (and (listp form) (>= (length form) 2)
               (token-named-p (first form) "combo")
               (listp (second form)))
    (signal-layout-parse-error
     "Expected (combo (KEY KEY...) ((LAYER LEVEL) ACTION)...)" form))
  (make-combo :keys (require-names (second form) form)
              :bindings (mapcar #'parse-binding (cddr form))))

(defun parse-layers-section (section)
  (unless (and (>= (length section) 2)
               (listp (second section)) (= 2 (length (second section)))
               (token-named-p (first (second section)) "root"))
    (signal-layout-parse-error
     "Expected (layers (root NAME) (layer NAME)...)" section))
  (let ((root (require-name (second (second section)) (second section))))
    (values
     (cons (make-layer :name root)
           (mapcar
            (lambda (form)
              (unless (and (listp form) (= 2 (length form))
                           (token-named-p (first form) "layer"))
                (signal-layout-parse-error "Expected (layer NAME)" form))
              (make-layer :name (require-name (second form) form)))
            (cddr section)))
     root)))

(defun parse-name-list-section (section constructor)
  (unless (and (= 2 (length section)) (listp (second section)))
    (signal-layout-parse-error "Expected a parenthesized identifier list" section))
  (mapcar (lambda (name)
            (funcall constructor :name (require-name name section)))
          (second section)))

(defun parse-layout (form)
  (unless (and (listp form) (>= (length form) 2)
               (token-named-p (first form) "layout"))
    (signal-layout-parse-error "Expected (layout NAME SECTION...)" form))
  (let ((name (require-name (second form) form))
        (dimensions '())
         (levels '())
         (layers '())
         (root-layer nil)
        (modifiers '())
        (commands '())
        (symbols '())
        (keys '())
        (combos '()))
    (dolist (section (cddr form))
      (unless (and (listp section) (consp section) (symbolp (first section)))
        (signal-layout-parse-error "Expected a layout section" section))
      (cond
         ((token-named-p (first section) "dimensions")
          (setf dimensions (nconc dimensions
                                  (mapcar #'parse-dimension (rest section)))))
         ((token-named-p (first section) "levels")
          (setf levels (nconc levels (mapcar #'parse-level (rest section)))))
         ((token-named-p (first section) "layers")
          (when layers
            (signal-layout-parse-error "LAYERS section may appear only once" section))
          (multiple-value-setq (layers root-layer)
            (parse-layers-section section)))
        ((token-named-p (first section) "modifiers")
         (setf modifiers
               (nconc modifiers
                      (parse-name-list-section section #'make-modifier))))
        ((token-named-p (first section) "commands")
         (setf commands
               (nconc commands
                      (parse-name-list-section section #'make-command))))
        ((token-named-p (first section) "symbols")
         (setf symbols
               (nconc symbols (mapcar #'parse-symbol-item (rest section)))))
        ((token-named-p (first section) "keys")
         (setf keys (nconc keys (mapcar #'parse-key (rest section)))))
        ((token-named-p (first section) "combos")
         (setf combos (nconc combos (mapcar #'parse-combo (rest section)))))
        (t (signal-layout-parse-error "Unknown layout section" section))))
    (make-layout :name name
                 :dimensions dimensions
                  :levels levels
                  :layers layers
                  :root-layer root-layer
                 :modifiers modifiers
                 :commands commands
                 :symbols symbols
                 :keys keys
                 :combos combos)))

(defun read-layout (&optional (stream *standard-input*))
  "Read exactly one data-only layout form from STREAM and parse it into the IR."
  (parse-layout (read-layout-form stream)))
