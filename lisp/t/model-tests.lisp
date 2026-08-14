;;;; model-tests.lisp - regression tests for the Manna Cadet abstract model

(in-package #:manna-cadet.test)

(defun manna-model-path ()
  (merge-pathnames #p"../model/manna-cadet.lisp"
                   (asdf:system-source-directory :manna-cadet)))

(defun read-manna-model ()
  (with-open-file (stream (manna-model-path))
    (read-layout stream)))

(defun model-object-named (name objects accessor)
  (find name objects :key (lambda (object) (symbol-name (funcall accessor object)))
                     :test #'string=))

(defun model-key-named (name layout)
  (model-object-named name (layout-keys layout) #'abstract-key-name))

(defun model-level-named (name layout)
  (model-object-named name (layout-levels layout) #'level-name))

(defun model-layer-named (name layout)
  (model-object-named name (layout-layers layout) #'layer-name))

(defun model-action-at (key-name level-name layout &optional active-layer-names)
  (key-action-at
   (model-key-named key-name layout)
   (model-level-named level-name layout)
   layout
    (mapcar (lambda (name) (model-layer-named name layout)) active-layer-names)))

(defun model-resolves-none-p (key-name level-name layout)
  (multiple-value-bind (action resolved-p)
      (model-action-at key-name level-name layout)
    (and resolved-p (null action))))

(defun action-name (action)
  (cond
    ((symbol-action-p action) (symbol-name (symbol-action-symbol action)))
    ((command-action-p action) (symbol-name (command-action-command action)))))

(defun command-names-in-action (action)
  (cond
    ((command-action-p action) (list (symbol-name (command-action-command action))))
    ((tap-hold-action-p action)
     (append (command-names-in-action (tap-hold-action-instant action))
             (command-names-in-action (tap-hold-action-continuous action))))
    ((simultaneous-action-p action)
     (mapcan #'command-names-in-action (simultaneous-action-actions action)))
    (t '())))

(defun all-model-bindings (layout)
  (append (mapcan (lambda (key) (copy-list (abstract-key-bindings key)))
                  (layout-keys layout))
          (mapcan (lambda (combo) (copy-list (combo-bindings combo)))
                  (layout-combos layout))))

(deftest manna-model-parses-and-validates
  (let ((layout (read-manna-model)))
    (multiple-value-bind (diagnostics valid-p) (validate-layout layout)
      (and (layout-p layout) valid-p (null diagnostics)))))

(deftest manna-model-has-exact-selector-space
  (let* ((layout (read-manna-model))
         (dimensions (layout-dimensions layout))
         (levels (layout-levels layout))
         (expected-levels
           '("normal" "shift" "greek" "greek+shift" "top" "top+shift"
             "top+greek" "top+greek+shift"))
         (expected-tuples
           '(("default" "default" "default")
             ("active" "default" "default")
             ("default" "active" "default")
             ("active" "active" "default")
             ("default" "default" "active")
             ("active" "default" "active")
             ("default" "active" "active")
             ("active" "active" "active"))))
    (and (equal '("shift" "greek" "top")
                (identifier-names dimensions #'dimension-name))
         (every (lambda (dimension)
                  (and (string= "default" (symbol-name (dimension-default dimension)))
                       (equal '("default" "active")
                              (mapcar #'symbol-name (dimension-states dimension)))))
                dimensions)
         (equal expected-levels (identifier-names levels #'level-name))
         (equal expected-tuples
                (mapcar (lambda (level)
                          (mapcar (lambda (selector) (symbol-name (cdr selector)))
                                  (level-selectors level)))
                        levels)))))

(deftest manna-model-has-five-modifiers-and-separate-fun-layer
  (let ((layout (read-manna-model)))
    (and (equal '("control" "meta" "super" "hyper" "alt")
                (identifier-names (layout-modifiers layout) #'modifier-name))
         (string= "main" (symbol-name (layout-root-layer layout)))
         (equal '("main" "fun")
                (identifier-names (layout-layers layout) #'layer-name))
         (not (member "fun" (identifier-names (layout-levels layout) #'level-name)
                      :test #'string=)))))

(deftest manna-model-declares-and-uses-exact-command-vocabulary
  (let* ((layout (read-manna-model))
         (declared (identifier-names (layout-commands layout) #'command-name))
         (used (remove-duplicates
                (mapcan (lambda (binding)
                          (command-names-in-action (binding-action binding)))
                        (all-model-bindings layout))
                :test #'string=)))
    (and (= 29 (length declared))
         (= 29 (length used))
         (every (lambda (name) (member name used :test #'string=)) declared))))

(deftest manna-model-locks-documented-fun-mnemonics
  (let ((layout (read-manna-model))
        (placements
          '(("q" "quote")
            ("e" "macro")
            ("t" "over-strike")
            ("g" "abort")
            ("c" "break")
            ("z" "hold-output")
            ("right-home-k" "clear-input")
            ("right-home-l" "clear-screen")
            ("m" "resume")
            ("u" "status")
            ("right-home-semicolon" "end")
            ("period" "repeat")
            ("i" "call"))))
    (every (lambda (placement)
             (string= (second placement)
                      (action-name
                       (model-action-at (first placement) "normal" layout '("fun")))))
           placements)))

(deftest manna-model-locks-fun-number-row-placements
  (let ((layout (read-manna-model))
        (placements
          '(("number-1" "roman-one")
            ("number-2" "roman-two")
            ("number-3" "roman-three")
            ("number-4" "roman-four")
            ("number-7" "finger-left")
            ("number-8" "thumb-up")
            ("number-9" "thumb-down")
            ("number-0" "finger-right"))))
    (every (lambda (placement)
             (string= (second placement)
                      (action-name
                       (model-action-at (first placement) "normal" layout '("fun")))))
           placements)))

(deftest manna-model-q-resolution-preserves-top-greek-holes
  (let* ((layout (read-manna-model))
         (q-key (model-key-named "q" layout))
         (top-greek (model-level-named "top+greek" layout))
         (direct-binding
           (find-if (lambda (binding)
                       (and (string= "main" (symbol-name (binding-layer binding)))
                            (string= (symbol-name (level-name top-greek))
                                     (symbol-name (binding-level binding)))))
                    (abstract-key-bindings q-key)))
          (expected-symbols
            '(("normal" "q")
              ("shift" "capital-q")
              ("greek" "greek-theta")
              ("greek+shift" "greek-capital-theta")
              ("top" "logical-and"))))
    (and direct-binding
         (none-action-p (binding-action direct-binding))
         (string= "top" (symbol-name (level-fallback top-greek)))
         (every (lambda (entry)
                   (string= (second entry)
                            (action-name
                             (model-action-at "q" (first entry) layout))))
                 expected-symbols)
         (model-resolves-none-p "q" "top+greek" layout)
         (model-resolves-none-p "q" "top+greek+shift" layout))))

(deftest manna-model-output-cells-preserve-defined-values-and-holes
  (let* ((layout (read-manna-model))
         (output-keys
           '("number-1" "number-2" "number-3" "number-4" "number-5"
             "number-6" "number-7" "number-8" "number-9" "number-0"
             "minus" "equals" "backspace" "tab" "q" "w" "e" "r" "t"
             "y" "u" "i" "o" "p" "left-bracket" "right-bracket" "enter"
             "left-home-a" "left-home-s" "left-home-d" "left-home-f" "g"
             "h" "right-home-j" "right-home-k" "right-home-l"
             "right-home-semicolon" "apostrophe-hyper" "grave" "backslash"
             "z" "x" "c" "v" "b" "n" "m" "comma" "period" "slash"
             "space" "extra-angle"))
         (greek-shift-holes
           '("number-1" "number-2" "number-3" "number-4" "number-5"
             "number-6" "number-7" "number-8" "number-9" "number-0"
             "minus" "equals" "enter" "right-home-semicolon" "slash"
             "space"))
         (top-holes
           '("number-1" "number-2" "number-3" "number-4" "number-5"
             "number-6" "number-7" "number-8" "number-9" "number-0"
             "minus" "equals" "left-bracket" "right-bracket"
             "right-home-semicolon" "apostrophe-hyper" "grave" "backslash"
             "comma" "period" "slash"))
         (top-shift-defined '("backspace" "tab" "extra-angle")))
    (and (every (lambda (key)
                  (model-resolves-none-p key "greek+shift" layout))
                greek-shift-holes)
         (every (lambda (key) (model-resolves-none-p key "top" layout))
                top-holes)
         (every (lambda (key)
                  (or (member key top-shift-defined :test #'string=)
                      (model-resolves-none-p key "top+shift" layout)))
                output-keys)
         (every (lambda (key)
                  (and (model-resolves-none-p key "top+greek" layout)
                       (model-resolves-none-p key "top+greek+shift" layout)))
                output-keys)
         (every (lambda (level)
                  (string= "backspace"
                           (action-name (model-action-at "backspace" level layout))))
                '("normal" "shift" "greek" "greek+shift" "top" "top+shift"))
         (every (lambda (entry)
                  (string= (second entry)
                           (action-name (model-action-at "tab" (first entry) layout))))
                '(("normal" "tab") ("shift" "reverse-tab")
                  ("greek" "tab") ("greek+shift" "reverse-tab")
                  ("top" "tab") ("top+shift" "reverse-tab")))
         (string= "broken-bar"
                  (action-name (model-action-at "extra-angle" "top+shift" layout))))))

(deftest manna-model-locks-abstract-hold-actions
  (let* ((layout (read-manna-model))
         (home (model-action-at "left-home-a" "normal" layout))
         (pure-modifier (model-action-at "hyper-key" "normal" layout))
         (shift (model-action-at "left-shift" "normal" layout))
         (greek (model-action-at "greek-selector" "normal" layout))
         (top (model-action-at "top-selector" "normal" layout))
         (fun (model-action-at "fun-layer-left" "normal" layout)))
    (and (tap-hold-action-p home)
         (string= "a" (action-name (tap-hold-action-instant home)))
         (equal '("super")
                (mapcar #'symbol-name
                        (mods-action-modifiers (tap-hold-action-continuous home))))
         (mods-action-p pure-modifier)
         (equal '("hyper")
                (mapcar #'symbol-name (mods-action-modifiers pure-modifier)))
         (hold-selector-action-p shift)
         (string= "shift" (symbol-name (hold-selector-action-dimension shift)))
         (string= "active" (symbol-name (hold-selector-action-state shift)))
         (hold-selector-action-p greek)
         (string= "greek" (symbol-name (hold-selector-action-dimension greek)))
         (string= "active" (symbol-name (hold-selector-action-state greek)))
         (hold-selector-action-p top)
         (string= "top" (symbol-name (hold-selector-action-dimension top)))
         (string= "active" (symbol-name (hold-selector-action-state top)))
         (tap-hold-action-p fun)
         (string= "end" (action-name (tap-hold-action-instant fun)))
         (layer-hold-action-p (tap-hold-action-continuous fun))
         (string= "fun"
                  (symbol-name
                   (layer-hold-action-layer (tap-hold-action-continuous fun)))))))

(deftest manna-model-source-is-ascii-and-backend-free
  (let* ((source (uiop:read-file-string (manna-model-path)))
         (lower (string-downcase source))
         (forbidden
           '("xkb" "kanata" "qmk" "evdev" "keycode" "arbitrary-code"
             "ue000" "modifier-map" "device" "matrix" "coordinate"
             "timing" "milliseconds")))
    (and (every (lambda (character) (< (char-code character) 128)) source)
         (notany (lambda (term) (search term lower)) forbidden))))
