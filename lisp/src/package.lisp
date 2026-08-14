;;;; package.lisp - namespace layout for the abstract keyboard-layout frontend

(defpackage #:manna-cadet
  (:use #:cl)
  (:export
   #:modifier
   #:modifier-p
   #:make-modifier
   #:modifier-name
   #:dimension
   #:dimension-p
   #:make-dimension
    #:dimension-name
    #:dimension-states
    #:dimension-default
   #:layout-symbol
   #:layout-symbol-p
   #:make-layout-symbol
   #:layout-symbol-name
   #:layout-symbol-display
   #:command
   #:command-p
   #:make-command
   #:command-name
   #:level
   #:level-p
   #:make-level
   #:level-name
    #:level-selectors
    #:level-fallback
    #:layer
    #:layer-p
    #:make-layer
    #:layer-name
    #:binding
    #:binding-p
    #:make-binding
    #:binding-layer
    #:binding-level
    #:binding-action
   #:action
   #:action-p
   #:symbol-action
   #:symbol-action-p
   #:make-symbol-action
   #:symbol-action-symbol
   #:command-action
   #:command-action-p
   #:make-command-action
   #:command-action-command
   #:mods-action
   #:mods-action-p
   #:make-mods-action
   #:mods-action-modifiers
   #:hold-selector-action
   #:hold-selector-action-p
    #:make-hold-selector-action
    #:hold-selector-action-dimension
    #:hold-selector-action-state
    #:layer-hold-action
    #:layer-hold-action-p
    #:make-layer-hold-action
    #:layer-hold-action-layer
    #:simultaneous-action
    #:simultaneous-action-p
    #:make-simultaneous-action
    #:simultaneous-action-actions
   #:tap-hold-action
   #:tap-hold-action-p
   #:make-tap-hold-action
    #:tap-hold-action-instant
    #:tap-hold-action-continuous
   #:none-action
   #:none-action-p
   #:make-none-action
   #:transparent-action
   #:transparent-action-p
   #:make-transparent-action
   #:abstract-key
   #:abstract-key-p
   #:make-abstract-key
   #:abstract-key-name
   #:abstract-key-bindings
   #:combo
   #:combo-p
   #:make-combo
    #:combo-keys
    #:combo-bindings
   #:layout
   #:layout-p
   #:make-layout
   #:layout-name
    #:layout-levels
    #:layout-layers
    #:layout-root-layer
   #:layout-modifiers
   #:layout-dimensions
   #:layout-symbols
   #:layout-commands
   #:layout-keys
   #:layout-combos
     #:key-action-at
      #:combo-action-at
      #:unsafe-text-code-point-p
      #:valid-identifier-p
    #:layout-read-error
   #:layout-read-error-message
   #:layout-read-error-context
   #:layout-parse-error
   #:layout-parse-error-message
    #:layout-parse-error-context
    #:parse-layout
    #:read-layout
    #:diagnostic
    #:diagnostic-p
    #:make-diagnostic
    #:diagnostic-severity
    #:diagnostic-code
    #:diagnostic-message
    #:diagnostic-context
     #:validate-layout
     #:layout-equal-p
      #:normalize-layout
      #:inspect-layout
      #:inspection-identifier
      #:inspection-limit-exceeded
      #:inspection-limit-exceeded-kind
      #:inspection-limit-exceeded-estimate
      #:inspection-limit-exceeded-limit))

(defpackage #:manna-cadet.realization
  (:use #:cl)
  (:export
   #:realization-profile
   #:realization-not-implemented
   #:realization-not-implemented-operation
   #:validate-profile
   #:compile-layout))

(defpackage #:manna-cadet.cli
  (:use #:cl)
  (:export #:main #:run-command))

(defpackage #:manna-cadet.test
  (:use #:cl #:manna-cadet)
  (:export #:run-all))
