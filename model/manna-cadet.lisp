(layout manna-cadet
  (dimensions
    (shift (default default) (states default active))
    (greek (default default) (states default active))
    (top (default default) (states default active)))
  (levels
    (normal
      (selectors (shift default) (greek default) (top default)))
    (shift
      (selectors (shift active) (greek default) (top default))
      (fallback normal))
    (greek
      (selectors (shift default) (greek active) (top default))
      (fallback normal))
    (greek+shift
      (selectors (shift active) (greek active) (top default))
      (fallback greek))
    (top
      (selectors (shift default) (greek default) (top active))
      (fallback normal))
    (top+shift
      (selectors (shift active) (greek default) (top active))
      (fallback top))
    (top+greek
      (selectors (shift default) (greek active) (top active))
      (fallback top))
    (top+greek+shift
      (selectors (shift active) (greek active) (top active))
      (fallback top+greek)))
  (layers (root main) (layer fun))
  (modifiers (control meta super hyper alt))
  (commands
    (macro terminal quote over-strike clear-input clear-screen
     hold-output stop-output abort break system network call end
     roman-one roman-two roman-three roman-four
     finger-left thumb-up thumb-down finger-right
     repeat alt-mode mode-lock resume status line help))
  (symbols
    digit-0 digit-1 digit-2 digit-3 digit-4
    digit-5 digit-6 digit-7 digit-8 digit-9
    exclamation-mark at-sign number-sign dollar-sign percent-sign caret
    ampersand asterisk left-parenthesis right-parenthesis hyphen-minus
    underscore equal-sign plus-sign dagger double-dagger
    open-triangle-bullet-down cent-sign circle white-vertical-rectangle
    division-sign multiplication-sign paragraph-sign open-circle
    almost-equal empty-set
    q w e r t y u i o p a s d f g h j k l z x c v b n m
    capital-q capital-w capital-e capital-r capital-t capital-y capital-u
    capital-i capital-o capital-p capital-a capital-s capital-d capital-f
    capital-g capital-h capital-j capital-k capital-l capital-z capital-x
    capital-c capital-v capital-b capital-n capital-m
    greek-theta greek-omega greek-epsilon greek-rho greek-tau greek-psi
    greek-upsilon greek-iota greek-omicron greek-pi greek-alpha greek-sigma
    greek-delta greek-phi greek-gamma greek-eta greek-theta-symbol
    greek-kappa greek-lambda greek-zeta greek-xi greek-chi
    greek-final-sigma greek-beta greek-nu greek-mu
    greek-capital-theta greek-capital-omega greek-capital-epsilon
    greek-capital-rho greek-capital-tau greek-capital-psi
    greek-capital-upsilon greek-capital-iota greek-capital-omicron
    greek-capital-pi greek-capital-alpha greek-capital-sigma
    greek-capital-delta greek-capital-phi greek-capital-gamma
    greek-capital-eta greek-capital-theta-symbol greek-capital-kappa
    greek-capital-lambda greek-capital-zeta greek-capital-xi
    greek-capital-chi greek-capital-beta greek-capital-nu greek-capital-mu
    logical-and logical-or union intersection subset-of superset-of
    for-all infinity exists partial-derivative bottom top-element
    right-tack left-tack up-arrow down-arrow left-arrow right-arrow
    left-right-arrow left-floor left-ceiling similar-or-equal identical
    less-than-or-equal greater-than-or-equal
    left-bracket right-bracket left-brace right-brace
    mathematical-left-white-square-bracket
    mathematical-right-white-square-bracket
    left-white-curly-bracket right-white-curly-bracket
    semicolon colon apostrophe quotation-mark diaeresis middle-dot
    greek-ano-teleia grave-accent tilde backslash vertical-line
    double-vertical-line broken-bar comma period slash question-mark
    less-than-sign greater-than-sign left-guillemet right-guillemet
    left-double-angle-bracket right-double-angle-bracket integral
    backspace tab reverse-tab enter line-feed space escape delete end
    page-down menu)
  (keys
    (key number-1
      ((main normal) digit-1)
      ((main shift) exclamation-mark)
      ((main greek) dagger)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command roman-one)))
    (key number-2
      ((main normal) digit-2)
      ((main shift) at-sign)
      ((main greek) double-dagger)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command roman-two)))
    (key number-3
      ((main normal) digit-3)
      ((main shift) number-sign)
      ((main greek) open-triangle-bullet-down)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command roman-three)))
    (key number-4
      ((main normal) digit-4)
      ((main shift) dollar-sign)
      ((main greek) cent-sign)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command roman-four)))
    (key number-5
      ((main normal) digit-5)
      ((main shift) percent-sign)
      ((main greek) circle)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key number-6
      ((main normal) digit-6)
      ((main shift) caret)
      ((main greek) white-vertical-rectangle)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key number-7
      ((main normal) digit-7)
      ((main shift) ampersand)
      ((main greek) division-sign)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command finger-left)))
    (key number-8
      ((main normal) digit-8)
      ((main shift) asterisk)
      ((main greek) multiplication-sign)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command thumb-up)))
    (key number-9
      ((main normal) digit-9)
      ((main shift) left-parenthesis)
      ((main greek) paragraph-sign)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command thumb-down)))
    (key number-0
      ((main normal) digit-0)
      ((main shift) right-parenthesis)
      ((main greek) open-circle)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command finger-right)))
    (key minus
      ((main normal) hyphen-minus)
      ((main shift) underscore)
      ((main greek) almost-equal)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key equals
      ((main normal) equal-sign)
      ((main shift) plus-sign)
      ((main greek) empty-set)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key q
      ((main normal) q)
      ((main shift) capital-q)
      ((main greek) greek-theta)
      ((main greek+shift) greek-capital-theta)
      ((main top) logical-and)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command quote)))
    (key w
      ((main normal) w)
      ((main shift) capital-w)
      ((main greek) greek-omega)
      ((main greek+shift) greek-capital-omega)
      ((main top) logical-or)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command terminal)))
    (key e
      ((main normal) e)
      ((main shift) capital-e)
      ((main greek) greek-epsilon)
      ((main greek+shift) greek-capital-epsilon)
      ((main top) union)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command macro)))
    (key r
      ((main normal) r)
      ((main shift) capital-r)
      ((main greek) greek-rho)
      ((main greek+shift) greek-capital-rho)
      ((main top) intersection)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key t
      ((main normal) t)
      ((main shift) capital-t)
      ((main greek) greek-tau)
      ((main greek+shift) greek-capital-tau)
      ((main top) subset-of)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command over-strike)))
    (key y
      ((main normal) y)
      ((main shift) capital-y)
      ((main greek) greek-psi)
      ((main greek+shift) greek-capital-psi)
      ((main top) superset-of)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key u
      ((main normal) u)
      ((main shift) capital-u)
      ((main greek) greek-upsilon)
      ((main greek+shift) greek-capital-upsilon)
      ((main top) for-all)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command status)))
    (key i
      ((main normal) i)
      ((main shift) capital-i)
      ((main greek) greek-iota)
      ((main greek+shift) greek-capital-iota)
      ((main top) infinity)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command call)))
    (key o
      ((main normal) o)
      ((main shift) capital-o)
      ((main greek) greek-omicron)
      ((main greek+shift) greek-capital-omicron)
      ((main top) exists)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command stop-output)))
    (key p
      ((main normal) p)
      ((main shift) capital-p)
      ((main greek) greek-pi)
      ((main greek+shift) greek-capital-pi)
      ((main top) partial-derivative)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key left-bracket
      ((main normal) left-bracket)
      ((main shift) left-brace)
      ((main greek) mathematical-left-white-square-bracket)
      ((main greek+shift) left-white-curly-bracket)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key right-bracket
      ((main normal) right-bracket)
      ((main shift) right-brace)
      ((main greek) mathematical-right-white-square-bracket)
      ((main greek+shift) right-white-curly-bracket)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key left-home-a
      ((main normal) (tap-hold a (mods super)))
      ((main shift) (tap-hold capital-a (mods super)))
      ((main greek) (tap-hold greek-alpha (mods super)))
      ((main greek+shift) (tap-hold greek-capital-alpha (mods super)))
      ((main top) (tap-hold bottom (mods super)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key left-home-s
      ((main normal) (tap-hold s (mods meta)))
      ((main shift) (tap-hold capital-s (mods meta)))
      ((main greek) (tap-hold greek-sigma (mods meta)))
      ((main greek+shift) (tap-hold greek-capital-sigma (mods meta)))
      ((main top) (tap-hold top-element (mods meta)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command system)))
    (key left-home-d
      ((main normal) (tap-hold d (mods control)))
      ((main shift) (tap-hold capital-d (mods control)))
      ((main greek) (tap-hold greek-delta (mods control)))
      ((main greek+shift) (tap-hold greek-capital-delta (mods control)))
      ((main top) (tap-hold right-tack (mods control)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key left-home-f
      ((main normal) (tap-hold f (hold-selector shift active)))
      ((main shift) (tap-hold capital-f (hold-selector shift active)))
      ((main greek) (tap-hold greek-phi (hold-selector shift active)))
      ((main greek+shift)
       (tap-hold greek-capital-phi (hold-selector shift active)))
      ((main top) (tap-hold left-tack (hold-selector shift active)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key g
      ((main normal) g)
      ((main shift) capital-g)
      ((main greek) greek-gamma)
      ((main greek+shift) greek-capital-gamma)
      ((main top) up-arrow)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command abort)))
    (key h
      ((main normal) h)
      ((main shift) capital-h)
      ((main greek) greek-eta)
      ((main greek+shift) greek-capital-eta)
      ((main top) down-arrow)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command help)))
    (key right-home-j
      ((main normal) (tap-hold j (hold-selector shift active)))
      ((main shift) (tap-hold capital-j (hold-selector shift active)))
      ((main greek) (tap-hold greek-theta-symbol (hold-selector shift active)))
      ((main greek+shift)
       (tap-hold greek-capital-theta-symbol (hold-selector shift active)))
      ((main top) (tap-hold left-arrow (hold-selector shift active)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command line)))
    (key right-home-k
      ((main normal) (tap-hold k (mods control)))
      ((main shift) (tap-hold capital-k (mods control)))
      ((main greek) (tap-hold greek-kappa (mods control)))
      ((main greek+shift) (tap-hold greek-capital-kappa (mods control)))
      ((main top) (tap-hold right-arrow (mods control)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command clear-input)))
    (key right-home-l
      ((main normal) (tap-hold l (mods meta)))
      ((main shift) (tap-hold capital-l (mods meta)))
      ((main greek) (tap-hold greek-lambda (mods meta)))
      ((main greek+shift) (tap-hold greek-capital-lambda (mods meta)))
      ((main top) (tap-hold left-right-arrow (mods meta)))
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command clear-screen)))
    (key right-home-semicolon
      ((main normal) (tap-hold semicolon (mods super)))
      ((main shift) (tap-hold colon (mods super)))
      ((main greek) (tap-hold diaeresis (mods super)))
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command end)))
    (key apostrophe-hyper
      ((main normal) (tap-hold apostrophe (mods hyper)))
      ((main shift) (tap-hold quotation-mark (mods hyper)))
      ((main greek) (tap-hold middle-dot (mods hyper)))
      ((main greek+shift) (tap-hold greek-ano-teleia (mods hyper)))
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key grave
      ((main normal) grave-accent)
      ((main shift) tilde)
      ((main greek) grave-accent)
      ((main greek+shift) tilde)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command mode-lock)))
    (key backslash
      ((main normal) backslash)
      ((main shift) vertical-line)
      ((main greek) double-vertical-line)
      ((main greek+shift) broken-bar)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key z
      ((main normal) z)
      ((main shift) capital-z)
      ((main greek) greek-zeta)
      ((main greek+shift) greek-capital-zeta)
      ((main top) logical-or)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command hold-output)))
    (key x
      ((main normal) x)
      ((main shift) capital-x)
      ((main greek) greek-xi)
      ((main greek+shift) greek-capital-xi)
      ((main top) left-floor)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command network)))
    (key c
      ((main normal) c)
      ((main shift) capital-c)
      ((main greek) greek-chi)
      ((main greek+shift) greek-capital-chi)
      ((main top) left-ceiling)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command break)))
    (key v
      ((main normal) v)
      ((main shift) capital-v)
      ((main greek) greek-final-sigma)
      ((main greek+shift) greek-capital-sigma)
      ((main top) similar-or-equal)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key b
      ((main normal) b)
      ((main shift) capital-b)
      ((main greek) greek-beta)
      ((main greek+shift) greek-capital-beta)
      ((main top) identical)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key n
      ((main normal) n)
      ((main shift) capital-n)
      ((main greek) greek-nu)
      ((main greek+shift) greek-capital-nu)
      ((main top) less-than-or-equal)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key m
      ((main normal) m)
      ((main shift) capital-m)
      ((main greek) greek-mu)
      ((main greek+shift) greek-capital-mu)
      ((main top) greater-than-or-equal)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command resume)))
    (key comma
      ((main normal) comma)
      ((main shift) less-than-sign)
      ((main greek) left-guillemet)
      ((main greek+shift) left-double-angle-bracket)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key period
      ((main normal) period)
      ((main shift) greater-than-sign)
      ((main greek) right-guillemet)
      ((main greek+shift) right-double-angle-bracket)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none)
      ((fun normal) (command repeat)))
    (key slash
      ((main normal) slash)
      ((main shift) question-mark)
      ((main greek) integral)
      ((main greek+shift) none)
      ((main top) none)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key extra-angle
      ((main normal) less-than-sign)
      ((main shift) greater-than-sign)
      ((main greek) less-than-sign)
      ((main greek+shift) greater-than-sign)
      ((main top) vertical-line)
      ((main top+shift) broken-bar)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key backspace
      ((main normal) backspace)
      ((main shift) backspace)
      ((main greek) backspace)
      ((main greek+shift) backspace)
      ((main top) backspace)
      ((main top+shift) backspace)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key tab
      ((main normal) tab)
      ((main shift) reverse-tab)
      ((main greek) tab)
      ((main greek+shift) reverse-tab)
      ((main top) tab)
      ((main top+shift) reverse-tab)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key enter
      ((main normal) enter)
      ((main shift) none)
      ((main greek) line-feed)
      ((main greek+shift) none)
      ((main top) enter)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key space
      ((main normal) space)
      ((main shift) none)
      ((main greek) space)
      ((main greek+shift) none)
      ((main top) space)
      ((main top+shift) none)
      ((main top+greek) none)
      ((main top+greek+shift) none))
    (key escape-hyper
      ((main normal) (tap-hold escape (mods hyper))))
    (key left-thumb-backspace
      ((main normal) (tap-hold backspace (mods alt))))
    (key right-thumb-space
      ((main normal) (tap-hold space (mods alt))))
    (key greek-selector
      ((main normal) (hold-selector greek active)))
    (key greek-selector-delete
      ((main normal) (tap-hold delete (hold-selector greek active))))
    (key top-selector
      ((main normal) (hold-selector top active)))
    (key top-selector-enter
      ((main normal) (tap-hold enter (hold-selector top active))))
    (key left-shift
      ((main normal) (hold-selector shift active)))
    (key right-shift
      ((main normal) (hold-selector shift active)))
    (key hyper-key
      ((main normal) (mods hyper)))
    (key fun-layer-left
      ((main normal) (tap-hold end (layer-hold fun))))
    (key fun-layer-right
      ((main normal) (tap-hold page-down (layer-hold fun))))
    (key menu
      ((main normal) menu)
      ((fun normal) (command alt-mode)))))
