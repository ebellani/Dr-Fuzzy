#lang scheme/base
(require drscheme/tool
         mred
         mrlib/switchable-button
         mzlib/unit
         scheme/class
         "dr-fuzzy-gui.scm")

(provide tool@)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    (define phase1 void)
    (define phase2 void)
    
    (define start-message "&DrFuzzy file finder")
    
    (define dr-fuzzy-unit-frame-mixim
      (mixin (drscheme:unit:frame<%>) ()
        
        (define/override (file-menu:between-open-and-revert menu)
          (super file-menu:between-open-and-revert menu)
          (new menu-item%
               [label start-message]
               [parent menu]
               [callback
                (lambda (menu-item control-event)
                  (start-drfuzzy))]))
        
        (super-new))
      )
    (drscheme:get/extend:extend-unit-frame dr-fuzzy-unit-frame-mixim)))
