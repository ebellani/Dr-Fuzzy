#lang scheme
(require drscheme/tool
         drscheme/tool-lib
         mred
         mzlib/unit
         scheme/class
         "dr-fuzzy.scm"
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
                  (start-drfuzzy drscheme:unit:open-drscheme-window))]))
        
        (super-new)))
    (drscheme:get/extend:extend-unit-frame dr-fuzzy-unit-frame-mixim)))
