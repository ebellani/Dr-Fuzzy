#lang scheme
(require drscheme/tool
         drscheme/tool-lib
         mred
         mzlib/unit
         scheme/class
         "dr-fuzzy.scm")

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
        
        (super-new)))
    (drscheme:get/extend:extend-unit-frame dr-fuzzy-unit-frame-mixim)))

  (define main-dialog (new dialog%
                           [label "DrFuzzy"]
                           [width 500]
                           [height 400]))
  
  (define search-field (new text-field%
                            [parent main-dialog]
                            [label "&Search for:"]
                            [callback
                             (λ (a-text-field event)
                               (fill-list-with-search-result
                                (send (send a-text-field
                                            get-editor)
                                      get-text)))]))
  
  (define list-of-hits (new list-box%
                            [label "&Results:"]
                            [choices empty]
                            [parent main-dialog]
                            [style '(multiple vertical-label)]))
  
  (define panel (new horizontal-panel%
                     [parent main-dialog]
                     [alignment '(right bottom)]
                     [stretchable-height #f]))
  
  (new button% [parent panel]
       [label "&Cancel"]
       [callback (lambda (button event)
                   (send main-dialog show #f))])
  
  (new button% [parent panel]
       [label "&Ok"]
       [callback (lambda (button event)
                   (begin (open-files)
                          (send main-dialog show #f)))])
  
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  
  
  ;; CONTROL FUNCTIONS
  
  ;; fill-list-with-search-result : string -> void
  ;; fills the list-of-hits with the results of the search function
  ;; of the dr-fuzzy lib.
  (define (fill-list-with-search-result query)
    (local [(define (fill list-of-match-results)
              (cond
                [(empty? list-of-match-results) (void)]
                [else
                 (begin
                   (send list-of-hits append
                         (match-result-tagged-path (first list-of-match-results))
                         (match-result-path (first list-of-match-results)))
                   (fill (rest list-of-match-results)))]))]
      (begin (send list-of-hits set empty)
             (fill (search query)))))
  
  ;; open-files : void -> void
  ;; open all the files represented by the selected items on the hit list.
  (define (open-files)
    (local [(define (open-the-selection selected-files)
              (cond
                [(empty? selected-files) (void)]
                [else
                 (begin
                   (drscheme:unit:open-drscheme-window
                    (path->string (send list-of-hits
                                        get-data
                                        (first selected-files))))
                   (open-the-selection (rest selected-files)))]))]
      (open-the-selection (send list-of-hits get-selections))))
  
  (define (clean-list)
    (send list-of-hits set empty))
  
  (define (clean-all)
    (begin (clean-list)
           (send (send search-field
                       get-editor)
                 erase)))
  
  ;; popup-message : string -> void
  ;; little dialog to show the error message
  (define (popup-message message)
    (begin
      (define message-dialog (new dialog%
                                  [label "DrFuzzy"]))
      (new message%
           [parent message-dialog]
           [label message])
      (new button% [parent message-dialog]
           [label "&Ok"]
           [callback (lambda (button event)
                       (send message-dialog show #f))])
      (send message-dialog show #t)))
  
  ;; fails if there is more than a fixed ammount of files
  ;; in the current dir
  (define (start-drfuzzy)
    (with-handlers ((exn:fail?
                     (λ (exception)
                       (popup-message (exn-message exception)))))
      (begin
        (clean-all)
        (reload-files!)
        (send main-dialog show #t))))
  
