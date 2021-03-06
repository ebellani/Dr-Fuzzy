(module dr-fuzzy-gui scheme/gui
  (provide start-drfuzzy)
  
  (require "dr-fuzzy.scm"
           framework/gui-utils)
  
  (define (create-dr-fuzzy-dialog function-to-use-with-files)
    (local
      [;; CONTROL FUNCTIONS      
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
                              (match-result-tagged-path
                               (first list-of-match-results))
                              (match-result-path
                               (first list-of-match-results)))
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
                        (function-to-use-with-files
                         (path->string (send list-of-hits
                                             get-data
                                             (first selected-files))))
                        (open-the-selection (rest selected-files)))]))]
           (open-the-selection (send list-of-hits get-selections))))
       
       (define (clean-list)
         (send list-of-hits set empty))
       
       (define (clean-all)
         (begin (send (send search-field
                            get-editor)
                      erase)
                (clean-list)))
       
       ;; WIDGETS 
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
       
       (define-values (button-ok button-cancel)
         (gui-utils:ok/cancel-buttons panel
                                      (lambda (button event)
                                        (begin (open-files)
                                               (send main-dialog show #f)))
                                      (lambda (button event)
                                        (send main-dialog show #f))))]
      (begin 
        (clean-all)
        (send search-field focus)
        (send main-dialog show #t))))
  
  ;; fails if there is more than a fixed ammount of files
  ;; in the current dir. Uses a function with each file
  ;; selected in the dialog
  ;; start-drfuzzy : (path->string -> X) -> void
  (define (start-drfuzzy do-with-files)
    (with-handlers ((exn:fail?
                     (λ (exception)
                       (message-box "Error starting dr-fuzzy"
                                    (exn-message exception)
                                    #f
                                    '(ok stop)))))
      (begin
        (load-files!)
        (create-dr-fuzzy-dialog do-with-files)))))