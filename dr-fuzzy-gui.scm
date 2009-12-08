#lang scheme/gui
(require "./dr-fuzzy.scm")

(define main-dialog (new dialog%
                         [label "DrFuzzy"]
                         [width 500]
                         [height 400]))

(define search-field (new text-field%
                          [parent main-dialog]
                          [label "&Search for:"]
                          [callback (λ (a-text-field event)
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
                 (open-files))])

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
    (begin (send list-of-hits set empty) ;; clears the list-box
           (fill (search query)))))

;; open-files : void -> void
;; open all the files represented by the selected items on the hit list.
(define (open-files)
  (local [(define (open-the-selection selected-files)
            (cond
              [(empty? selected-files) (void)]
              [else
               (begin
                 (display (send list-of-hits ;;here open the file
                                get-data
                                (first selected-files)))
                 (open-the-selection (rest selected-files)))]))]
    (open-the-selection (send list-of-hits get-selections))))

(send main-dialog show #t)