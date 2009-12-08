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


(send main-dialog show #t)