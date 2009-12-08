#lang scheme

(require "./dr-fuzzy.scm")
(require test-engine/scheme-tests)
(require scheme/system)

;; builds up a fake enviroment for testing purposes
;;
(define test-directories (list (build-path "./test-1/")
                               (build-path "./test-2/")
                               (build-path "./test-3/")
                               (build-path "./test-1/test-4/")
                               (build-path "./test-1/test-5/")
                               (build-path "./test-1/test-4/test-6/")))

(define test-files (list (build-path "./test-1/example.txt")
                         (build-path "./test-2/bob.java")
                         (build-path "./test-1/test-4/bob.marley")
                         (build-path "./test-1/test-4/test-6/something.scm")))


(define (setup)
  (begin (for-each (λ (a-dir)
                     (if (not (directory-exists? a-dir))
                         (make-directory a-dir)
                         void))
                   test-directories)
         (for-each (λ (a-file)
                     (cond
                       [(not (file-exists? a-file))
                        (let ([out (open-output-file a-file)])
                          (begin (write "hello world" out)
                                 (close-output-port out)))]
                       [else void]))
                   test-files)))


;; TODO: have to find a cleaner way to erase everything
;; for now a rm is fine, but this is not very portable
(define (teardown)
  (system (format "rm -rf ./test-*")))

(check-expect (setup) (void))


;; tests for checking if the system is reading all the files correctly

(check-expect (all-files "test-1")
              (list (string->path "test-1/example.txt")
                    (string->path "test-1/test-4/bob.marley")
                    (string->path "test-1/test-4/test-6/something.scm")))

(check-expect (all-files "test-2")
              (list (string->path "test-2/bob.java")))


;; building lists from paths
;; path->list 

(check-expect (path->list (string->path "test-1/test-4/test-6/"))
              '("test-1" "test-4" "test-6"))
(check-expect (path->list (string->path "app/db/"))
              '("app" "db"))

(check-expect (path->list (string->path "app/"))
              '("app"))

(check-expect (path->list (string->path "./"))
              empty)



;; cleaning the path


;(check-expect (clean-path (string->path "./EXAMPLE.txt"))
;              "EXAMPLE.txt")
;
;(check-expect (clean-path (string->path "./"))
;              "")
;
;(check-expect (clean-path (string->path "./app/db/wee.txt"))
;              "app/db/wee.txt")

;; building regexps


(check-expect
 (build-path-parts-regex (list "app" "db"))
 "(?i:^(.*?)(a)([^/]*?)(p)([^/]*?)(p)(.*?/.*?)(d)([^/]*?)(b)(.*?)$)")

(check-expect (build-file-regex "foo")
              "(?i:^(.*?)(f)([^/]*?)(o)([^/]*?)(o)(.*)$)")

(check-expect (make-pattern "")
              "()")

(check-expect (make-pattern "foo")
              "(f)([^/]*?)(o)([^/]*?)(o)")


;; tests for finding the number of directories
(check-expect (how-many-directories-up-to
               (string->path "./a-dir/a-file"))
              1)

(check-expect (how-many-directories-up-to
               (string->path "./a-dir/a-file/something"))
              2)

(check-expect (how-many-directories-up-to
               (string->path "./a-dir/a-file/"))
              2)

(check-expect (how-many-directories-up-to
               (string->path "./"))
              0)

(check-expect (how-many-directories-up-to
               (string->path "./something.txt"))
              0)

;; test if the system ignores hidden unix files by default

(check-expect (ignored? (string->path "./.hidden-stuff"))
              true)

(check-expect (ignored? (string->path "./.hidden-stuff/something"))
              true)

(check-expect (ignored? (string->path "./not-hidden"))
              false)

(check-expect (ignored? (string->path "./not-hidden-stuff/.hidden"))
              true)

(check-expect (ignored? (string->path "./name.othername/hidden"))
              false)

(check-expect (ignored? (string->path "./something~"))
              true)

(check-expect (ignored? (string->path "./a-thing/something~"))
              true)

(check-expect (ignored? (string->path "./name.othername/not.hidden"))
              false)

(check-expect (ignored? (string->path "./.hidden/not.hidden"))
              true)

(check-expect (ignored? (string->path "./not-hidden/.hidden/not-hidden"))
              true)


;; TESTS FOR build-match-result
;; 3 states being tested, empty, one file, file nested
(check-expect (build-match-result empty 0)
              (make-match-result "" 1 empty))

(check-expect (build-match-result
               (list "LICENSE.txt" "" "L" "" "I" "" "C" "ENSE.txt") 0)
              (make-match-result "(LIC)ENSE.txt" 3/11 empty))


(check-expect (build-match-result
               (list "compiled/drscheme/errortrace/"
                     "" "c" "" "o" "mpiled/" "d" "rscheme/" "e" "rrortrace")
               2)
              (make-match-result "(co)mpiled/(d)rscheme/(e)rrortrace"
                                 2/13
                                 empty))

(check-expect (build-match-result
               (list "LICENSE.txt" "" "L" "" "I" "" "C" "ENSE.txt") 0)
              (make-match-result "(LIC)ENSE.txt" 3/11 empty))


;; adding match results
(check-expect (add-match-results (make-match-result "(t)est-1/(t)est-4/"
                                                    1/6
                                                    empty)
                                 (make-match-result "(bob).marley"
                                                    3/10
                                                    empty))
              (make-match-result "(t)est-1/(t)est-4/(bob).marley"
                                 1/20
                                 empty))

(check-expect (add-match-results
               (make-match-result "(test-1)/(test-4)/(test-6)" 1 empty)
               (make-match-result "(something.scm)" 1 empty))
              (make-match-result "(test-1)/(test-4)/(test-6)/(something.scm)"
                                 1
                                 empty))

(check-expect (add-match-results (make-match-result "" 1.0 empty)
                                 (make-match-result "(README)" 1.0 empty))
              
              (make-match-result "(README)" 1.0 empty))

(check-expect (add-match-results
               (make-match-result "test-1/test-4/test-6" 1.0 empty)
               (make-match-result "(so)mething.scm" 0.1538 empty))
              (make-match-result "t/t/t/(so)mething.scm" 0.1538 empty))


;; testing the search with local conditions
(check-expect (search "")
              empty)

(check-expect (search "test-1/test-4/test-6/something.scm")
              (list
               (make-match-result
                "(test-1)/(test-4)/(test-6)/(something.scm)"
                1
                (string->path "./test-1/test-4/test-6/something.scm"))))

(check-expect (search "README")
              (list (make-match-result
                     "(README)"
                     1
                     (string->path "./README"))))

(check-expect (search "REAME")
              (list (make-match-result
                     "(REA)D(ME)"
                     5/12
                     (string->path "./README"))))

(check-expect (search "t/t/bob")
              (list (make-match-result
                     "(t)est-1/(t)est-4/(bob).marley"
                     1/20
                     (string->path "./test-1/test-4/bob.marley"))))

(check-expect (search "bob")
              (list (make-match-result
                     "t/(bob).java"
                     0.375
                     (string->path "./test-2/bob.java"))
                    (make-match-result
                     "t/t/(bob).marley"
                     0.3
                     (string->path "./test-1/test-4/bob.marley"))))

; this test is not good because it depends of the compiled file for drscheme
(check-expect (search "so")
              (list (make-match-result
                     "t/t/t/(so)mething.scm"
                     0.15384615384615385
                     (string->path "./test-1/test-4/test-6/something.scm"))
                    (make-match-result
                     "c/d/e/dr-fuzzy_(s)cm.z(o)"
                     0.06666666666666667
                     (string->path
                      "./compiled/drscheme/errortrace/dr-fuzzy_scm.zo"))))

;; uncomment this to cleanup all the created test files
;; in unix 

;(check-expect (teardown) true)

(test)