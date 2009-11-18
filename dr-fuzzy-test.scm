#lang scheme

(require "./dr-fuzzy.scm")
(require test-engine/scheme-tests)
(require scheme/system)

;; builds up a fake enviroment for testing purposes
;;
(define test-directories (list (build-path "test-1/")
                               (build-path "test-2/")
                               (build-path "test-3/")
                               (build-path "test-1/test-4/")
                               (build-path "test-1/test-5/")
                               (build-path "test-1/test-4/test-6/")))

(define test-files (list (build-path "test-1/example.txt")
                         (build-path "test-2/bob.java")
                         (build-path "test-1/test-4/chuck.norris")
                         (build-path "test-1/test-4/test-6/something.scm")))


(define (setup)
  (begin (for-each (λ (a-dir)
                     (if (not (directory-exists? a-dir))
                         (make-directory a-dir)
                         void))
                   test-directories)
         (for-each (λ (a-file)
                     (if (not (link-exists? a-file))
                         (make-file-or-directory-link a-file a-file)
                         void))
                   test-files)))


;; TODO: have to find a cleaner way to erase everything
;; for now a rm is fine, but this is not very portable
(define (teardown)
  (system (format "rm -rf ./test-*")))



(check-expect (setup) (void))


(check-expect (all-files "test-1")
              (list (string->path "test-1/example.txt")
                    (string->path "test-1/test-4/chuck.norris")
                    (string->path "test-1/test-4/test-6/something.scm")))

(check-expect (all-files "test-2")
              (list (string->path "test-2/bob.java")))


(check-expect (build-path-parts-regex (list "app" "db"))
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
;; TODO, FIX THE NUMBER OF DIRECTORIES
;(check-expect (build-match-result empty 0)
;              (make-match-result "" 1))
;
;(check-expect (build-match-result
;               (list "LICENSE.txt" "" "L" "" "I" "" "C" "ENSE.txt") 0)
;              (make-match-result "(LIC)ENSE.txt" (exact->inexact 3/11)))
;
;
;(check-expect (build-match-result
;               (list "compiled/drscheme/errortrace"
;                     "" "c" "" "o" "mpiled/" "d" "rscheme/" "e" "rrortrace") 2)
;              (make-match-result "(co)mpiled/(d)rscheme/(e)rrortrace"
;                                 (exact->inexact 2/13)))

;; testing the search with local conditions
;(check-expect ("test-1/test-4/test-6/something.scm")
;              (list
;               (make-match-result "(test-1)/(test-4)/(test-6)/(something.scm)"
;                                  1)))

;(check-expect (teardown) true)

(reload-files!)



(test)