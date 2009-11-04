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

(check-expect (subdirectories "test-1")
              (list (string->path "test-1/test-4")
                    (string->path "test-1/test-5")))

(check-expect (files "test-1")
              (list (string->path "test-1/example.txt")))

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

(check-expect (build-match-result empty 1)
              (make-match-result "" 1))

(check-expect (build-match-result (list "LICENSE.txt" "" "L" "" "I" "" "C" "ENSE.txt") 1)
              (make-match-result "(LIC)ENSE.txt" (exact->inexact 3/11)))


(check-expect (build-match-result (list "compiled/drscheme/errortrace"
                                        "" "c" "" "o" "mpiled/" "d" "rscheme/" "e" "rrortrace") 3)
              (make-match-result "(co)mpiled/(d)rscheme/(e)rrortrace" (exact->inexact 2/13)))


(check-expect (teardown) true)

(test)

