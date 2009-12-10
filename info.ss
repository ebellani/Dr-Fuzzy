#lang setup/infotab
(define name "DrFuzzy")
(define blurb '("Quick find files"))
(define homepage "http://github.com/ebellani/Dr-Fuzzy")
(define version "1.0")

(define release-notes '((p "First version. Supports opening multiple tabs.")))

;; copied from divascheme info.ss

(define primary-file "fuzzy-tool.scm")
(define categories '(devtools))
;; This is the file which is loaded on the module start.
(define tools '(("fuzzy-tool.scm")))

;; the url of the plugin.
(define tool-urls (list #f))

;; don't compile the test or the project tabs ;)
(define compile-omit-paths (list "dr-fuzzy-test.scm"
                                 "saved-tabs-file.sp"))

(define required-core-version "4.0")
(define repositories '("4.x"))
