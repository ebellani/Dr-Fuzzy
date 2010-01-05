#lang setup/infotab
(define name "DrFuzzy")
(define blurb '("Quick find files to open inside DrScheme."))
(define homepage "http://github.com/ebellani/Dr-Fuzzy")
(define version "1.1")

(define release-notes
  '((p "Second release.")
    (p "Fixed the a bug involving a file with a char that was used in regexp.") 
    (p "Minor improvements in the code")))

(define primary-file "fuzzy-tool.scm")
(define categories '(devtools))

;; This is the file which is loaded on the module start.
(define tools '(("fuzzy-tool.scm")))

;; the url of the plugin.
(define tool-urls (list #f))

;; don't compile the test file
(define compile-omit-paths (list "dr-fuzzy-test.scm"))

(define required-core-version "4.0")
(define repositories '("4.x"))
(define tool-icons (list "fuzzy.jpg"))