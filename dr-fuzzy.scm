(module dr-fuzzy scheme
  (provide subdirectories 
           files
           all-files
           make-pattern
           build-path-parts-regex
           build-file-regex
           build-match-result
           make-match-result
           reload-files!
           ignore?)
  
  
  ;; result is a string representing the formatted text result, like "lib/c(ap)_(p)ool/"
  ;; score is the final weight that will be used to calculate the probability that this
  ;; result is the one that the user wants
  (define-struct match-result (score result) #:transparent)
  
  ;; match-result
  
  ;; a run is a piece of a match that represents the 
  ;; different parts matched.
  ;;     for example, the match "app" for the string "lib/cap_pool/" will produce
  ;;     ((make-run ("test/c" false) ("ap" true) ("_" false)  ("p" true) ("ool" false)))
  ;;     indicating what parts of the file was matched. 
  ;; So capture is the text itself like "test/c"
  ;; and is inside is if the text was used to capture the match. "test/c" was not, so it is
  ;; false, but "ap" was used, so it is true
  (define-struct run (capture is-inside?) #:mutable)
  
  ;; used to mark a run
  (define LEFT-RUN-MARKER "(")
  (define RIGHT-RUN-MARKER ")")
  
  ;; used in the separation into fragments
  ;; like "foo" -> (f)([^/]*?)(o)([^/]*?)(o)
  (define LEFT-PATTERN-MARKER "(")
  (define RIGHT-PATTERN-MARKER ")")
  (define IN-BETWEEN-PATTERN "([^/]*?)")
  
  ;; used in separation of path parts, like
  ;; app/db/ will get you. 
  ;; This is case insensitive
  ;; "(?i:^(.*?)(a)([^/]*?)(p)([^/]*?)(p)(.*?/.*?)(d)([^/]*?)(b)(.*?)$)"
  (define START-PATH-PART-REGEX "(?i:^(.*?)")
  (define IN-BETWEEN-PATH-PART-REGEX "(.*?/.*?)")
  (define END-PATH-PART-REGEX "(.*?)$)")
  
  ;; used for building the file regex
  (define END-FILE-REGEX "(.*)$)")
  
  ;; ignore patterns. A list of patters
  ;; for files that should not be included.
  ;; For default ignores unix style hidden files.
  (define PATTERNS-TO-IGNORE '(#px"^\\..*$"))
  
  ;; all files. Use reload if something changed
  (define ALL-FILES empty)
  
  ;; reload-files! : void -> void
  ;; use this for the side effect of
  ;; setting the ALL-FILES var to contain
  ;; all files in the current dir and below
  (define (reload-files!)
    (set! ALL-FILES (all-files "./")))
  
  ;; all-files : path-string -> (listof path-string)                    
  ;; fetches all the files in all the directories, starting with the root
  ;; passed. 
  (define (all-files (root-directory (current-directory)))
    (local [;; all-files-in-directories : (listof path-string) -> 
            ;;                                               (listof path-string)
            ;; retrieves all the files in a list of directories
            (define (all-files-in-directories directories)
              (cond
                [(empty? directories) empty]
                [else
                 (append (all-files (first directories))
                         (all-files-in-directories (rest directories)))]))]
      (append (files root-directory)
              (all-files-in-directories (subdirectories root-directory)))))
  
  
  (define (subdirectories current-dir)
    (get-sub-types current-dir directory-exists?))
  
  
  (define (files current-dir)
    (get-sub-types current-dir (λ (file-or-dir)
                                 (or (file-exists? file-or-dir)
                                     (link-exists? file-or-dir)))))
  
  ;; ignore? : file-path -> boolean
  ;; checks if a given file path is should be ignored
  (define (ignore? a-file)
    (local [(define (matches-any? patterns)
              (cond
                [(empty? patterns) false]
                [(let-values ([(base name must-be-a-dir?)
                               (split-path a-file)])
                   (or (and (path-string? base)
                            (regexp-match (first patterns)
                                          (path->string base)))
                       (and (path-string? name)
                            (regexp-match (first patterns) (path->string name)))))
                 true]
                [else (matches-any? (rest patterns))]))]
      (matches-any? PATTERNS-TO-IGNORE)))
  
  ;; get-sub-types : path-string (X -> boolean) -> (listof path-string)
  ;; returns only what the proc returns true in the current directory
  (define (get-sub-types a-dir what-to-get)
    (filter (λ (file-or-dir)
              (what-to-get file-or-dir))
            (map (λ (file-or-dir)
                   (build-path a-dir
                               file-or-dir))
                 (reverse (directory-list a-dir)))))
  
  
  ;; build-path-parts-regex : (listof path-string) -> string
  ;; builds a regexp that will provide a matching for the directory
  ;; part of a path. 
  ;; example: ('app 'db) will get you
  ;; "^(.*?)(a)([^/]*?)(p)([^/]*?)(p)(.*?/.*?)(d)([^/]*?)(b)(.*?)$"
  (define (build-path-parts-regex path-parts)
    (build-regex-from-string (string-join
                              (map (λ (path-part)
                                     (make-pattern path-part))
                                   path-parts)
                              IN-BETWEEN-PATH-PART-REGEX)
                             END-PATH-PART-REGEX))
  
  
  
  ;; build-file-regex : string -> string
  ;; builds a regex from a file name. Actually just a string
  (define (build-file-regex a-file)
    (build-regex-from-string (make-pattern a-file)
                             END-FILE-REGEX))
  
  
  ;; build-regex-from-string : string string -> string
  ;; builds a regex from any string. The ending must also
  ;; be provided.
  (define (build-regex-from-string string-to-be-regex ending)
    (string-append START-PATH-PART-REGEX
                   string-to-be-regex
                   ending))
  
  
  ;; make-pattern : string -> string
  ;; Takes the given pattern string "foo" and converts it to a new
  ;; string "(f)([^/]*?)(o)([^/]*?)(o)" that can be used to create
  ;; a regular expression.
  (define (make-pattern pattern)
    (local [(define (build-piece-of-patter piece)
              (string-append LEFT-PATTERN-MARKER
                             piece
                             RIGHT-PATTERN-MARKER))
            (define (build-the-pattern splitted-pattern accumulator)
              (cond
                [(and (not (empty? (first splitted-pattern)))
                      (empty? (rest splitted-pattern))) 
                 (string-append accumulator (build-piece-of-patter
                                             (first splitted-pattern)))]
                [else
                 (build-the-pattern (rest splitted-pattern)
                                    (string-append accumulator
                                                   (build-piece-of-patter
                                                    (first splitted-pattern))
                                                   IN-BETWEEN-PATTERN))]))]
      (build-the-pattern (regexp-split (regexp "") pattern)
                         "")))
  
  
  ;; search : string -> (listof match-result)
  ;; given a search query returns a list of match results
  ;  (define (search query)
  ;    $expr$ ---)
  
  ;; build-match-result : (listof string) number -> match-result
  ;; given a regexp-match result and the number of directories
  ;; used in the match, constructs a match-result struct.
  (define (build-match-result the-match0 number-of-folders)
    (local [;; analise-match   : (listof string) (listof run) number -> match-result
            ;; accumulates the matched chars and the runs and ultimately throws it all in the synthesize-result
            ;; so it can build a match result from the gathered data.
            (define (analise-match raw-match runs matched-chars index)
              (cond
                [(empty? raw-match) ;; returns the match-result
                 (synthesize-result runs matched-chars)]
                [(zero? (modulo index 2)) ;; here we have a match string
                 (analise-match (rest raw-match)
                                (update-runs runs
                                             (make-run (first raw-match)
                                                       true))
                                (+ matched-chars
                                   (string-length (first raw-match)))
                                (add1 index))]
                [else
                 (analise-match (rest raw-match)
                                (update-runs runs
                                             (make-run (first raw-match)
                                                       false))
                                matched-chars
                                (add1 index))])) ;; here we have the rest of the string
            
            ;; update-runs : (listof run) run -> (listof run)
            ;; check if this run and the last of the list 
            ;; are both inside, because if they are they actually are the same,
            ;; so join them.
            (define (update-runs runs a-run)
              (cond
                [(and (not (empty? runs))
                      (equal? (run-is-inside? (last runs))
                              (run-is-inside? a-run)))
                 (begin
                   (set-run-capture! (last runs)
                                     (string-append (run-capture (last runs))
                                                    (run-capture a-run)))
                   runs)]
                [(not (string=? "" (run-capture a-run)))
                 (append runs (list a-run))]
                [else runs]))
            
            
            ;; format-run : string -> string
            ;; nest the run inside run formatters
            (define (format-run a-run)
              (string-append LEFT-RUN-MARKER
                             (run-capture a-run)
                             RIGHT-RUN-MARKER))
            
            ;; remove-/ : string -> number
            ;; removes the '/' char, a la gsub, and
            ;; returns the length of the string
            ;; TODO: find a simpler way, like gsub
            (define (total-chars a-string)
              (string-length (list->string (remove* (list #\/)
                                                    (string->list a-string)))))
            
            
            ;; synthesize-result : (listof run) number -> match-result
            ;; returns a match result consisting of the score and 
            ;; the full path
            (define (synthesize-result runs matched-chars)
              (make-match-result
               (string-join (map (λ (a-run)
                                   (cond
                                     [(run-is-inside? a-run)
                                      (format-run a-run)]
                                     [else (run-capture a-run)]))
                                 runs) "")
               (exact->inexact
                (* (get-a-ratio (count (λ (a-run)
                                         (run-is-inside? a-run))
                                       runs)
                                number-of-folders)
                   (get-a-ratio (total-chars (first the-match0))
                                matched-chars)))))
            
            ;; get-a-ration : number number -> number
            ;; formulates a radio of 2 numbers.
            (define (get-a-ratio divisor base)
              (cond
                [(zero? divisor) 1]
                [else
                 (/ base divisor)]))]
      
      (cond
        [(or (empty? the-match0)
             (empty? (rest the-match0))
             (string=? "" (first the-match0)))
         (make-match-result "" 1)] ;; pretty sure it nothing
        [else (analise-match (rest the-match0) empty 0 1)])))
  
  
  )