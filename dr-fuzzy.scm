(module dr-fuzzy scheme
  (provide all-files
           make-pattern
           build-path-parts-regex
           build-file-regex
           build-match-result
           make-match-result
           reload-files!
           ignored?
           how-many-directories-up-to
           search
           clean-path
           add-match-results
           path->list)
  
  
  ;; match-result is a string representing the formatted 
  ;; text result, like "lib/c(ap)_(p)ool/"
  ;; score is the final weight that will be used to calculate 
  ;; the probability that this
  ;; result is the one that the user wants
  (define-struct match-result (path score) #:transparent)
  
  ;; a run is a piece of a match that represents the
  ;; different parts matched.
  ;; for example, the match "app" for 
  ;; the string "lib/cap_pool/" will produce
  ;; ((make-run ("test/c" false) ("ap" true) ("_" false) 
  ;;            ("p" true) ("ool" false)))
  ;; indicating what parts of the file was matched. 
  ;; So capture is the text itself like "test/c"
  ;; and is inside is if the text was used to capture the match. 
  ;; "test/c" was not, so it is
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
  
  ;; this will be used to exponentiate 10 to
  ;; truncate the result while calculating the score
  (define SCORE-PRECISION 4)
  
  ;; for portability reasons I must know what the separator is
  (define (FILE-SEPARATOR)
    (cond
      [(equal? (system-path-convention-type) 'windows) "\\"]
      [else "/"]))
  
  (define IN-BETWEEN-PATH-PART-REGEX (format "(.*?~a.*?)"
                                             (FILE-SEPARATOR)))
  
  (define END-PATH-PART-REGEX "(.*?)$)")
  
  ;; used for building the file regex
  (define END-FILE-REGEX "(.*)$)")
  
  ;; ignore patterns. A list of patters
  ;; for files that should not be included.
  ;; For default ignores unix style hidden files and
  ;; files ending in ~
  (define PATTERNS-TO-IGNORE '(#px"^.*\\/\\..*$"
                               #px"^.*~$"))
  
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
    (local [;; get-sub-types : 
            ;;     path-string (X -> boolean) -> (listof path-string)
            ;; returns only what the proc returns true in the current directory
            (define (get-sub-types a-dir what-to-get)
              (filter (λ (file-or-dir)
                        (what-to-get file-or-dir))
                      (map (λ (file-or-dir)
                             (build-path a-dir
                                         file-or-dir))
                           (reverse (directory-list a-dir)))))
            
            ;; subdirectories : path-string -> (listof path-string)
            ;; return all directories in a directory
            (define (subdirectories current-dir)
              (get-sub-types current-dir
                             directory-exists?))
            
            ;; files : path-string -> (listof path-string)
            ;; return all files in a directory
            (define (files current-dir)
              (get-sub-types current-dir
                             (λ (file-or-dir)
                               (and (file-exists? file-or-dir)
                                    (not (ignored? file-or-dir))))))
            
            ;; all-files-in-directories : 
            ;;   (listof path-string) -> (listof path-string)
            ;; retrieves all the files in a list of directories
            (define (all-files-in-directories directories)
              (cond
                [(empty? directories) empty]
                [else
                 (append (all-files (first directories))
                         (all-files-in-directories (rest directories)))]))]
      
      (append (files root-directory)
              (all-files-in-directories (subdirectories root-directory)))))
  
  
  ;; ignored? : file-path -> boolean
  ;; checks if a given file path is should be ignored
  (define (ignored? a-file)
    (local [(define (matches-any? patterns)
              (cond
                [(empty? patterns) false]
                [(and (path-string? a-file)
                      (regexp-match (first patterns)
                                    (path->string a-file)))
                 true]
                [else (matches-any? (rest patterns))]))]
      (matches-any? PATTERNS-TO-IGNORE)))
  
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
  
  ;; path->list : path-string -> (listof string)
  ;; transform a file path in a list representing it.
  ;; Used to build the regexp for a path
  ;; Ex: ("./app/db") -> '("app" "db")
  (define (path->list the-path)
    (filter (λ (path-part)
              (and (not (string=? "" path-part))
                   (not (string=? "." path-part))))
            (regexp-split (FILE-SEPARATOR)
                          (path->string the-path))))
  
  ;; how-many-directories-up-to : path-string -> number
  ;; find how many directories there are in the file
  (define (how-many-directories-up-to a-file)
    (operation-on-path (λ (path-part accumulator)
                         (add1 accumulator))
                       0
                       a-file))
  
  ;; clean-path : path-string -> string
  ;; used to clean a path of signs of
  ;; things like "./"
  ;; For example "./EXAMPLE.txt" -> "EXAMPLE.txt"
  ;; "./app/db/wee.txt" -> "app/db/wee.txt"
  ;; returns empty if the tring is blank
  (define (clean-path the-path)
    (operation-on-path (λ (path-part accumulator)
                         (string-append (path->string path-part)
                                        (FILE-SEPARATOR)
                                        accumulator))
                       (let ([filename
                              (file-name-from-path the-path)])
                         (cond
                           [(false? filename) ""]
                           [else (path->string filename)]))
                       the-path))
  
  ;; operation-on-path : (X X -> X) X path-string -> (listof X)
  (define (operation-on-path operation initial the-path0)
    (local [(define (operation-on-path-acc the-path accumulator)
              (let-values ([(base name must-be-dir?) (split-path the-path)])
                (cond
                  [(or (equal? name 'same)
                       (equal? base 'relative))
                   accumulator]
                  [(false? must-be-dir?)
                   (operation-on-path-acc base
                                          accumulator)]
                  [else
                   (operation-on-path-acc base
                                          (operation name
                                                     accumulator))])))]
      (operation-on-path-acc the-path0 initial)))
  
  
  ;; add-match-results : match-result match-result -> match-result
  ;; sums 2 match results. Used to sum the match of the path
  ;; with the match of the file
  (define (add-match-results match-path match-file)
    (local [;; format-result-path : (listof string) -> string
            ;; receives the path splitted by the separator
            ;; the verifies if the path is enclosed in parens.
            ;; if it is, use all the path part, if there is not, use the first 
            ;; letter of the path
            (define (format-result-path path-parts accumulator)
              (cond
                [(empty? path-parts) accumulator]
                [(string=? "" (first path-parts))
                 (format-result-path (rest path-parts)
                                     accumulator)]
                [(equal? (string-ref (first path-parts) 0) #\()
                 (format-result-path (rest path-parts)
                                     (string-append accumulator
                                                    (first path-parts)
                                                    (FILE-SEPARATOR)))]
                [else
                 (format-result-path (rest path-parts)
                                     (string-append
                                      accumulator
                                      (string (string-ref (first path-parts) 0))
                                      (FILE-SEPARATOR)))]))]
      (cond
        [(string=? "" (match-result-path match-path)) match-file]
        [else
         (make-match-result
          (string-append
           (format-result-path (regexp-split (pregexp (FILE-SEPARATOR))
                                             (match-result-path match-path))
                               "")
           (match-result-path match-file))
          (* (match-result-score match-path)
             (match-result-score match-file)))])))
  
  
  ;; search : string -> (listof match-result)
  ;; given a search query returns a list of match results
  (define (search query)
    (local [;; separates a regexp for the path part of the query, if any
            ;; returns empty if there is none, as in "./EXAMPLE.txt"
            (define path-regexp
              (cond
                [(or (string=? "" query)
                     (false? (path-only query))) empty]
                [else 
                 (build-path-parts-regex (path->list
                                          (path-only query)))]))
            
            ;; separates a regexp for the file part of the query
            ;; in the case this is not a file, returns empty.
            (define file-regexp
              (cond
                [(string=? "" query) empty]
                [else
                 (let
                     ([filename (file-name-from-path query)])
                   (cond
                     [(false? filename) empty]
                     [else
                      (build-file-regex (path->string filename))]))]))
            
            ;; build-result : string regexp -> match-result or false
            ;; creates the result for the given path. Returns false if
            ;; there is no match
            (define (build-result path regexp)
              (cond
                [(or (string=? path "")
                     (empty? path)
                     (empty? regexp)) false]
                [else
                 (let ([the-match (regexp-match regexp path)])
                   (cond
                     [(false? the-match) false]
                     [else
                      (build-match-result
                       (regexp-match regexp path)
                       (how-many-directories-up-to path))]))]))
            
            (define (search-all-files files accumulator)
              (cond
                [(empty? files) accumulator]
                [else
                 (let*-values ([(base name must-be-dir?)
                                (split-path (first files))]
                               [(path-result)
                                (cond
                                  [(empty? path-regexp)
                                   (make-match-result (clean-path base)
                                                      1.0)]
                                  [else
                                   (build-result (clean-path base)
                                                 path-regexp)])])
                   (cond
                     [(false? path-result)
                      (search-all-files (rest files)
                                        accumulator)]
                     [else
                      (let ([file-result
                             (build-result (clean-path name) file-regexp)])
                        (cond
                          [(false? file-result)
                           (search-all-files (rest files)
                                             accumulator)]
                          [else
                           (search-all-files
                            (rest files)
                            (cons (add-match-results path-result
                                                     file-result)
                                  accumulator))]))]))]))]
      (search-all-files ALL-FILES empty)))
  
  ;; build-match-result : (listof string) number -> match-result
  ;; given a regexp-match result and the number of directories
  ;; used in the match, constructs a match-result struct.
  (define (build-match-result the-match0 number-of-folders)
    (local [;; analise-match : 
            ;;   (listof string) (listof run) number -> match-result
            ;; accumulates the matched chars and the runs and ultimately 
            ;; throws it all in the synthesize-result
            ;; so it can build a match result from the gathered data.
            (define (analise-match raw-match runs matched-chars index)
              (cond
                [(empty? raw-match) 
                 (synthesize-result runs matched-chars)]
                [(zero? (modulo index 2)) 
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
                                (add1 index))]))
            
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
               (* (get-a-ratio (count (λ (a-run)
                                        (run-is-inside? a-run))
                                      runs)
                               (add1 number-of-folders))
                  (get-a-ratio (total-chars (first the-match0))
                               matched-chars))))
            
            ;; get-a-ration : number number -> number
            ;; formulates a radio of 2 numbers.
            (define (get-a-ratio divisor base)
              (cond
                [(zero? divisor) 1]
                [else
                 (/ base divisor)]))]
      
      (cond
        [(or (empty? the-match0)
             (false? the-match0)
             (string=? "" (first the-match0)))
         (make-match-result "" 1)] ;; pretty sure it is nothing
        [else (analise-match (rest the-match0) empty 0 1)])))
  
  (reload-files!))