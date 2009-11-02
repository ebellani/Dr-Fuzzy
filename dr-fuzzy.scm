(module dr-fuzzy scheme
  (provide subdirectories 
           files
           all-files
           make-pattern
           build-path-parts-regex
           build-file-regex)
  
  
  ;; used in the separation into fragments
  ;; like "foo" -> (f)([^/]*?)(o)([^/]*?)(o)
  (define LEFT-PATTERN-MARKER "(")
  (define RIGHT-PATTERN-MARKER ")")
  (define IN-BETWEEN-PATTERN "([^/]*?)")
  
  ;; used in separation of path parts, like
  ;; app/db/ will get you
  ;; "^(.*?)(a)([^/]*?)(p)([^/]*?)(p)(.*?/.*?)(d)([^/]*?)(b)(.*?)$"
  (define START-PATH-PART-REGEX "^(.*?)")
  (define IN-BETWEEN-PATH-PART-REGEX "(.*?/.*?)")
  (define END-PATH-PART-REGEX "(.*?)$")
  
  ;; used for building the file regex
  (define END-FILE-REGEX "(.*)$")
  
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
  
  )