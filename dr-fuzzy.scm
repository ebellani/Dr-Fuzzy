(module dr-fuzzy scheme
  (provide subdirectories 
           files
           all-files)
  
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
  
  ;; subdirectories : path-string -> (listof path-string)
  ;; returns only the subdirectories of the current directory
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
  
  )