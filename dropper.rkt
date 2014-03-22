#lang racket/gui

(define destination (string->path "/Users/spdegabrielle/Development/dropper/test"))
;"C:/importInput/"))
;"//eht-eds06v/importInput/"))

;
(define dropper% 
  (class frame% 
    ;(define drop-filename (string->path  "test.txt"))
    (define dropped-files-list '())
    
    ;;construct-drop-filename identifier subject suffix -> path
    ;;construct the filename for import
    (define/private (construct-drop-filename identifier subject suffix)
      (path-replace-suffix 
       (string->path 
        (string-append identifier "_" subject)) suffix)); identifier and original extention
    (define/private (verify-identifier identifier-string identifier-verification-string)
      (and (not (= (string-length identifier-string) 0))
      (equal? identifier-string identifier-verification-string)
      ))
    ;; add to list of filenames
    (define/override (on-drop-file pathname)
      (displayln pathname)
      (load-file pathname)
      (set! dropped-files-list (cons pathname dropped-files-list)))
    ;

    (define (load-file pathname)
      (let* (; the original extention
             (suffix (string-append "." (bytes->string/utf-8 (filename-extension pathname))))
             ; hospital number twice to reduce errors
             (identifier-string (send identifier-tf get-value)) 
             ; get doctors a hand held scanner
             (identifier-verification-string (send identifier-verification-tf get-value))
             
             (document-subject (send document-subject-tf get-value))
             (drop-filename (construct-drop-filename 
                             identifier-string
                             document-subject
                             suffix)); identifier and original extention
             )
        ;; if the identifiers match, copy the file to the import folder,
        (if (verify-identifier identifier-string identifier-verification-string)
            ;if equal, copy the file
            (let* ((dest (build-path destination drop-filename))
                   (src pathname))
              ;(send status-message set-label "Copied. Drop next file.")
              (copy-file src dest)
              drop-filename)
            ; if not equal
            (send status-message set-label 
                  (format "Not verified ~n~n~a is not equal to ~a"
                          identifier-string identifier-verification-string))
            )))
    
    (super-new)
    
    (define identifier-tf (new text-field%
                               (label "Hospital number")
                               (parent this)))
    (define identifier-verification-tf (new text-field%
                                            (label "Hospital number")
                                            (parent this)))
    ;Document subject
    (define document-subject-tf (new text-field%
                                     (label "Document Subject")
                                     (parent this)))
    (define send-to-gp (new check-box%
                            (parent this)
                            (label "to GP?")
                            (value #t)))
    
    (define drop-message (new message%
                              (parent this)
                              (label "Drop here")))
    
    (define status-message (new message%
                              (parent this)
                              [min-width 300]
                              (label "Drop here")))
    
    (define group-box-panel (new group-box-panel%
                                 (parent this)
                                 (label "Files sent: (check in epro)")))
    
    ))
(define dropper (new dropper% [label "Dropper"] [width 300][height 300]))
(send dropper accept-drop-files #t) 
(send dropper show #t) 