#lang racket/gui

(define destination (string->path "C:/importInput/"))
                     ;"//eht-eds06v/importInput/"))
(define dropper% 
  (class frame% 
    (define drop-filename (string->path  "test.txt"))
    (define/override (on-drop-file pathname)
      (displayln pathname)
      
      ;construct the filename for import
      (let* ((suffix (string-append "." (bytes->string/utf-8 (filename-extension pathname)))); the original extention
             (identifier-string (send identifier-tf get-value)) ; hospital number twice to reduce errors
             (identifier-verification-string (send identifier-verification-tf get-value)); get doctors a hand held scanner
             (document-subject (send document-subject-tf get-value))
             (drop-filename (path-replace-suffix (string->path 
                                                  (string-append identifier-string "_" document-subject)
                                                  ) suffix)); identifier and original extention
             )
        ;; if the identifiers match, copy the file to the import folder,
        (if (equal? identifier-string identifier-verification-string)
            ;if equal, copy the file
            (let* ((dest (build-path destination drop-filename))
                   (src pathname))
              ;(send status-message set-label "Copied. Drop next file.")
              (copy-file src dest))
            ; if not equal
            (send status-message set-label 
                  (format "Not verified ~n~n~a is not equal to ~a"  identifier-string identifier-verification-string))
            )))
    (super-new)
    
    
    (define identifier-tf (new text-field%
                               (label "Hospital number")
                               (parent this)))
    (define identifier-verification-tf (new text-field%
                                            (label "Hospital number")
                                            (parent this)))
    ;_Document subject
    (define document-subject-tf (new text-field%
                                     (label "Document Subject")
                                     (parent this)))
    (define status-message (new message%
                                (parent this)
                                (label "                                  ")))
    (define drop-message (new message%
                              (parent this)
                              (label "Drop here")))
    
    
    ))
(define dropper (new dropper% [label "Dropper"] [width 300][height 300]))
(send dropper accept-drop-files #t) 
(send dropper show #t) 