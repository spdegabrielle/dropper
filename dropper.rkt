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
    (define/private (construct-drop-filename identifier type gp subject suffix)
      ;3)       Type
      ;This represents the type of the item you are importing. 
      ;;If you are importing a discharge summary, use the value DS. 
      ;;If you are importing a letter, use LETTER. 
      ;;If you are importing an A&E summary, use AE. 
      ;;If you would like the document to only appear in the ‘All’ documents tab, use ALL. 
      ;;If no value is provided, then a Type of ALL will be assumed.
      (define type-s (case type
                       [(0) "_ALL"] ;all
                       [(1) "_A&E"] ;a&E
                       [(2) "_DS"] ;discharge
                       [(3) "_LETTER"] ;letter
                       ))
      (define gp-s (if gp "_GP" ""))
      (define subject-s subject)
      
      
      (path-replace-suffix 
       (string->path 
        (string-append identifier type-s gp-s subject-s)) suffix)); identifier and original extention
    
    ;; verify the identifiers are equal and non-empty - can add more validation here.
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
             (type (send type-rb get-selection))
             (send-to-gp? (send send-to-gp get-value))
             
             (document-subject (send document-subject-tf get-value))
             (drop-filename (construct-drop-filename 
                             identifier-string
                             type
                             send-to-gp?
                             document-subject
                             suffix)); identifier and original extention
             )
        ;; if the identifiers match, copy the file to the import folder,
        (send send-to-gp set-value #f)
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
    
    (super-new  [label "Dropper"] [width 400][height 400])
    (send this accept-drop-files #t) 
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
                            (label "Send to to GP?")
                            (value #f)))
    
    (define type-rb (new radio-box%
                           (label "Document type")
                           (parent this)
                           [selection 0]
                           (choices (list 
                                     "general/other";0
                                     "A&&E" ;1
                                     "Discharge";2
                                     "LETTER";3
                                     ))))
    
    
    (define drop-message (new message%
                              (parent this)
                              (label "Drop here")))
    
    
    
    (define group-box-panel (new group-box-panel%
                                 (parent this)
                                 (label "Files sent: (check in epro)")))
    (define status-message (new message%
                                (parent group-box-panel)
                                [min-width 250]
                                (label "status")))
    
    ))
(define dropper (new dropper%))
;(send dropper accept-drop-files #t) 
(send dropper show #t) 