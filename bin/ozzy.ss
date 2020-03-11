#! /usr/bin/chez-scheme --program

(import
  (chezscheme))

;; Markdown journal entries.
;; Sample format:
;; # Wednesday, 2020-03-11
;; ## 11:24 Title

(define identity
  (lambda (x)
    x))

(define editor
  (cond
    [(getenv "VISUAL") => identity]
    [(getenv "EDITOR") => identity]
    [else "vi"]))

(define base-dir
  (cond
    [(getenv "OZZYDIR") => identity]
    [else "~/journal"]))

(define day-names
  (map
    symbol->string
    '(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)))

;; [proc] date->string date [separator]: create a filename from date object.
;; Date format for returned string is "YYYY-MM-DD", zero padded.
;; Hyphen "-" is used for the separator if none is provided.
;; eg, (date->string (current-date)) -> "2020-03-11".
(define date->string
  (case-lambda
    [(d)
     (date->string d "-")]
    [(d sep)
     (format
       "~d~a~2,'0d~a~2,'0d"
       (date-year d)
       sep
       (date-month d)
       sep
       (date-day d))]))

(define date->header
  (lambda (d)
    (string-append "# "
                   (list-ref day-names (date-week-day d))
                   ", "
                   (date->string d "/"))))

(define make-journal-path
  (lambda (d)
    (string-append
      base-dir
      "/"
      (date->string d "")
      ".md")))

(define entry-title
  (lambda (d)
    (format
      "## ~2,'0d:~2,'0d " 
      (date-hour d) (date-minute d))))

;; Add a new entry.
(define ozzy-add-entry
  (case-lambda
    [()
     (ozzy-add-entry (current-date))]
    [(d)
     ;; Make leading directory if necessary.
     (unless (file-directory? base-dir)
       (mkdir base-dir))
     (let* ([fp (make-journal-path d)]
            [add-header? (not (file-exists? fp))])
       (call-with-output-file fp
         (lambda (port)
           (parameterize ([current-output-port port])
             (when add-header?
               (show-nl
                 (date->header d)))
             (show-nl
               ""
               (entry-title d))))
         'append)
       (system (string-append "exec " editor " " fp)))]))

(define-syntax show-nl
  (syntax-rules ()
    [(_ x x* ...)
     (begin
       (display x)(newline)
       (begin
         (display x*)(newline)) ...)]))

(define show-config
  (lambda ()
    (show-nl
      (make-journal-path (current-date))
      editor)))

(define show-help
  (lambda (rc)
    (write (command-line))
    (newline)
    (exit rc)))

(cond
  [(null? (command-line-arguments))
   (show-help 1)]
  [else
    (case (string->symbol (car (command-line-arguments)))
      [(new add n a)
       (ozzy-add-entry)]
      [(help h)
       (show-help 0)]
      [(info i)
       (show-config)]
      [else
        (show-help 1)])])
