#! /usr/bin/chez-scheme --program

;; TODO / Ideas:
;; - Auto commit using title.
;; - Do not write empty entries, maybe write to temp file first?

(import
  (chezscheme)
  (irregex))

;; Markdown journal entries.
;; Sample format:
;; # Wednesday, 2020-03-11
;; ## 11:24 Title

(define white/green
  (list->string `(#\esc #\[ #\3 #\7 #\; #\4 #\2 #\m)))
(define white/red
  (list->string `(#\esc #\[ #\3 #\7 #\; #\4 #\1 #\m)))
(define normal
  (list->string `(#\esc #\[ #\0 #\m)))

(define identity
  (lambda (x)
    x))

(define editor
  (cond
    [(getenv "VISUAL") => identity]
    [(getenv "EDITOR") => identity]
    [else "vi"]))

(define pager
  (cond
    [(getenv "PAGER") => identity]
    [else editor]))

(define base-dir
  (cond
    [(getenv "OZZYDIR") => identity]
    [else "~/journal"]))

(define dmy->date
  (lambda (dmy)
    (guard (e [else #f])
      (apply make-date 0 0 0 0 dmy))))

(define dmy-
  (lambda (dmy)
    (let ([dd (car dmy)]
          [mm (cadr dmy)]
          [yy (caddr dmy)])
      (if (= (- dd 1) 0)
          (if (= (- mm 1) 0)
              ;; lower the year.
              `(31 12 ,(- yy 1))
              ;; lower the month.
              `(31 ,(- mm 1) ,yy))
          `(,(- dd 1) ,mm ,yy)))))

(define date--
  (lambda (d)
    (let loop ([dmy (dmy- `(,(date-day d) ,(date-month d) ,(date-year d)))])
      (cond
        [(dmy->date dmy) => identity]
        [else
          (loop (dmy- dmy))]))))

(define make-date-series
  (lambda (d count)
    (let loop ([acc `(,d)])
      (cond
        [(= (length acc) count)
         acc]
        [else
          (loop (cons (date-- (car acc)) acc))]))))

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

(define entry-exists?
  (lambda (d)
    (file-exists? (make-journal-path d))))

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

;; Edit/view an existing entry.
(define ozzy-view-entry
  (case-lambda
    [()
     (ozzy-view-entry (current-date))]
    [(d)
     (let* ([fp (make-journal-path d)])
       (if (file-exists? fp)
           (system (string-append "exec " pager " " fp))
           (error #f "Entry does not exist" d)))]))

(define ozzy-show-recent
  (lambda (days)
    (for-each
      (lambda (d)
        (let ([exists? (entry-exists? d)])
          (when exists?
            (display white/green))
          (display (date->string d))
          (when exists?
            (display normal)
            (display " ")
            (display (last (get-titles d))))
          (newline)
          ))
      (make-date-series (current-date) days))))

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

;; [proc] get-titles date: get the list of titles in an entry.
(define get-titles
  (lambda (d)
    (let ([p (irregex '(: "##" whitespace num num ":" num num (+ whitespace)
                          (submatch-named title (* any))))])

      (filter
        (lambda (x) (if x #t #f))
        (map
          (lambda (line)
            (let ([m (irregex-search p line)])
              (if (and m (irregex-match-valid-index? m 'title))
                  (irregex-match-substring m 'title)
                  #f)))
          (slurp (make-journal-path d)))))))

(define get-arg/default
  (lambda (i default)
    (guard (e [else default])
      (list-ref (command-line-arguments) i))))

(define arg->date
  (lambda (i)
    (let ([arg (get-arg/default i "today")])
      (case (string->symbol arg)
        [(today)
         (current-date)]
        [(yesterday)
         (car (make-date-series (current-date) 2))]
        [else
          (case (string->number arg)
            [(0)
             (current-date)]
            [else
              (car (make-date-series (current-date) (+ 1 (string->number arg))))])]))))

(define last
  (lambda (lst)
    (car (reverse lst))))

;; [proc] slurp: Read all lines from a text file.
;; Name is akin to the perl function.
;; All lines of a file are returned as a list with newlines removed.
(define slurp
  (lambda (path)
    (let ([f (open-file-input-port
               path
               (file-options no-create)
               (buffer-mode line)
               (make-transcoder (utf-8-codec)))])
      (let loop ([line (get-line f)] [lines '()])
        (cond
          [(eof-object? line)
           (close-input-port f)
           (reverse lines)]
          [else
            (loop (get-line f) (cons line lines))])))))

(cond
  [(null? (command-line-arguments))
   (show-help 1)]
  [else
    (case (string->symbol (car (command-line-arguments)))
      [(edit view e v)
       (ozzy-view-entry (arg->date 1))]
      [(new add n a)
       (ozzy-add-entry (arg->date 1))]
      [(recent r)
       (ozzy-show-recent (string->number (get-arg/default 1 "24")))]
      [(help h)
       (show-help 0)]
      [(info i)
       (show-config)]
      [else
        (show-help 1)])])
