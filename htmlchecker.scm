;; $ gosh htmlchecker.scm target.html
(define (begin-of-tag? c) (char=? c #\<))
(define (end-of-tag?   c) (char=? c #\>))
(define (tag-closing?  c) (char=? c #\/))
(define (car-of-tag tag) (car (reverse tag)))
(define (list->tag src) (list->string (reverse src)))
(define (tag-closing-of closing)
  (substring closing 1 (string-length closing)))
(define (string->tag s) (string-append "<" s ">"))

(define (single-tag? tag)
  (pair? (filter
           (lambda (s) (string-ci=? tag s))
           '(
             "!doctype"
             "meta"
             "script"
             "link"
             "br"
             "li"
             "img"
             "p"
             "dt"
             "dd"
             "hr"
             ))))

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop ((lst '()) (c (read-char p)))
      (cond ((eof-object? c) (close-input-port p)
                             (reverse lst))
            (else (loop (cons c lst) (read-char p)))))))

(define (print-tag tags)
  (cond ((null? tags) #f)
        (else (print (string->tag (car tags)))
              (print-tag (cdr tags)))))

(define (put-error-msg tags tag)
  (print (string-append "error: "
                        (if (pair? tags) (string->tag (car tags)) (string->tag ""))
                        " is not closed around before "
                        (if (pair? tag) (string->tag (list->tag tag)) (string->tag ""))
                        "."))
  (print "\ntag stack")
  (print "----------------")
  (print-tag tags))

(define (tag-name src tags tag)
  (cond ((null? src) #f)
        ((string=? (list->tag tag) "!--") (tag-check (cdr src) tags))
        ((or (end-of-tag? (car src)) (char-whitespace? (car src)))
         (if (tag-closing? (car-of-tag tag))
           (cond ((null? tags) (put-error-msg tags tag))
                 ((string=? (tag-closing-of (list->tag tag)) (car tags))
                  (tag-check (cdr src) (cdr tags)))
                 ((single-tag? (car tags)) (tag-name src (cdr tags) tag))
                 (else (put-error-msg tags tag)))
           (tag-check (cdr src) (cons (list->tag tag) tags))))
        (else (tag-name (cdr src) tags (cons (car src) tag)))))

(define (tag-check src tags)
  (cond ((null? src) (if (null? tags)
                       (print "ok")
                       (if (single-tag? (car tags))
                         (tag-check src (cdr tags))
                         (put-error-msg tags '()))))
        ((begin-of-tag? (car src)) (tag-name (cdr src) tags '()))
        (else (tag-check (cdr src) tags))))

(define (main args)
  (tag-check (read-file (cadr args)) '())
  0)

