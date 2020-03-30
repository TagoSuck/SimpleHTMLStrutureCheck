(define (begin-of-tag? c) (char=? c #\<))
(define (end-of-tag? c) (char=? c #\>))
(define (tag-closing? c) (char=? c #\/))
(define (car-of-tag tag) (car (reverse tag)))
(define (list->tag src) (list->string (reverse src)))
(define (tag-closing-of closing)
  (substring closing 1 (string-length closing)))
(define (string->tag s) (string-append "<" s ">"))
(define (single-tag? tag)
  (not (null? (filter (lambda (s) (string=? s tag)) '("li" "img" "meta" "br")))))

(define (print-tag tags)
  (cond ((null? tags) #f)
        (else (print (string->tag (car tags)))
              (print-tag (cdr tags)))))

(define (put-error-msg tags tag)
  (print (string-append "error: "
                        (string->tag (car tags))
                        " is not closed around before "
                        (string->tag (list->tag tag))
                        "."))
  (print "\ntag stack")
  (print "----------------")
  (print-tag tags))

(define (tag-name src tags tag)
  (cond ((end-of-tag? (car src))
         (if (tag-closing? (car-of-tag tag))
           (cond ((string=? (tag-closing-of (list->tag tag)) (car tags))
                  (tag-check (cdr src) (cdr tags)))
                 (else (put-error-msg tags tag)))
           (if (single-tag? (list->tag tag))
             (tag-check (cdr src) tags)
             (tag-check (cdr src) (cons (list->tag tag) tags)))))
        (else (tag-name (cdr src) tags (cons (car src) tag)))))

(define (tag-check src tags)
  (cond ((null? src) (if (null? tags) (print "ok") #f))
        ((begin-of-tag? (car src)) (tag-name (cdr src) tags '()))
        (else (tag-check (cdr src) tags))))

(define (main args)
  (tag-check
    (string->list "<html><head><meta></head><body><h1>hello</h1><div></body></html>")
    ;; => error
    ;(string->list "<html><head><meta></head><body><h1>hello</h1></body></html>")
    ;; => ok
    '())
  0)

