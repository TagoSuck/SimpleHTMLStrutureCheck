;; $ gosh htmlchecker.scm target.html
(define (begin-of-tag? c) (char=? c #\<))
(define (end-of-tag?   c) (char=? c #\>))
(define (tag-closing?  c) (char=? c #\/))
(define (car-of-tag tag) (car (reverse tag)))
(define (list->tag src) (list->string (reverse src)))
(define (tag-closing-of closing)
  (substring closing 1 (string-length closing)))
(define (string->tag s) (string-append "<" s ">"))
(define (string-empty? s) (zero? (string-length s)))
(define (match-tag regexp tag)
  (rxmatch->string (string->regexp regexp :case-fold #t) tag))

(define (any-single-tag? tag single-tag-regexps matched)
  (cond ((null? single-tag-regexps) (pair? matched))
        (else
          (let ((match (match-tag (car single-tag-regexps) tag)))
                (if (not (string? match))
                  (any-single-tag? tag (cdr single-tag-regexps) matched)
                  (cons match matched))))))

(define (single-tag? tag)
  (let ((regexps '("^!doctype( .*)*" ;; <!DOCTYPE>
                   "^meta( .*)*"     ;; <meta>
                   "^!--.*--$"       ;; <!-- COMMENT -->
                   "^br( .*)*"       ;; <br>
                   "^li( .*)*"       ;; <li>
                   "^img( .*)*"      ;; <img>
                   "^p( .*)*"        ;; <p>
                   )))
    (any-single-tag? tag regexps '())))

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
  (cond ((end-of-tag? (car src))
         (if (tag-closing? (car-of-tag tag))
           (cond ((and (pair? tags)
                       (string=? (tag-closing-of (list->tag tag)) (car tags)))
                  (tag-check (cdr src) (cdr tags)))
                 (else (put-error-msg tags tag)))
           (if (single-tag? (list->tag tag))
             (tag-check (cdr src) tags)
             (tag-check (cdr src) (cons (list->tag tag) tags)))))
        (else (tag-name (cdr src) tags (cons (car src) tag)))))

(define (tag-check src tags)
  (cond ((null? src) (if (null? tags) (print "ok") (put-error-msg tags '())))
        ((begin-of-tag? (car src)) (tag-name (cdr src) tags '()))
        (else (tag-check (cdr src) tags))))

(define (main args)
  (tag-check (read-file (cadr args)) '())
  0)

