#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
      (begin
      (define recorded '())
      (define (in-recorded? l)
        (define (search-loop ref aim)
          (cond ((null? ref) #f)
                ((equal? (car ref) aim) #t)
                (else (search-loop (cdr ref)aim))))
        (search-loop recorded l))
      (define (add-to-recorded l)
        (set! recorded (cons l recorded)))
      (define (check-cycle llist)
        (define (check-loop l)
          (cond((null? l)#f);#f 代表没有循环
               ((in-recorded? l) #t)
               (else
                (begin
                  (add-to-recorded l)
                  (check-loop (cdr l))))))
        (check-loop llist)))
      env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)