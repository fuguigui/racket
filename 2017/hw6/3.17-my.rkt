#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (begin
   (define counted-pairs '())
   (define (not-counted? p)
       (define (search? p countpairs);能否在已数过的Pairs里找到当前的p
         (cond ((null? countpairs)#f)
               ((eq? p (car countpairs))#t)
               (else (search? p (cdr countpairs)))))
       (not (search? p counted-pairs)))
   (define (save-in-counted p)
     (set! counted-pairs (cons p counted-pairs)))
   (define (count-pairs x)
     
     (if (not(pair? x))
         0
         (if(not-counted? x)
            (begin
              (save-in-counted x)
              (+ 1
                 (count-pairs (car x))
                 (count-pairs (cdr x))))
            0))))
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