#lang racket
;功能强大的my-map，要求参数个数可变
(require r5rs)

(define env (scheme-report-environment 5))
(eval
 '(define (my-map proc . args)
    (define (iter proc args)
      (define non-null-args (clean args))
      (if (null? non-null-args)
          '()
          (cons (proc-list proc (map car non-null-args))
                (iter proc (map cdr non-null-args)))))
    (iter proc args))
 env)
(eval
 ' (define (proc-list proc args)
      ;args以list的形式存在,保证args不为空,且均为有意义的值
        (if(null? (cdr args))
            (proc (car args))
            (proc (car args)
                  (proc-list proc (cdr args)))))
 env)
(eval
 '(define (clean args)
    ;清洗参数列表，如果列表里有'()，就跳过它
    (if(null? args)
       '()
       (if (null? (car args))
           (clean (cdr args))
           (cons (car args)
                 (clean (cdr args))))))
 env)

(define (myloop)
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin  (displayln (eval codes env)) (myloop)))))


(myloop)