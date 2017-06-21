#lang racket
(define (square x) (* x x))
(define (divisible? x y ) (= (remainder x y ) 0))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
 
(define the-empty-stream '())
 
(define (stream-null? stream)
  (null? stream))

(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
 
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) 
                   (stream-map proc (stream-cdr s)))))


; 在此处补充你的代码

(define (merge-weighted s1 s2 weight-func)
  (cond((stream-null? s1) s2)
       ((stream-null? s2) s1)
       (else
        (let((s1w (weight-func(stream-car s1)))
             (s2w (weight-func(stream-car s2))))
          (if(< s1w s2w)
             (cons-stream (stream-car s1)(merge-weighted (stream-cdr s1)s2 weight-func))
             (cons-stream (stream-car s2)(merge-weighted s1 (stream-cdr s2) weight-func)))))))
(define (weighted-pairs s1 s2 weight-func)
  (cons-stream (list(stream-car s1)(stream-car s2))
               (merge-weighted
                (stream-map (lambda (x)(list(stream-car s1) x)) (stream-cdr s2))
                (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight-func)
                 weight-func)))
(define (Ramanujan l)
  (let((first (stream-car l))
       (second (stream-car(stream-cdr l))))
    (if(= (weight3 first)(weight3 second))
       (cons-stream (weight3 first)(Ramanujan (stream-cdr (stream-cdr l))))
       (Ramanujan (stream-cdr l)))))



(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define (cube x)  (* x x x))
(define weight3 (lambda (x) (+ (cube (car x)) (cube (cadr x)))))
(define lst (weighted-pairs integers integers weight3)) 
(define result-stream  (Ramanujan lst))



(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (displayln (stream-ref result-stream n)) (myloop)))))

(myloop)