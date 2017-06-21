#lang racket
;review ch03-04 流
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

#|(eval '(define (stream-map proc . argstreams)
         (if(stream-null? argstreams)
            the-empty-stream
            (cons-stream (apply proc (map stream-car argstreams))
                         (apply stream-map (cons proc (map (stream-cdr argstreams)))))))
      env)|#
(eval '(define (stream-map proc . argstreams)
         (define (stream-map-list proc streams)
           (if (stream-null? streams)
               the-empty-stream
               (cons-stream (apply proc (map stream-car streams))
                            (stream-map-list proc (map stream-cdr streams)))))
         (stream-map-list proc argstreams))
      env)
(eval '(define (add-stream s1 s2)
         (stream-map + s1 s2))
      env)
(eval '(define ones (cons-stream 1 ones))
      env)
;(eval '(define integers
 ;        (cons-stream 1 (add-stream ones integers)))
  ;    env)
#|
;测试输入
(
(define (displayln x) 
  (display x) (newline))

(define integers(cons-stream 1 (add-stream ones integers)))
(displayln (stream-ref integers 3)) 
(displayln (stream-ref integers 4)) 

'ok
)
;测试fib的流形式
(
(define (displayln x)
(display x)
(newline))
(define fibs
(cons-stream 0 
(cons-stream 1
(add-stream (stream-cdr fibs)
fibs))))
(displayln (stream-ref fibs 3))
(displayln (stream-ref fibs 5))
'ok)
|#
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