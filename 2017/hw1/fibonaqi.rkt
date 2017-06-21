#lang racket
;求斐波那契数列：迭代算法
(define v (make-vector 51 0))
(define (initial n)
  (begin(vector-set! v n 1)
  (if(= 0 n)
      (void)
     (initial (- n 1)))))

(define (f n)
  (define tmp (vector-ref v n))
  (if(= tmp 0)
     (begin
       (set! tmp (+
                    (f (- n 1))
                    (* 4 (f (- n 2)))
                    (* 5 (f (- n 3)))
                    (* -2 (f (- n 4)) (f (- n 4)))
                    (* (f (- n 5)) (f (- n 5)) (f (- n 5)))
                    ))
       (vector-set! v n tmp)
       tmp)
     tmp))

(define (loop)
  (define n (read))
  (if(equal? n eof)
     (void)
     (begin (display (f n)) (newline) (loop))))
(initial 4)
(loop)
