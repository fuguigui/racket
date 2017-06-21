#lang racket
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
  (if (null? (car w))
      '()
      (cons (apply op (map car w))
            (apply super-map (cons op (map cdr w))))))
  

(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin
          ;(displayln (list a b c))
          ;(displayln (apply + (map car (list a b c))))
         ; (displayln (map car(map cdr (list a b c))))

        (displayln (super-map + a b c)) 
         ;      (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)
