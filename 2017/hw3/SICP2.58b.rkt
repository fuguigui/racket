#lang racket
(define (number? exp)
  (if (pair? exp)
      #f
      (not (symbol? exp))))
(define (variable? exp)
  (if (pair? exp)#f
      (symbol? exp)))
(define (same-variable? exp var)
  (if (variable? exp)
      (eq? exp var)
      #f))
(define (search-loop op lst)
  (if (null? lst) lst
      (if (eq? (car lst) op)
           lst
          (search-loop op (cdr lst)))))
(define (cutexp old cutout)
  (if (eq? old cutout)
      '()
      (cons (car old) (cutexp (cdr old) cutout))))
(define (sum? exp)
  (if (pair? exp)
      (if (pair? (car exp))
          (sum? (cdr exp))
          (if (eq? (car exp) '+)#t
              (sum? (cdr exp))))
      #f))
(define (addend exp)
  (define add (cutexp exp (search-loop '+ exp)))
  (if(null? (cdr add))
     (car add)
     add))
(define (augend exp)
  (define aug (cdr (search-loop '+ exp)))
    (if (null? (cdr aug))
        (car aug)
        aug))
(define (make-sum add aug)
  (cond ((and (number? add)
              (number? aug))
          (+ add aug))
         ((eq? add 0)
          aug)
         ((eq? aug 0)
          add)
         ((pair? aug)
          (if (pair? add)
              (append add (list '+) aug)
              (append (list add '+) aug)))
         ((pair? add)
         (append add (list '+ aug)))
         (else (list add '+ aug))))
(define (product? exp)
  (if (pair? exp)
      (if (pair? (car exp))
          (product? (cdr exp))
          (if (eq? (car exp) '*)#t
              (product? (cdr exp))))
      #f))
(define (multiplier exp)
  (define nexp (cutexp exp (search-loop '* exp)))
  (if (null? (cdr nexp))
      (car nexp)
      nexp))
(define (multiplicand exp)
  (define nexp(cdr (search-loop '* exp)))
  (if (null? (cdr nexp))
      (car nexp)
      nexp))
(define (make-product plier plicand)
   (cond ((and (number? plier)
              (number? plicand))
          (* plier plicand))
         ((eq? plier 0)0)
         ((eq? plicand 0)0)
         ((eq? plier 1)plicand)
         ((eq? plicand 1)plier)
         ((pair? plicand)
          (list plier '* plicand))
         ((pair? plier)
          (list plier '* plicand))
         (else (list plier '* plicand))))

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))


(myloop)
;0
;x
;(x * 3)
;(y + x)
;(y * x)
;(x * x  + x * y)
;(x * x * x)
;(x * x * y)
;(x * x + x * x * y * (x + 3) + x * y)