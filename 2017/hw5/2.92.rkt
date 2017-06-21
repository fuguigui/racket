#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;----------我的部分---------
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
;---------terms的相关操作
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))
  ;------------多项式的相关操作
 
  (define (add-poly p1 p2);给出的是不带标志的多项式
    (let((v1 (variable p1))
         (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1
                 (add-terms (term-list p1)
                            (term-list p2)))
          (if (higher? v1 v2)
              (add-poly p1 (list v1 (list 0 (cons 'polynomial p2))))
              (add-poly p2 (list v2 (list 0 (cons 'polynomial p1))))))))
      
  (define (mul-poly p1 p2)
    (let((v1 (variable p1))
         (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1
                 (mul-terms (term-list p1)
                            (term-list p2)))
          (if (higher? v1 v2)
              (mul-poly p1 (list v1 (list 0(cons 'polynomial p2))))
              (mul-poly p2 (list v2 (list 0(cons 'polynomial p1))))))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))))
 (define (higher? v1 v2)
    (define alphabet (list 'a 'b 'c 'd 'e))
    (define (find v alpha)
      (cond((null? alpha)1)
           ((eq? v (car alpha)) 1)
           (else (+ 1 (find v (cdr alpha))))))
    (define o1 (find v1 alphabet))
    (define o2 (find v2 alphabet))
    (< o1 o2))


(define (integer->poly var int);把带标志的整数变为带标志的零次多项式，变量给定
  (list 'polynomial var (list 0 int)))

(define (display-poly p);把带标志的poly转换成list，然后输出整个list
  (define (poly? term);对于给定的项进行判断（0,('polynomial a (...))）
    (if(eq? (caadr term) 'polynomial)
       #t
       #f))
  (define (termlist->list tlist);把termlist转换成去掉integer标志的termlist
    (cond((null? tlist)'())
         ((poly? (car tlist))
          (cons (list (caar tlist)(poly->list (cadar tlist)))
                                   (termlist->list (cdr tlist))))
         (else (cons (list (caar tlist) (cdadar tlist));取出指数和系数
                     (termlist->list (cdr tlist))))))
  (define (poly->list p)
    (cons (cadr p) (termlist->list (cddr p))))
  (displayln(poly->list p)))

(define (build-poly e1);把读入的东西形如(a (4 (b (3 1))))转化成多项式('polynomial a (4 ('integer.3)))
  (define (list->poly tlist)
         (if (null? tlist)
             '()
             (if(pair? (cadar tlist));如果系数项是多项式
                (cons (list (caar tlist)(build-poly (cadar tlist)))
                      (list->poly (cdr tlist)))
                (cons (list (caar tlist)(cons 'integer (cadar tlist)))
                      (list->poly (cdr tlist))))))
  (if (not(pair? e1))
      (cons'integer e1)
      (cons 'polynomial (cons (car e1)(list->poly (cdr e1))))))

(define (apply-generic op arg1 arg2);带标志的arg1 arg2
  (let((tags (list (car arg1) (car arg2))))
    (let ((proc (get op tags)))
      (if proc
          (apply proc (list (cdr arg1)(cdr arg2)));apply作用在不带标志的arg1,arg2上；没找到proc要么是因为第一个变量是integer，要么第二个是
          (if (eq? (car tags) 'integer)
              (apply-generic op (integer->poly (cadr arg2) arg1) arg2)
              (apply-generic op arg1 (integer->poly (cadr arg1) arg2)))))))
;---------已写好
(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(display-poly c1)

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
(myloop)
;(define f1 (read))
;(build-poly f1)
;(define f2(read))
;(add (build-poly f2)(build-poly f1))
;(build-poly f2)
;(list 'a (list 0 (cons 'polynomial (cdr (build-poly f2)))))
;(higher? 'a 'b)
;(display-poly(add (build-poly f1) (build-poly f2)));(list 'polynomial v1 (list 0 p2))
;(list 'polynomial 'a (list 0 (build-poly m)))
;(build-poly m)
;(higher? 'a 'b)
;(+ (a (4 3) (3 2) (2 1) (0 1)) (a (4 2) (1 3) (0 9)))
;(* (a (4 3) (3 2) (2 1) (0 1)) (a (4 2) (1 3) (0 9)))
;(+ (a (4 3) (3 2) (2 1) (0 1)) (b (4 2) (1 3) (0 9)))
;(+ (a (2 (b (3 1) (2 2))) (1 5)) (a (2 (c (2 2))) (1 4)))
;(+ (a (1 2) (0 (c (2 1) (1 2) (0 4)))) (b (1 2) (0 3)))
;(* (a (1 2) (0 (c (2 1) (1 2) (0 4)))) (b (1 2) (0 3)))
;(* (a (2 (b (3 1) (2 2))) (1 5)) (a (2 (c (2 2))) (1 4)))

    ;(a (4 5) (3 2) (2 1) (1 3) (0 10))
    ;(a (8 6) (7 4) (6 2) (5 9) (4 35) (3 21) (2 9) (1 3) (0 9))
    ;(a (4 3) (3 2) (2 1) (0 (b (4 2) (1 3) (0 10))))
   ; (a (2 (b (3 1) (2 2) (0 (c (2 2))))) (1 9))
  ;  (a (1 2) (0 (b (1 2) (0 (c (2 1) (1 2) (0 7))))))
 ;   (a (1 (b (1 4) (0 6))) (0 (b (1 (c (2 2) (1 4) (0 8))) (0 (c (2 3) (1 6) (0 12))))))
;    (a (4 (b (3 (c (2 2))) (2 (c (2 4))))) (3 (b (3 4) (2 8) (0 (c (2 10))))) (2 20))
