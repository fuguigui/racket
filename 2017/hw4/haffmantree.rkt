#lang racket
(define (generate-huffman-tree lst)
  (define leaf-set (make-leaf-set lst))
  (define (generate1 lset)
    (cond((null? lset) lset)
         ((null? (cdr lset))
           (car lset))
         (else(make-code-tree (car lset)(generate1 (cdr lset))))))
  (generate1 leaf-set))

(define (in-symbols? bit node)
  (define slist (symbols node))
  (define (list-in-symbols? bit symlst)
  (if (null? symlst)
      #f
      (if(eq? (car slist) bit)
         #t
         (list-in-symbols? bit (cdr symlst)))))
  (list-in-symbols? bit slist))

(define (symbol-list symbols)
  (string->list (symbol->string symbols)))
(define (char-symbol chars)
  (string->symbol (string chars)))
(define (each-code bit tree)
    (if(leaf? tree)
       '()
       (let ((left (left-branch tree))
             (right (right-branch tree))
             (newbit (char-symbol bit)))
         (if(in-symbols? newbit left)
            (cons 0 (each-code bit left))
            (cons 1 (each-code bit right))))))

(define (encode bits0 htree)
  (define bits (symbol-list bits0))
  (define (encode-list bits htree)
    (if (null? bits)
      '()
      (append(each-code (car bits) htree)
             (encode-list (cdr bits) htree))))
  (encode-list bits htree))
  
  


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)


(define huffman-tree '())
(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))

(myloop)



;(define a (read))
;(define huffman(generate-huffman-tree a))
;(define b (read))
;(symbol? b)
;(decode (encode (symbol-list b) huffman)huffman)
;(eq? (string->symbol (string (car(string->list (symbol->string b)))))'A)

;((A 5) (B 4) (C 3) (E 2))