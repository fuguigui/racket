#lang racket
;集合：用有序列表实现：对集合进行四种操作：union-set,intersection-set,element-of-set? adjoin-set
;用二叉树实现排序的集合
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2 set1))
        (else (cond ((< (car set1) (car set2))
                     (cons (car set1)(union-set (cdr set1) set2)))
                    ((> (car set1)(car set2))
                     (cons (car set2)(union-set set1 (cdr set2))))
                    (else (cons (car set1)(union-set (cdr set1)(cdr set2)))))))))
(define (intersection-set set1 set2)
  (if (or (null? set1)(null? set2))
      '()
      (cond ((< (car set1) (car set2))
             (intersection-set (cdr set1) set2))
            ((> (car set1)(car set2))
             (intersection-set set1 (cdr set2)))
            (else (cons (car set1)(intersection-set (cdr set1)(cdr set2)))))))))
