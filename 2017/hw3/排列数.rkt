#lang racket
;关于如何寻找下一个排列数
;给出一个排列，怎么找出他的下一个排列？4731265;4731526;4731562;4731625
;      第一个逆序位b0，a1,  ,ai,ai+1  ，an
;ai>b0>ai+1下一个数: ai,an,...,ai+1,b0,ai-1,..,a1
(define (searchni n vct)
  (define (searchloop i vct)
    (if (< i 0)
        -1
        (if(< (vector-ref vct i) (vector-ref vct (+ i 1)))
           i
           (searchloop (- i 1) vct))))
  (searchloop (- n 2) vct));寻找到第一个逆序位
(define (findbigger n aim vct);寻找到第一个大的数的位置
  (define (findloop i vct)
    (if(> (vector-ref vct i) aim)
       i
       (findloop (- i 1) vct)))
  (findloop (- n 1) vct))

(define (copy aim s1 now s2 end)
  (if (> s2 end)
      aim
      (begin(vector-set! aim s1 (vector-ref now s2))
            (copy aim (+ s1 1) now (+ s2 1) end))))

(define (merge seq1 s1 e1 seq2 s2 e2)
  (define result (make-vector (+ (- e1 s1) (- e2 s2) 2) 0))
  (begin(copy result 0 seq1 s1 e1)
  (copy result (+(- e1 s1)1) seq2 s2 e2)))
(define (reverse ns os num new old)
    (if (< num 1)
        new
        (begin(vector-set! new ns (vector-ref old os))
              (reverse (+ ns 1) (- os 1) (- num 1) new old))))

(define (nextseq n seq)
  (define pre (searchni n seq))
  (define div (findbigger n (vector-ref seq pre) seq))
  (define newseq (make-vector (- n pre) 0))
  
  (begin(vector-set! newseq 0 (vector-ref seq div))
        (reverse 1 (- n 1) (- n 1 div) newseq seq)
        (vector-set! newseq (- n div) (vector-ref seq pre))
        (reverse (+ (- n div) 1) (- div 1) (- div pre 1) newseq seq )
        (merge seq 0 (- pre 1) newseq 0 (- n pre 1))))
(define tria (vector 4  7 3 1 2 6 5))
(nextseq 7 (nextseq 7 tria))