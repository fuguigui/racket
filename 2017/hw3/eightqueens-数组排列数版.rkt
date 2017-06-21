#lang racket
(define queenvct (make-vector 93 0))
(vector-set! queenvct 0 (vector 1 5 8 6 3 7 2 4))

(define (check vct)
  (define (checkloop i j)
    (cond((> i 7)#t)
          ((> j 7) (checkloop (+ i 1) (+ i 2)))
          ((or (eq?(- (vector-ref vct i) (vector-ref vct j)) (- i j))
               (eq?(- (vector-ref vct i) (vector-ref vct j)) (- j i))
               (eq? (- (vector-ref vct i) (vector-ref vct j)) 0)) #f)
          (else (checkloop i (+ j 1)))))
  (checkloop 0 1))
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
(define (try-next vct)
  (if (check vct)
      vct
     (try-next (nextseq 8 vct))))

(define (queenarray-n i)
  (if(not (eq? (vector-ref queenvct (- i 1)) 0));给出初始值
     (void)
     (begin(queenarray-n (- i 1))
           (vector-set! queenvct (- i 1)
                  (try-next (nextseq 8 (vector-ref queenvct (- i 2)))))
           (void))))
(define (mydisplay vct)
  (define l (vector-length vct))
  (define (loop i)
    (if (< i l)
        (begin(display (vector-ref vct i))
              (loop (+ i 1)))
        (void)))
  (loop 0)
  (newline))

(define (find)
  (define n (read))
  (begin(queenarray-n n))
  (mydisplay (vector-ref queenvct(- n 1))))
(define (myfor times op)
  (if (= times 0)
      (void)
      (begin(op)
       (myfor (- times 1) op))))

(define (myloop)
  (define t (read))
  (myfor t find))
(myloop)