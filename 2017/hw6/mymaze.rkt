#lang racket
(define list mlist)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
;对list的一些基本操作
;取出list的最后一个指针
(define (tail l)
  (if (null? (cdr l))
      l
      (tail (cdr l))))

(define (maze)
  ;读入三个参数
   (define r (read))
   (define c (read))
   (define k (read))
  (define the-maze '());存储整个迷宫
  ;读入每一行迷宫，c代表需要读入的个数，返回值为一个list
  (define (read-line c)
    (define line '())
    (if ( = c 0)
        void
        (begin(set-cdr! (tail c) (list (read)))
              (read-line (- c 1)))))
  ;读入整个迷宫，r代表行数
   (define (read-whole r)
      (if ( = r 0)
          void
          (begin(set! the-maze
                (cons (read-line c)the-maze))
          (read-whole (- r 1)))))
  (read-whole r))
;编写一个打印maze的函数，用于检查maze 的构造是否成功
(define (display-maze mm)
  (define (display-row (car mm)))
  (begin(display-row)))

(define (main)
  (define N (read))
  (define (myloop n)
    (if (= n 0)
        void
        (begin(maze)
              (myloop (- n 1)))))
  (myloop N))
(main)