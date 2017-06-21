#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s);返回一张表，表里的每个元素都是表
  (define (condcopy s)
    (lambda (seq)
    (if (= (+(car seq)(cadr seq)(caddr seq)) s)
        seq
        '())))
  (if (< (+ n n n -2 -1) s)
      '()
      (map (condcopy s)
           (flatmap
            (lambda (i)
             (flatmap
              (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval (+ j 1) n)))
              (enumerate-interval (+ i 1) (- n 1))))
           (enumerate-interval 1 (- n 2))))))


(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)