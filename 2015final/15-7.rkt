#lang racket
;检查列表是否有循环
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
      ;--------从这儿开始，利用闭包
      (define (check-cycle exp)
        (let((checked '()))
          (define (find-in? item all)
          (if(null? all)
             #f
             (if(equal? item (car all))
                #t
                (find-in? item (cdr all)))))
          (define (inner-check aim)
            (if(null? aim)
               #f
               (if(find-in? aim checked)
                  #t
                  (begin
                    (set! checked (cons aim checked))
                    (inner-check (cdr aim))))))
          (inner-check exp)))
    ;-----------结束
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)