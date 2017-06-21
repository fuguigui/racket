#lang racket/gui
;1400017694伏贵荣
;--------定义工作界面
(define frame (new frame%
                   [label "Little-person"]
                   [width 1000]
                   [height 560]))
;---------定义画布
(define mycanvas(new canvas%[parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-pen "red" 1 'solid)
        (for-each
         (lambda(seg)
           (let((v1 (start-segment seg))
                (v2 (end-segment seg)))
             (send dc draw-line (xcor-vect v1) (ycor-vect v1)(xcor-vect v2)(ycor-vect v2))))
         initline))]))
;----------定义画图操作,把所有需要画的线段都存在列表里
(define initline '())
(define (pre-draw-line v1 v2)
  (set! initline (cons (make-segment v1 v2) initline)))

;----------定义基本操作
(define (accumulate op init args)
  (if (null? args)
      init
      (op (car args)
          (accumulate op init (cdr args)))))
;----------定义框架操作
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)(car f))
(define (edge1-frame f)(cadr f))
(define (edge2-frame f)(caddr f))


;----------定义向量操作
(define (make-vect x y)(cons x y))
(define (xcor-vect x)(car x))
(define (ycor-vect y)(cdr y))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (to-unit-list vectlist s);对原始的矩阵列表做归一化处理，s为系数
  (map (lambda(v)(scale-vect s v)) vectlist))
(define (to-unit-llist vll s);对矩阵的列表的列表里的矩阵进行归一化处理
  (map (lambda (vlist)(to-unit-list vlist s)) vll))

;----------定义线段
(define (make-segment start end) (cons start end))
(define (start-segment seg)(car seg))
(define (end-segment seg) (cdr seg))
(define (segments->painter segment-list);把线段列表里的线画出来
  (lambda (frame)
    (for-each
     (lambda (segment)
       (pre-draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame)(end-segment segment))))
     segment-list)))

;---------向量到线段之间的转化
(define (vect->segment v1 v2)(make-segment v1 v2))
(define (vlist->seglist vectors);一组向量到一组线段
  (define segments '())
  (define (vs->ss vectors segs)
    (if (or
         (null? vectors)
         (null? (cdr vectors)))
        segs
        (let((v1 (car vectors))
             (v2 (cadr vectors)))
          (vs->ss (cdr vectors)
                  (cons (make-segment v1 v2) segs)))))
  (vs->ss vectors segments))
(define (vllist->seglist vll);向量列表的列表到线段列表的转化
  (define ss '())
  (define (vll->sl ll s)
    (if (null? ll)
        s
        (vll->sl (cdr ll)
                 (append
                  (vlist->seglist (car ll))
                  s))))
  (vll->sl vll ss))
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
(define (transform-painter painter origin corner1 corner2)
   (lambda (frame)
     (let((m (frame-coord-map frame)))
       (let ((new-origin (m origin)))
         (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))
;----------定义具体的painter的操作
;-------以下以painter为参数，返回painter，在给定的frame上进行操作
(define (beside wav1 wav2)
  (let((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter wav1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter wav2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
          (lambda (frame)
            (paint-left frame)
            (paint-right frame)))))
(define (flip-vert wav)
  (transform-painter wav
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (below wav1 wav2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let((up-painter
          (transform-painter wav1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
         (down-painter
          (transform-painter wav2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))))
      (lambda (frame)
        (up-painter frame)
        (down-painter frame)))))
(define (flip-horiz wav);从左向右翻转
  (transform-painter wav
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (four-copy wav);four-copy以wav作为左上角的原型
  (let((up (beside wav (flip-horiz wav))))
    (below up (flip-vert up))))
(define (square-limit wav n)
  (let ((left-up (corner-split-right-down wav n)))
    (four-copy left-up)))
(define (up-split wav n)
  (if (= n 0)
      wav
      (let((upper (up-split wav (- n 1))))
        (below (beside upper upper)
               wav))))
(define (left-split wav n)
  (if (= n 0)
      wav
      (let ((lleft (left-split wav (- n 1))))
        (beside (below lleft lleft) wav))))

(define (corner-split-right-down painter n);保持右下角为最大的图
  (if (= n 0)
      painter
      (let((up (up-split painter (- n 1)))
           (left (left-split painter (- n 1))))
        (let ((top-right (beside up up))
              (bottom-left (below left left))
              (corner (corner-split-right-down painter (- n 1))))
          (below (beside corner top-right)
                 (beside bottom-left painter))))))




;---------定义原始小人的线段列表
(define origin-vectlist
   (let ((v1 (list (make-vect 0 26)
                  (make-vect 6 17)
                  (make-vect 12 25)
                  (make-vect 14 21)
                  (make-vect 10 0)))
        (v2 (list (make-vect 16 0)
                  (make-vect 21 13)
                  (make-vect 25 0)))
        (v3 (list (make-vect 31 0)
                  (make-vect 25 19)
                  (make-vect 41 6)))
        (v4 (list (make-vect 41 15)
                  (make-vect 31 27)
                  (make-vect 25 27)
                  (make-vect 27 35)
                  (make-vect 25 41)))
        (v5 (list (make-vect 16 41)
                  (make-vect 14 35)
                  (make-vect 16 27)
                  (make-vect 12 27)
                  (make-vect 6 25)
                  (make-vect 0 35))))
     (list v1 v2 v3 v4 v5)))
(define newvllist (to-unit-llist origin-vectlist 0.024))
(define origin-segs (vllist->seglist newvllist));得到原始小人的线段列表


(define init (segments->painter origin-segs))
(define person(flip-horiz(flip-vert init)))
(define two-people (beside(flip-vert init) init))
(define omiting (square-limit person 4))
;--------------定义画布
(define frame1 (make-frame
                (make-vect 10 10)
                (make-vect 370 0)
                (make-vect 0 370)))
(define frame2 (make-frame
                (make-vect 390 10)
                (make-vect 360 100)
                (make-vect 150 360)))

(omiting frame1)
(two-people frame2)
(send frame show #t)
