#lang racket
(require 2htdp/image)
;基本图元
;circle 第一个参数：半径，第二个参数：模式 第三个参数：颜色
;模式可选参数: solid outline
;颜色可选参数：red brown yellow blue green black white maroon（褐红色）
;ellipse 宽度，高度，模式，颜色
;pulled-regular-polygon
(pulled-regular-polygon 50 5 1/2 -10 "solid" "red")
(above (triangle 40 "solid" "red")
       (rectangle 40 30 "solid" "black"))
;beside
(beside (triangle 40 "solid" "red")
        (triangle 30 "solid" "red"))
;对齐的模式是什么？beside默认沿着中间对齐
;beside/align可以安排对齐的方式，可选参数有什么？
;可选参数："right" "center" "bottom" "left" "top"
(beside/align "bottom"
              (triangle 40 "solid" "red")
              (triangle 30 "solid" "red"))

(define victorian
  (above (beside/align "bottom"
                       (triangle 40 "solid" "red")
                       (triangle 30 "solid" "red"))
         (rectangle 70 40 "solid" "white")))
(define door (rectangle 15 25 "solid" "brown"))
;overlay/align 第一个参数是x方向，第二个参数是y方向？？
(overlay/align "center" "bottom" door victorian)
(define door-with-knob
  (overlay/align "right" "center" (circle 3 "solid" "yellow")
                 door))

(define house(overlay/align "center" "bottom" door-with-knob victorian))

;旋转覆盖
;要点在于使用了rotate函数，第一个参数：逆时针旋转的角度，第二个参数，旋转的图形
(define (a-number digit)
  (overlay
   (text (number->string digit) 12 "black")
   (circle 10 "solid" "white")))

(define (place-and-turn digit dial)
  (rotate 30
          (overlay/align "center" "top"
                         (a-number digit)
                         dial)))
(place-and-turn
 0
 (circle 60 "solid" "black"))
;颜色的第四个可选参数：alpha :透明度.255是不透明，0时完全透明，因为白色是255,255,255
;旋转迭代加透明度产生的图形
(define (spin-alot t)
    (local [(define (spin-more i θ)
              (cond
                [(= θ 360) i]
                [else
                 (spin-more (overlay i (rotate θ t))
                            (+ θ 1))]))]
      (spin-more t 0)))
(define spin-tri (spin-alot (triangle 90 "solid" (color 0 255 0 1))))
(save-image spin-tri "/planetcute/spin-tri.bmp")
(isosceles-triangle 120 30 "solid" (color 0 0 255 1))
(define spin-triangle (spin-alot (isosceles-triangle 120 30 "solid" (color 0 0 255 1))))
(define vari-circle(spin-alot (rectangle 120 30 "solid" (color 0 0 255 1))))
(save-image spin-triangle "/planetcute/spin-triangle.bmp")
(save-image vari-circle "/planetcute/circle.bmp")
;循环迭代的奇妙图形
(define (sierpinski-carpet n)
    (cond
      [(zero? n) (square 1 "solid" "black")]
      [else
       (local [(define c (sierpinski-carpet (- n 1)))
               (define i (square (image-width c) "solid" "yellow"))]
         (above (beside c c c)
                (beside c i c)
                (beside c c c)))]))
(define (koch-curve n)
    (cond
      [(zero? n) (square 1 "solid" "black")]
      [else
       (local [(define smaller (koch-curve (- n 1)))]
         (beside/align "bottom"
                       smaller
                       (rotate 60 smaller)
                       (rotate -60 smaller)
                       smaller))]))
(koch-curve 5)
(define (hero α)
    (triangle 30 "solid" (color 255 0 0 α)))
(save-image (koch-curve 5) "/planetcute/iterator.bmp")
(save-image house "/planetcute/house2.bmp")
;
(pulled-regular-polygon 100 3 1.8 30 "solid" "blue")