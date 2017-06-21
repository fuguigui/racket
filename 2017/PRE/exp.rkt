#lang racket
(require 2htdp/image)
(require 2htdp/planetcute)

;定义向上叠加操作
(define (stack imgs)
  (cond [(empty? (rest imgs))(first imgs)]
        [else (overlay/xy (first imgs)
                          0 40
                          (stack (rest imgs)))]))
;定义在同一平面上纵向紧密排列操作
(define (vertical-align imgs)
  (cond [(empty? (rest imgs))(first imgs)]
        [else (underlay/xy (first imgs)
                           0 80
                           (vertical-align (rest imgs)))]))
;同一平面上的横向排列
(define (horizon-align imgs)
  (cond [(empty? (rest imgs))(first imgs)]
        [else (overlay/xy (first imgs)
                           100 0
                           (horizon-align (rest imgs)))]))
;放置在指定位置的操作，这里是相对坐标。
(define (put/xy x y img1 img2)
  (overlay/xy img1
              (- 0 (* 100 x)) (+ 40(* 80 (- 0 y)))
              img2))
(define (stack/xy x y new old)
  (overlay/xy new
              (- 0 (* 100 x))(* 80 (- 1 y))
              old))



;定义各种基本图形元素
(define river-1 (vertical-align (list water-block water-block water-block water-block water-block)))
(define river (horizon-align (list river-1 river-1)))
(define base-1 (vertical-align (list dirt-block dirt-block dirt-block dirt-block dirt-block)))
(define base (horizon-align (list base-1 base-1 base-1 base-1 base-1 base-1 base-1)))
(define stone-bridge (horizon-align (list ramp-west stone-block stone-block stone-block ramp-east)))
(define bridge-with-bug (put/xy 2 0 enemy-bug stone-bridge))
(define grass-1 (vertical-align (list grass-block grass-block grass-block grass-block grass-block)))
(define grass (horizon-align (list grass-1 grass-1 grass-1)))
(define castle (beside/align "bottom"
                               (stack (list wall-block wall-block wall-block))
                              door-tall-closed
                              (stack (list wall-block wall-block wall-block))))
(define stone-2 (vertical-align (list stone-block stone-block)))
(define stone-3 (vertical-align (list stone-block stone-block stone-block)))
(define stone-3-3 (horizon-align (list stone-3 stone-3 stone-3)))
(define stone-big (horizon-align (list stone-3 stone-3 stone-3 stone-3)))
(define stone-little (horizon-align (list stone-2 stone-2)))
(define dirty-1 (vertical-align (list dirt-block dirt-block)))
(define dirty (horizon-align (list dirty-1 dirty-1)))
(define key-stone (put/xy 0 1 key (put/xy 1 1 rock dirty)))


(define base2-1(stack/xy 3 0.5 stone-big base))
(define base2-2 (stack/xy 0 1 grass base2-1))
(define base2-3 (stack/xy 3 4 stone-little base2-2))
(define base2 (stack/xy 5 4 key-stone base2-3))
(define tree-on-grass (put/xy 0 0 tree-tall 
                                    (vertical-align
                                     (list
                                      grass-block
                                      grass-block))))

(define base3-1 (stack/xy 0 0.5 stone-3 base2))
(define base3-2 (stack/xy 0 3.5 tree-on-grass base3-1))
(define base3-3 (stack/xy 1 1 river base3-2))
(define base3-4 (stack/xy 3 1 stone-3-3 base3-3))

(define horngirl-selector (put/xy 0 0 character-horn-girl 
                                    (put/xy
                                     0 1
                                     selector
                                     (vertical-align
                                     (list
                                      grass-block
                                      grass-block)))))
(define slap-stone (vertical-align (list stone-block stone-block ramp-south)))
(define base3-5 (stack/xy 3 3.5 horngirl-selector base3-4))
(define base3 (stack/xy 6 1 slap-stone base3-5))
(define base4-1 (stack/xy 4 -0.5 castle base3))
(define base4-2 (stack/xy 0 2.5 bridge-with-bug base4-1))
(define finale (stack/xy 5 3 character-princess-girl base4-2))

;显示最终成果
finale
;存储图片
(save-image base2 "/planetcute/base2.bmp")
(save-image base "/planetcute/base1.bmp")
(save-image base3 "/planetcute/base3.bmp")
(save-image castle "/planetcute/castle.bmp")
(save-image finale "/planetcute/finished2.bmp")