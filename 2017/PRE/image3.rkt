#lang racket
(require 2htdp/image)
(require 2htdp/planetcute)
(define (stack imgs)
  (cond [(empty? (rest imgs))(first imgs)]
        [else (overlay/xy (first imgs)
                          0 40
                          (stack (rest imgs)))]))
(define game1(beside/align
 "bottom"
 (stack (list wall-block-tall stone-block))
 (stack (list character-cat-girl
              stone-block stone-block
              stone-block stone-block))
 water-block
 (stack (list grass-block dirt-block))
 (stack (list grass-block dirt-block dirt-block))))
(save-image game1 "/planetcute/game1.bmp")
(beside roof-north-west
        roof-north
        roof-north-east)
(beside roof-south-west
        roof-south
        roof-south-east)
(define (tile-a-plane length width imgs)
  (let((l length)
       (w width))
  (cond [(= 0 l)
         (first imgs)]
        [(= 0 w)
         (underlay (first imgs)
                   (tile-a-plane (- l 1) width (rest imgs)))]
        [else (beside (first imgs)
                        (first(rest imgs)))])))
(tile-a-plane 2 4
              (list door-tall-closed door-tall-closed))