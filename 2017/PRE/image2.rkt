#lang racket
(require 2htdp/image)
(define heihei(bitmap/url "http://img.zcool.cn/community/01d92855f3c88632f875a1323c55cd.JPG@900w_1l_2o_100sh.jpg"))
(scale 40
       (color-list->bitmap
        (list "red""green""yellow""blue")
            4 1))
(mode? 255)
(overlay/pinhole
 (put-pinhole 25 10 (ellipse 100 50 "solid" "red"))
 (put-pinhole 75 40 (ellipse 100 50 "solid" "blue")))
(clear-pinhole
(overlay/pinhole
 (put-pinhole 25 10 (ellipse 100 50 "solid" "red"))
 (put-pinhole 75 40 (ellipse 100 50 "solid" "blue"))))
(let ([petal (put-pinhole 100 20 (ellipse 200 40 "solid" "purple"))])
  (clear-pinhole
   (overlay/pinhole
    (put-pinhole 30 30(circle 30 "solid" "yellow"))
    petal
    (rotate 60 petal)
    (rotate 120 petal))))
(save-svg-image heihei "cute.bmp")