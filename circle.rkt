#lang racket/base
(display "请输入圆的半径：")
(define r (read))
(define res (*  3.141526 r r ))
(printf "计算得到面积是：~a" res )
(newline)
(read)