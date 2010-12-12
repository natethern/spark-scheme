(load "easy-graphics.ss")

(define-display basic-shapes
  (background 'white)
  (stroke 'black)
  (fill 'dark-yellow)
  (rect 50 40 75 100)
  (fill 'dark-red)
  (ellipse 150 140 100 100))

(define-display line-across-rect
  (background 150)
  (stroke 'red)
  (line 0 0 100 100)
  (stroke 'white)
  (no-fill)
  (rect 25 25 50 50))

(define-display squares
  (let ((a 100))
    (background 'white)
    (stroke 'black)
    (fill 'dark-red)
    (rect 0 0 a a)
    (fill (rgb 205 183 158))
    (rect a a a a)))

;(basic-shapes)
;(line-across-rect "Line/Rect" 100 100 100 100)
(squares "Squares" 100 100 200 200)
