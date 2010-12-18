(load "../easy-graphics.ss")

(define-display colors
  (background 'white)
  (no-stroke)
  (fill (rgb 255 0 0))
  (ellipse 20 20 16 16)
  (fill (rgb 127 0 0))
  (ellipse 40 20 16 16)
  (fill (rgb 255 200 200))
  (ellipse 60 20 16 16))

(colors "colors" 100 100 100 50)
