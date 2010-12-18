(load "../easy-graphics.ss")

(define-display zoog
  (stroke 'black)
  (fill (rgb 150 0 0))
  (rect 100 100 20 100)
  (fill 'red)
  (ellipse 80 70 60 60)
  (fill 'black)
  (ellipse 90 85 16 32)
  (ellipse 116 85 16 32)
  (line 100 200 80 210)
  (line 120 200 130 215))

(zoog)