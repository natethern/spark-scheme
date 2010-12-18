(load "../easy-graphics.ss")

(define-display draw-rects
  (background 'white)
  (stroke 'black)
  (fill 'red)
  (let ((w 50) (h 50))
    (redraw-w w)
    (redraw-h h)
    (rect mouse-x mouse-y w h)))

(define-display free-hand
  (background 'white)
  (stroke 'black)
  (redraw-w 1)
  (redraw-h 1)
  (line p-mouse-x p-mouse-y mouse-x mouse-y))

(mouse-pressed
 (stroke 'black)
 (fill 'red)
 (rect mouse-x mouse-y 5 5))

(define key-hit #f)

(key-pressed
 (if key-hit
     (background 'black)
     (background 'white))
 (set! key-hit (not key-hit)))

(define-display free-rects
  (background 'white))

;(draw-rects)
;(free-hand)
(free-rects)
