(load "../easy-graphics.ss")

(define width 200)
(define height 200)
(define x 0)
(define y 100)
(define xspeed 1)
(define yspeed 0)
(define gravity 1)

(draw
 (set! x (+ x xspeed))
 (if (or (> x (- width 32)) (< x 0))
     (set! xspeed (* xspeed -1)))
 (set! y (+ y yspeed))
 (if (< y 0) (set! y 0))
 (set! yspeed (+ yspeed gravity))
 (when (or (> y height) (< y 0))
     (set! yspeed (* yspeed -1)))
 (sleep .02))

(define-display bouncing-ball
  (background 'white)
  (stroke 'white)
  (fill (rgb 100 0 0))
  (ellipse x y 32 32))

  

(bouncing-ball "bouncing-ball" 100 100 width height)
  