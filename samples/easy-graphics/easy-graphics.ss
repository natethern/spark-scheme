(import (airglow))

(define main-window null)
(define canvas null)

(define rgb airglow-rgb-color)
(define *fill-color* null)

(define (fill color)
  (set! *fill-color* color))

(define (no-fill)
  (set! *fill-color* null))

(define (set-fill-color shape x y w h)
  (unless (null? *fill-color*)
          (graphics-clip x y w h)
          (graphics-color! *fill-color*)
          (case shape
            ((rect)
             (graphics-draw-rectf x y w h))
            ((ellipse)
             (graphics-draw-pie x y w h 360 0)))
          (graphics-unclip)))

(define *stroke-color* 'black)

(define (stroke color)
  (set! *stroke-color* color))

(define (no-stroke)
  (set! *stroke-color* null))

(define (draw-stroke shape x y w h)
  (unless (null? *stroke-color*)
          (graphics-color! *stroke-color*))
  (case shape
    ((line)
     (graphics-draw-line x y w h))
    ((rect)
     (graphics-draw-rect x y w h))
    ((ellipse)
     (graphics-draw-arc x y w h 360 0))))

(define (line x y w h)
  (draw-stroke 'line x y w h))

(define (rect x y w h)
  (set-fill-color 'rect x y w h)
  (draw-stroke 'rect x y w h))

(define (ellipse x y w h)
  (set-fill-color 'ellipse x y w h)
  (draw-stroke 'ellipse x y w h))

(define (background color)
  (with (x (widget-x canvas)
           y (widget-y canvas)
           w (widget-width canvas)
           h (widget-height canvas))        
        (graphics-clip x y w h)
        (graphics-color! color)
        (graphics-draw-rectf x y w h)
        (graphics-unclip)))

(define (show-gui title x y w h draw-cb)
  (set! main-window (window 'x x 'y y
                            'w w 'h h 
                            'title title
                            'type 'double))
  (set! canvas (widget 'x 0 'y 0 
                       'w w 'h h
                       'super 'widget
                       'draw (lambda (self p) (draw-cb))))
  
  (group-finish main-window)
  (window-show main-window)
  (airglow-run))

(define-syntax define-display
  (syntax-rules ()
    ((_ name body ...)
     (define name 
       (case-lambda 
        (()
         (name ""))
        ((title)
         (show-gui title 200 200 450 450 
                   (lambda () body ...)))
        ((title x y)
         (show-gui title x y 450 450
                   (lambda () body ...)))
        ((title x y w h)
         (show-gui title x y w h
                   (lambda () body ...))))))))
     
