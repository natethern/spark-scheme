(import (airglow))

(define main-window null)
(define canvas null)

(define rgb airglow-rgb-color)
(define *fill-color* null)
(define mouse-x 0)
(define mouse-y 0)
(define p-mouse-x 0)
(define p-mouse-y 0)
(define *full-redraw* #f)
(define *redraw-w* 0)
(define *redraw-h* 0)

;; callbacks
(define *draw* null)
;; event handlers
(define *mouse-pressed* null)
(define *mouse-released* null)
(define *key-pressed* null)
(define *key-released* null)

(define (full-redraw)
  (set! *full-redraw* #t))

(define (no-full-redraw)
  (set! *full-redraw* #f))

(define (redraw-w w)
  (set! *redraw-w* w))

(define (redraw-h h)
  (set! *redraw-h* h))

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

(define (invoke-event-handlers widget event)
  (when (and (eq? event 'mouse-push) 
             (procedure? *mouse-pressed*))
        (*mouse-pressed*))
  (when (and (eq? event 'mouse-release)
             (procedure? *mouse-released*))
        (*mouse-released*))
  (when (and (eq? event 'key-down) 
             (procedure? *key-pressed*))
        (*key-pressed*))
  (when (and (eq? event 'key-up)
             (procedure? *key-released*))
        (*key-released*)))

(define (event-handler widget event user-arg)
  (set! p-mouse-x mouse-x)
  (set! p-mouse-y mouse-y)
  (set! mouse-x (event-x))
  (set! mouse-y (event-y))
  (invoke-event-handlers widget event)
  (if (or (not (= p-mouse-x mouse-x))
          (not (= p-mouse-y mouse-y)))
      (if *full-redraw*
          (widget-redraw widget)
          (widget-damage! widget 16 mouse-x mouse-y
                          *redraw-w* *redraw-h*))))

(define (show-gui title x y w h draw-cb)
  (set! main-window (window 'x x 'y y
                            'w w 'h h 
                            'title title
                            'type 'double))
  (let ((draw-called #f))
    (set! canvas (widget 'x 0 'y 0 
                         'w w 'h h
                         'super 'widget
                         'draw (lambda (self p) 
                                 (draw-cb)
                                 (when (and (not (null? *draw*))
                                            (eq? draw-called #f))
                                       (set! draw-called #t)
                                       (thread (lambda ()
                                                 (let loop ()
                                                   (*draw*)
                                                   (widget-redraw self)
                                                   (loop))))))
                         'events event-handler)))
  
  (group-finish main-window)
  (window-show main-window)
  (airglow-thread-manager-start)
  (airglow-run)
  (airglow-thread-manager-stop))

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

(define-syntax draw
  (syntax-rules ()
    ((_ body ...)
     (set! *draw* (lambda () body ...)))))

(define-syntax mouse-pressed
  (syntax-rules ()
    ((_ body ...)
     (set! *mouse-pressed* (lambda () body ...)))))

(define-syntax mouse-released
  (syntax-rules ()
    ((_ body ...)
     (set! *mouse-released* (lambda () body ...)))))

(define-syntax key-pressed
  (syntax-rules ()
    ((_ body ...)
     (set! *key-pressed* (lambda () body ...)))))

(define-syntax key-released
  (syntax-rules ()
    ((_ body ...)
     (set! *key-released* (lambda () body ...)))))
     
