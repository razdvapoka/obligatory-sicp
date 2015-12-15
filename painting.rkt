; 2.2.4 Example: A Picture Language

; The picture language

#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

;(define (my-flipped-pairs painter)
;  (let ((painter2 (my-beside painter (my-flip-vert painter))))
;    (my-below-I painter2 painter2)))


;(define (my-right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (my-right-split painter (- n 1))))
;       (my-beside painter (my-below-I smaller smaller)))))


;(define (my-up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (my-up-split painter (- n 1))))
;        (my-below-I painter (my-beside smaller smaller)))))




;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
;    (combine4 (my-corner-split painter n))))


; Frames

(define (my-frame-coord-map frame)
  (lambda (vect)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect vect)
                           (edge1-frame frame))
               (scale-vect (ycor-vect vect)
                           (edge2-frame frame))))))
; Excercises

; 2.46

(define (my-make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* (xcor-vect v) s)
        (* (ycor-vect v) s)))

(define v1 (my-make-vect 100 100))
(define v2 (my-make-vect 200 200))

; 2.47

(define (my-make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define (vector-to-posn v)
  (make-posn (xcor-vect v) (ycor-vect v)))

(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (my-draw-line v1 v2)
  (line (vector-to-posn v1) (vector-to-posn v2)))

;(my-draw-line v1 v2)

; Painters

(define (my-segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (my-draw-line
         ((my-frame-coord-map frame) (start-segment segment))
         ((my-frame-coord-map frame) (end-segment segment))))
      segment-list)))

; Excercises

; 2.48

(define (my-make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; 2.49

(define outline-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0 0) (my-make-vect 0 1))
    (my-make-segment (my-make-vect 0 1) (my-make-vect 1 1))
    (my-make-segment (my-make-vect 1 1) (my-make-vect 1 0))
    (my-make-segment (my-make-vect 1 0) (my-make-vect 0 0)))))

(define X-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0 0) (my-make-vect 1 1))
    (my-make-segment (my-make-vect 0 1) (my-make-vect 1 0)))))

(define diamond-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0.5 0) (my-make-vect 1 0.5))
    (my-make-segment (my-make-vect 1 0.5) (my-make-vect 0.5 1))
    (my-make-segment (my-make-vect 0.5 1) (my-make-vect 0 0.5))
    (my-make-segment (my-make-vect 0 0.5) (my-make-vect 0.5 0)))))

(define A-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0.5 0) (my-make-vect 1 1))
    (my-make-segment (my-make-vect 0.5 0) (my-make-vect 0 1))
    (my-make-segment (my-make-vect 0.25 0.5) (my-make-vect 0.75 0.5)))))

(define slash-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0 0) (my-make-vect 0.35 0.35)))))

(define my-frame (my-make-frame (my-make-vect 0 0) (my-make-vect 300 0) (my-make-vect 0 300)))

;(outline-painter my-frame)
;(X-painter my-frame)
;(diamond-painter my-frame)
;(A-painter my-frame)
;(slash-painter my-frame)

(define (my-transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (my-frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (my-make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (my-flip-vert painter)
  (my-transform-painter painter
                     (my-make-vect 0 1)
                     (my-make-vect 1 1)
                     (my-make-vect 0 0)))

;((my-flip-vert A-painter) my-frame)

(define (shrink-to-upper-right painter)
  (my-transform-painter painter
                        (my-make-vect 0.5 0)
                        (my-make-vect 1 0)
                        (my-make-vect 0.5 0.5)))

;((shrink-to-upper-right outline-painter) my-frame)
;(outline-painter my-frame)
;((shrink-to-upper-right A-painter) my-frame)

(define (my-rotate90 painter)
  (my-transform-painter painter
                        (my-make-vect 0 1)
                        (my-make-vect 0 0)
                        (my-make-vect 1 1)))

((my-rotate90 A-painter) my-frame)
((my-rotate90 (my-rotate90 A-painter)) my-frame)
((my-rotate90 (my-rotate90 (my-rotate90 A-painter))) my-frame)
((my-rotate90 (my-rotate90 (my-rotate90 (my-rotate90 A-painter)))) my-frame)

(define (my-squash-inwards painter)
  (my-transform-painter painter
                        (my-make-vect 0 0)
                        (my-make-vect 0.65 0.35)
                        (my-make-vect 0.35 0.65)))

;(outline-painter my-frame)
;((my-squash-inwards outline-painter) my-frame)
;((my-squash-inwards diamond-painter) my-frame)

(define (my-beside painter1 painter2)
  (let ((split-point (my-make-vect 0.5 0)))
    (define left-painter
      (my-transform-painter painter1
                            (my-make-vect 0 0)
                            split-point
                            (my-make-vect 0 1)))
    (define right-painter
      (my-transform-painter painter2
                            split-point
                            (my-make-vect 1 0)
                            (my-make-vect 0.5 1)))
    (lambda (frame)
      (left-painter frame)
      (right-painter frame))))

;((my-beside A-painter diamond-painter) my-frame)

; Excercises

; 2.50

(define (my-flip-horiz painter)
  (my-transform-painter painter
                     (my-make-vect 1 0)
                     (my-make-vect 0 0)
                     (my-make-vect 1 1)))

;((my-flip-vert (my-beside A-painter diamond-painter)) my-frame)

;((my-beside A-painter diamond-painter) my-frame)

;((my-flip-vert (my-beside A-painter diamond-painter)) my-frame)

(define (my-rotate180 painter)
  (my-transform-painter painter
                        (my-make-vect 1 1)
                        (my-make-vect 0 1)
                        (my-make-vect 1 0)))

;(slash-painter my-frame)
;((my-rotate180 slash-painter) my-frame)

(define (my-rotate270 painter)
  (my-transform-painter painter
                        (my-make-vect 1 0)
                        (my-make-vect 1 1)
                        (my-make-vect 0 0)))

;(slash-painter my-frame)
;((my-rotate270 slash-painter) my-frame)

; 2.51

(define (my-below-I painter1 painter2)
  (let ((split-point (my-make-vect 0 0.5)))
    (define up-painter
      (my-transform-painter painter2
                            (my-make-vect 0 0)                            
                            (my-make-vect 1 0)
                            split-point))
    (define bottom-painter
      (my-transform-painter painter1
                            split-point
                            (my-make-vect 1 0.5)
                            (my-make-vect 0 1)))
    (lambda (frame)
      (up-painter frame)
      (bottom-painter frame))))

;((my-below-I A-painter diamond-painter) my-frame)

(define (my-below-II painter1 painter2)
  (lambda (frame)
    ((my-rotate270
      (my-beside (my-rotate90 painter2)
                 (my-rotate90 painter1)))
     frame)))

;((my-below-II A-painter diamond-painter) my-frame)

;((my-flipped-pairs A-painter) my-frame)

;((my-right-split A-painter 15) my-frame)

(define (my-split left-op right-op)
  (define (inner-split painter n)  
    (if (= n 0)
      painter
      (let ((smaller (inner-split painter (- n 1))))
        (left-op painter (right-op smaller smaller)))))
  inner-split)

(define my-right-split (my-split my-beside my-below-I))

(define my-up-split (my-split my-below-I my-beside))

(define (my-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (my-up-split painter (- n 1)))
            (right (my-right-split painter (- n 1))))
        (let ((top-left (my-beside up up))
              (bottom-right (my-below-I right right))
              (corner (my-corner-split painter (- n 1))))
          (my-beside (my-below-I painter top-left)
                     (my-below-I bottom-right corner))))))

(define (my-square-limit painter n)
  (let ((quarter (my-corner-split painter n)))
    (let ((half (my-beside (my-flip-horiz quarter) quarter)))
      (my-below-I (my-flip-vert half) half))))

; Higher order operations

(define (my-square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (my-beside (tl painter) (tr painter)))
          (bottom (my-beside (bl painter) (br painter))))
      (my-below-I bottom top))))

(define (my-flipped-pairs painter)
  (let ((combine4 (my-square-of-four identity my-flip-vert identity my-flip-vert)))
    (combine4 painter)))


;((my-up-split A-painter 5) my-frame)

;((my-right-split A-painter 5) my-frame)

;((my-corner-split A-painter 5) my-frame)

;((my-square-limit A-painter 5) my-frame)

;((my-flipped-pairs A-painter) my-frame)

;(((my-square-of-four identity my-rotate90 identity my-rotate180) A-painter) my-frame)

(define A-upd-painter
  (my-segments->painter
   (list
    (my-make-segment (my-make-vect 0.5 0) (my-make-vect 1 1))
    (my-make-segment (my-make-vect 0.5 0) (my-make-vect 0 1))
    (my-make-segment (my-make-vect 0.25 0.5) (my-make-vect 0.75 0.5))
    (my-make-segment (my-make-vect 0.5 0.25) (my-make-vect 0.5 0.75)))))

;(A-upd-painter my-frame)

(define (my-upd-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (my-up-split painter (- n 1)))
            (right (my-right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (my-corner-split painter (- n 1))))
          (my-beside (my-below-I painter top-left)
                     (my-below-I bottom-right corner))))))

;((my-corner-split A-upd-painter 3) my-frame)

;((my-upd-corner-split A-upd-painter 3) my-frame)


(define (my-upd-square-limit painter n)
  (let ((quarter (my-corner-split painter n)))
    (let ((half (my-beside (my-flip-vert quarter) quarter)))
      (my-below-I (my-flip-horiz half) half))))

;((my-square-limit A-upd-painter 3) my-frame)
;((my-upd-square-limit A-upd-painter 3) my-frame)
