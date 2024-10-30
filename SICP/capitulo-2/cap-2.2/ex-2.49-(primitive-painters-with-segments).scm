#lang sicp
; ------------------------------- Exercise 2.49 --------------------------------
; Use segments->painter to define the following primitive painters:

; a.  The painter that draws the outline of the designated frame.
(define outline-frame
  (lambda (frame)
    (segments->painter
     ; Creo los segmentos que forman edge1 y edge2
     (let ((segment1 (make-segment (origin-frame frame)
                                   (add-vect (origin-frame frame)
                                             (edge1-frame frame))))
           (segment2 (make-segment (origin-frame frame)
                                   (add-vect (origin-frame frame)
                                             (edge2-frame frame)))))
       ; Creo los otros 2 segmentos a partir de donde terminan los anteriores
       (let ((segment3 (make-segment (end-segment segment1)
                                     (add-vect (end-segment segment1)
                                               (edge2-frame frame))))
             (segment4 (make-segment (end-segment segment2)
                                     (add-vect (end-segment segment2)
                                               (edge1-frame frame)))))
         ; Ordeno los segmentos en sentido horario partiendo desde el origen
         (list segment1 segment3 segment4 segment2))))
    frame))


; b.The painter that draws an ``X'' by connecting opposite corners of the frame.

(define draw-x
  (lambda (frame)
    (segments->painter
     ; Primero creo el segmento que nace en el origen
     (list (make-segment (origin-frame frame)
                         ((frame-coord-map frame) (make-vect 1 1)))
           ; Ahora el que une edge1 y edge2
           (make-segment (add-vect (origin-frame frame) (edge1-frame frame))
                         (add-vect (origin-frame frame) (edge2-frame frame)))))
    frame))


; c.The painter that draws a diamond shape by connecting the midpoints of the 
; sides of the frame.
(define (midpoint segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-vect (/ (+ (xcor-vect end) (xcor-vect start)) 2)
               (/ (+ (ycor-vect end) (ycor-vect start)) 2))))

(define diamond
  (lambda (frame)
    (segments->painter
     ; Reutilizo parte del codigo del ejercicio "a" para crear los bordes
     ; Creo los segmentos que forman edge1 y edge2
     (let ((segment1 (make-segment (origin-frame frame)
                                   (add-vect (origin-frame frame)
                                             (edge1-frame frame))))
           (segment2 (make-segment (origin-frame frame)
                                   (add-vect (origin-frame frame)
                                             (edge2-frame frame)))))
       ; Creo los otros 2 segmentos a partir de donde terminan los anteriores
       (let ((segment3 (make-segment (end-segment segment1)
                                     (add-vect (end-segment segment1)
                                               (edge2-frame frame))))
             (segment4 (make-segment (end-segment segment2)
                                     (add-vect (end-segment segment2)
                                               (edge1-frame frame)))))
         ; Creo los segmentos del diamante en sentido horario
         ; desde el origen relativo del frame
         (list (make-segment (midpoint segment1) (midpoint segment3))
               (make-segment (midpoint segment3) (midpoint segment4))
               (make-segment (midpoint segment4) (midpoint segment2))
               (make-segment (midpoint segment2) (midpoint segment1))))))
    frame))
         

; d.The wave painter.
; Ver archivo auxiliar "ex-2.49-(wave-diagram)"
(define wave
  (lambda (frame)
    (segments->painter
     (let ((p1 (make-vect 0 14))
           (p2 (make-vect 3 10))
           (p3 (make-vect 5 11))
           (p4 (make-vect 7 11))
           (p5 (make-vect 6 14))
           (p6 (make-vect 7 16))
           (p7 (make-vect 9 16))
           (p8 (make-vect 10 14))
           (p9 (make-vect 9 11))
           (p10 (make-vect 12 11))
           (p11 (make-vect 16 6))
           (p12 (make-vect 0 11))
           (p13 (make-vect 3 7))
           (p14 (make-vect 5 10))
           (p15 (make-vect 6 8))
           (p16 (make-vect 4 0))
           (p17 (make-vect 6 0))
           (p18 (make-vect 8 5))
           (p19 (make-vect 10 0))
           (p20 (make-vect 12 0))
           (p21 (make-vect 9 8))
           (p22 (make-vect 16 4)))
       (let ((A (make-segment p1 p2))
             (B (make-segment p2 p3))
             (C (make-segment p3 p4))
             (D (make-segment p4 p5))
             (E (make-segment p5 p6))
             (F (make-segment p7 p8))
             (G (make-segment p8 p9))
             (GH (make-segment p9 p10))
             (H (make-segment p10 p11))
             (I (make-segment p12 p13))
             (J (make-segment p13 p14))
             (K (make-segment p14 p15))
             (L (make-segment p15 p16))
             (M (make-segment p17 p18))
             (N (make-segment p18 p19))
             (Ñ (make-segment p20 p21))
             (O (make-segment p21 p22)))
         (list A B C D E F G GH H I J K L M N Ñ O))))
    frame))
