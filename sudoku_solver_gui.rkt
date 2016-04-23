#lang racket/gui

(require racket/gui/base)

(define h 600)
(define w 600)

(define frame (new frame% [label "Sudoku Solver"] [width w] [height (+ 100 h)]))

(define panel1 (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]
                                     [min-height h]
                                     [min-width w]
                                     [stretchable-width false]	 
                                     [stretchable-height false]))

(new canvas% [parent panel1]
             [min-height h]
             [min-width w]
             [paint-callback
              (lambda (canvas dc)
                (draw-grid canvas dc h)
                (draw-sub-grid canvas dc h)
                (print-puzzle canvas dc h))])

(define sudoku-ex1
  '((0 0 0 9 7 0 0 0 0) 
    (0 4 0 2 5 0 1 0 7) 
    (0 0 7 6 0 0 4 0 3) 
    (0 1 2 8 0 0 6 0 0) 
    (9 7 0 0 4 0 0 3 5) 
    (0 0 4 0 0 2 9 1 0) 
    (2 0 1 0 0 7 5 0 0) 
    (4 0 9 0 8 1 0 6 0) 
    (0 0 0 0 2 9 0 0 0)))
   
(define (draw-grid canvas dc s)
  (send dc set-pen "black" 4 'solid)
  (send dc draw-line (* 1/3 s) 0 (* 1/3 s) s)
  (send dc draw-line (* 2/3 s) 0 (* 2/3 s) s)
  (send dc draw-line 0 (* 1/3 s) s (* 1/3 s))
  (send dc draw-line 0 (* 2/3 s) s (* 2/3 s)))

(define (draw-sub-grid canvas dc s)
  (send dc set-pen "black" 1 'solid)
  (send dc draw-line (* 1/9 s) 0 (* 1/9 s) s)
  (send dc draw-line (* 2/9 s) 0 (* 2/9 s) s)
  (send dc draw-line (* 4/9 s) 0 (* 4/9 s) s)
  (send dc draw-line (* 5/9 s) 0 (* 5/9 s) s)
  (send dc draw-line (* 7/9 s) 0 (* 7/9 s) s)
  (send dc draw-line (* 8/9 s) 0 (* 8/9 s) s)
  
  (send dc draw-line 0 (* 1/9 s) s (* 1/9 s))
  (send dc draw-line 0 (* 2/9 s) s (* 2/9 s))
  (send dc draw-line 0 (* 4/9 s) s (* 4/9 s))
  (send dc draw-line 0 (* 5/9 s) s (* 5/9 s))
  (send dc draw-line 0 (* 7/9 s) s (* 7/9 s))
  (send dc draw-line 0 (* 8/9 s) s (* 8/9 s)))

(define (print-puzzle canvas dc s)
  (send dc set-pen "black" 4 'solid)
  (send dc set-font (make-font #:size 25 #:family 'roman #:weight 'bold))
  (p-nested-lst sudoku-ex1 dc s))

(define (p-lst lst y dc s)
  (for ((val lst) (x 9)) (print-ind val x y dc s)))

(define (p-nested-lst lst dc s)
  (for ((item lst) (y 9)) (p-lst item y dc s)))

(define (print-ind val x y dc s)
  (send dc draw-text (number->string val) (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15)))

(define panel2 (new horizontal-panel% [parent frame]
                                     [alignment '(center bottom)]
                                     [min-height 100]
                                     [min-width 600]))

(define solve-button (new button% [parent panel2] 
                          [label "Solve"]
                          [min-width 290]
                          [min-height 100]))

(define step-button (new button% [parent panel2] 
                          [label "Step"]
                          [min-width 290]
                          [min-height 100]))

(send frame show #t)