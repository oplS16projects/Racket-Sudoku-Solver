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
                (draw-grid canvas dc h))])

(define (draw-grid canvas dc s)
  (send dc set-pen "black" 4 'solid)
  (send dc draw-line (* 1/3 s) 0 (* 1/3 s) s)
  (send dc draw-line (* 2/3 s) 0 (* 2/3 s) s)
  (send dc draw-line 0 (* 1/3 s) s (* 1/3 s))
  (send dc draw-line 0 (* 2/3 s) s (* 2/3 s))
  (send dc set-pen "red" 1 'solid)
  (send dc draw-line 

(define panel2 (new horizontal-panel% [parent frame]
                                     [alignment '(center bottom)]
                                     [min-height 100]
                                     [min-width 600]))

(define solve-button (new button% [parent panel2] 
                          [label "Solve"]
                          [min-width 190]
                          [min-height 100]))

(define step-button (new button% [parent panel2] 
                          [label "Step"]
                          [min-width 190]
                          [min-height 100]))

(define interact-button (new button% [parent panel2] 
                          [label "Interact"]
                          [min-width 190]
                          [min-height 100]))

(send frame show #t)