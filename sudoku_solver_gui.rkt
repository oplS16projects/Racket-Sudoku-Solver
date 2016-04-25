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

(define style-delta (make-object style-delta% 
                                 'change-normal-color))

(new canvas% [parent panel1]
             [min-height h]
             [min-width w]
             [paint-callback
              (lambda (canvas dc)
                (draw-grid canvas dc h)
                (draw-sub-grid canvas dc h)
                (print-puzzle sudoku-ex t-table canvas dc h))])

(define sudoku-ex
  '((0 0 0 9 7 0 0 0 0) 
    (0 4 0 2 5 0 1 0 7) 
    (0 0 7 6 0 0 4 0 3) 
    (0 1 2 8 0 0 6 0 0) 
    (9 7 0 0 4 0 0 3 5) 
    (0 0 4 0 0 2 9 1 0) 
    (2 0 1 0 0 7 5 0 0) 
    (4 0 9 0 8 1 0 6 0) 
    (0 0 0 0 2 9 0 0 0)))

(define t-table
  '((#f #f #f #t #t #f #f #f #f) 
    (#f #t #f #t #t #f #t #f #t) 
    (#f #f #t #t #f #f #t #f #t) 
    (#f #t #t #t #f #f #t #f #f) 
    (#t #t #f #f #t #f #f #t #t) 
    (#f #f #t #f #f #t #t #t #f) 
    (#t #f #t #f #f #t #t #f #f) 
    (#t #f #t #f #t #t #f #t #f) 
    (#f #f #f #f #t #t #f #f #f)))
   
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

(define (print-puzzle sudoku-ex t-table canvas dc s)
  (send dc set-font (make-font #:size 25 #:family 'roman #:weight 'bold))
  (p-nested-lst sudoku-ex t-table dc s))

(define (p-lst lst t-table y dc s)
  (for ((val lst) (x 9)) (print-ind val t-table x y dc s)))

(define (p-nested-lst lst t-table dc s)
  (for ((item lst) (y 9)) (p-lst item t-table y dc s)))

(define (print-ind val t-table x y dc s)
  (if (equal? val 0) (set! val " ") #f)
  (if (not(is-given? t-table x y))
      (send dc draw-text (if (equal? val " ") val (number->string val)) (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15))
      (begin 
        (send dc set-text-foreground "green")
        (send dc draw-text (if (equal? val " ") val (number->string val)) (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15))
        (send dc set-text-foreground "black"))))

(define (get-row s row)
  (if (= row 1)
      (car s)
      (get-row (cdr s) (- row 1))))

(define (get-val s row column)
  (define (iter row column)
    (if (= column 1) (car row)
        (iter (cdr row) (- column 1))))
  (iter (get-row s row) column))

(define (is-given? table x y)
  (if (eq? (get-val table (+ 1 y) (+ 1 x)) #t) #t #f))


                    
(define panel2 (new horizontal-panel% [parent frame]
                                     [alignment '(center bottom)]
                                     [min-height 100]
                                     [min-width 600]))

(define solve-button (new button% [parent panel2] 
                          [label "Solve"]
                          [min-width 290]
                          [min-height 100]
                          [callback  (lambda (button event)
                                       event)]))

(define step-button (new button% [parent panel2] 
                          [label "Step"]
                          [min-width 290]
                          [min-height 100]
                          [callback  (lambda (button event)
                                       event)]))

(send frame show #t)