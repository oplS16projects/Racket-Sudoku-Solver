#lang racket

(require slideshow/text)

(define (get-row s row)
  (if (= row 1)
      (car s)
      (get-row (cdr s) (- row 1))))

(define (get-column s column)
  (define (iter res row)
    (if (> row 9) res
        (iter (append res (list (get-val s row column))) (+ row 1))))
  (iter '() 1))

(define (get-nonet s row column)
  (define nonet (+ (* 3 (floor (/ (- row 1) 3))) (floor (/ (- column 1) 3)) 1))
  (define start-row (+ (* 3 (floor (/ (- row 1) 3))) 1))
  (define start-column (+ (* 3 (floor (/ (- column 1) 3))) 1))
  (define (iter3 vals column)
    (if (= column 1)
        (list (car vals) (cadr vals) (caddr vals))
        (iter3 (cdr vals) (- column 1))))
  (define (iter2 s row column res)
    (if (= row 0) res
        (iter2 (cdr s) (- row 1) column (append res (iter3 (car s) column)))))
  (define (iter s row column)
    (if (= row 1)
        (iter2 s 3 column '())
        (iter (cdr s) (- row 1) column)))
  (iter s start-row start-column))

(define (get-val s row column)
  (define (iter row column)
    (if (= column 1) (car row)
        (iter (cdr row) (- column 1))))
  (iter (get-row s row) column))

(define (display-sudoku s)
  (define (iter s row column)
    (display (if (and (= (modulo row 3) 1) (= column 1)) "+---+---+---+\n" ""))
    (display (if (= (modulo column 3) 1) "|" ""))
    (display (if (= 0 (get-val s row column)) "?" (get-val s row column)))
    (display (if (= column 9) "|\n" ""))
    (cond ((< column 9) (iter s row (+ column 1)))
          ((< row 9) (iter s (+ row 1) 1))
          (else (display "+---+---+---+\n"))))
  (iter s 1 1))

(define (set s row column value)
  (define (iter2 vals col res)
    (if (null? vals) (list res)
        (if (= col 1)
            (iter2 (cdr vals) (- col 1) (append res (list value)))
            (iter2 (cdr vals) (- col 1) (append res (list (car vals)))))))
  (define (iter s row column result)
    (if (null? s) result
        (if (= row 1)
            (iter (cdr s) (- row 1) column (append result (iter2 (car s) column '())))
            (iter (cdr s) (- row 1) column (append result (list (car s)))))))
  (iter s row column '()))

(define (value-okay? s row column)
  (and (not (= 0 (get-val s row column)))
       (row-okay? (get-row s row) column (get-val s row column))
       (column-okay? (get-column s column) row (get-val s row column))
       (nonet-okay? (get-nonet s row column) row column (get-val s row column))))

(define (row-okay? vals column value)
  (define (iter vals col res)
    (if (null? vals) res
        (if (= col 1)
            (iter (cdr vals) (- col 1) res)
            (iter (cdr vals) (- col 1) (append res (list (car vals)))))))
  (foldl (lambda (n m)
           (if (= n 0) m
               (if (= n value) #f m)))
         #t (iter vals column '())))

(define (column-okay? vals row value) (row-okay? vals row value))

(define (nonet-okay? vals row column value)
  (row-okay? vals (+ (* 3 (remainder (- row 1) 3)) (remainder (- column 1) 3) 1) value))

(define (step-solve-sudoku puzzle)
  (define (iter s row column back)
    (display row)
    (display " ")
    (display column)
    (display "\n")
    (cond ((= row 10) (display-sudoku s)) ; Puzzle is solved!
          ((get-val (cadr puzzle) row column) ; We ran into a fixed value that must be skipped over
           (if back ; We are back tracking and need to continue to back track
               (if (= column 1) (iter s (- row 1) 9 #t) (iter s row (- column 1) #t))
               (if (= column 9) (iter s (+ row 1) 1 #f) (iter s row (+ column 1) #f))))
          ((and (value-okay? s row column) (not back)) ; We have found a number that works so let's skip it
           (if (= column 9) (iter s (+ row 1) 1 #f) (iter s row (+ column 1) #f)))
          ((< (get-val s row column) 9) ; Try a different value in that space
           (iter (set s row column (+ (get-val s row column) 1)) row column #f))
          (else ; Backtrack
           (if (= column 1) (iter (set s row column 0) (- row 1) 9 #t) (iter (set s row column 0) row (- column 1) #t)))))
  (iter (car puzzle) 1 1 #f))
    

(define (create-sudoku-obj s) ;; This attaches the list of boolean values to the sudoku.  The bools are used to see if a value in a sudoku was given.
  (define (iter s-bools row column)
    (cond ((> column 9) (iter s-bools (+ 1 row) 1))
          ((> row 9) (list s s-bools))
          ((= 0 (get-val s row column)) (iter (set s-bools row column #f) row (+ column 1)))
          (else (iter (set s-bools row column #t) row (+ column 1)))))
  (iter s 1 1))

;;; Example sudokus listed below

(define sudoku-ex1
  (create-sudoku-obj
   '((0 0 0 9 7 0 0 0 0) 
    (0 4 0 2 5 0 1 0 7) 
    (0 0 7 6 0 0 4 0 3) 
    (0 1 2 8 0 0 6 0 0) 
    (9 7 0 0 4 0 0 3 5) 
    (0 0 4 0 0 2 9 1 0) 
    (2 0 1 0 0 7 5 0 0) 
    (4 0 9 0 8 1 0 6 0) 
    (0 0 0 0 2 9 0 0 0))))

(define sudoku-ex2
  (create-sudoku-obj
   '((0 0 0 2 6 0 7 0 1) 
    (6 8 0 0 7 0 0 9 0) 
    (1 9 0 0 0 4 5 0 0) 
    (8 2 0 1 0 0 0 4 0) 
    (0 0 4 6 0 2 9 0 0) 
    (0 5 0 0 0 3 0 2 8) 
    (0 0 9 3 0 0 0 7 4) 
    (0 4 0 0 5 0 0 3 6) 
    (7 0 3 0 1 8 0 0 0))))

(define sudoku-ex3
  (create-sudoku-obj
   '((0 0 0 6 0 0 4 0 0) 
    (7 0 0 0 0 3 6 0 0) 
    (0 0 0 0 9 1 0 8 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 5 0 1 8 0 0 0 3) 
    (0 0 0 3 0 6 0 4 5) 
    (0 4 0 2 0 0 0 6 0) 
    (9 0 3 0 0 0 0 0 0) 
    (0 2 0 0 0 0 1 0 0))))

;; --- End of examples ---


  
