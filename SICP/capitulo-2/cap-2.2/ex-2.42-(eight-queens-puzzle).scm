#lang sicp
; ------------------------------- Exercise 2.42 --------------------------------
; The ``eight-queens puzzle'' asks how to place eight queens on a chessboard so
; that no queen is in check from any other (i.e., no two queens are in the same
; row, column, or diagonal). One possible solution is shown in figure 2.8. One
; way to solve the puzzle is to work across the board, placing a queen in each
; column. Once we have placed k - 1 queens, we must place the kth queen in a
; position where it does not check any of the queens already on the board. We
; can formulate this approach recursively: Assume that we have already generated
; the sequence of all possible ways to place k - 1 queens in the first k - 1
; columns of the board. For each of these ways, generate an extended set of
; positions by placing a queen in each row of the kth column. Now filter these,
; keeping only the positions for which the queen in the kth column is safe with
; respect to the other queens. This produces the sequence of all ways to place k
; queens in the first k columns. By continuing this process, we will produce not
; only one solution, but all solutions to the puzzle.

; We implement this solution as a procedure queens, which returns a sequence of
; all solutions to the problem of placing n queens on an n× n chessboard. Queens
; has an internal procedure queen-cols that returns the sequence of all ways to
; place queens in the first k columns of the board.

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; In this procedure rest-of-queens is a way to place k - 1 queens in the first
; k - 1 columns, and new-row is a proposed row in which to place the queen for
; the kth column. Complete the program by implementing the representation for
; sets of board positions, including the procedure adjoin-position, which
; adjoins a new row-column position to a set of positions, and empty-board,
; which represents an empty set of positions. You must also write the procedure
; safe?, which determines for a set of positions, whether the queen in the kth
; column is safe with respect to the others. (Note that we need only check
; whether the new queen is safe -- the other queens are already guaranteed safe
; with respect to each other.)

(define empty-board nil)

; Verifica si la reina en la columna k está en una posición segura respecto a
; las reinas ya colocadas. Comprueba que no haya conflicto en la misma fila o
; en las diagonales.
(define (safe? k positions)
  (let ((row car)
        (col cadr)
        (new-queen (car positions)) 
        (rest-of-queens (cdr positions)))
    
    (define (check-row? rest-queens)
      (cond ((null? rest-queens) #f)
            ((= (row new-queen) (row (car rest-queens))) #t)
            (else (check-row? (cdr rest-queens)))))

    ; downward-proj y upward-proj representan las proyecciones diagonales hacia
    ; abajo y hacia arriba, respectivamente, de la reina de rest-queens.
    (define (check-diagonal? rest-queens)
      (if (null? rest-queens)
          #f
          (let  ((downward-proj
                  (+ (row (car rest-queens))
                     (- (col new-queen) (col (car rest-queens)))))
                 (upward-proj
                  (- (row (car rest-queens))
                     (- (col new-queen) (col (car rest-queens))))))
            
            (if (or (= downward-proj (row new-queen))
                    (= upward-proj (row new-queen)))
                #t
                (check-diagonal? (cdr rest-queens))))))
    
    (and (not (check-row? rest-of-queens))
         (not (check-diagonal? rest-of-queens)))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

; Codigo viejo ----------------------------------------------------------------
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))