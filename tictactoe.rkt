;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; A Tic-Tac-Toe Grid (T3Grid) is a (listof (listof (anyof 'X 'O '_)))
;; Requires: all lists have the same length, and that length is odd
;; The number of 'X and 'O is equal, or there is one more 'X

;; Sample grids:
(define grid1 (list (list '_ '_ '_)
                    (list '_ '_ '_)
                    (list '_ '_ '_)))

(define grid2 (list (list 'X 'O 'O 'O '_)
                    (list 'X 'X 'O '_ '_)
                    (list '_ '_ '_ '_ '_)
                    (list '_ '_ '_ '_ '_)
                    (list '_ '_ '_ '_ 'X)))

(define grid3 (list (list 'O 'O 'O 'O '_)
                    (list 'X 'X 'O '_ '_)
                    (list '_ '_ 'X '_ 'O)
                    (list '_ '_ '_ '_ '_)
                    (list '_ 'X 'X 'X 'X)))

;;
;; a)
;;

;; (whose-turn T3Grid) consumes a T3Grid and determines whose turn it is.
;; Since X goes first, if the number of Xs and Os are equal, it produces 'X
;; If the number of Xs is greater than the number of Os, it produces 'O

;; Examples:
(check-expect (whose-turn (list (list 'X))) 'O)
(check-expect (whose-turn (list (list 'X 'O 'O 'O '_)
                                (list 'X 'X 'O '_ '_)
                                (list '_ '_ 'O '_ '_)
                                (list '_ '_ 'X '_ '_)
                                (list '_ '_ '_ '_ 'X))) 'X)
(check-expect (whose-turn (list (list '_ '_ '_)
                                (list '_ '_ '_)
                                (list '_ '_ '_))) 'X)

;; whose-turn: T3Grid -> Sym
;; Requires: the number of Xs and Os to be equal or Xs one greater than Os (valid game)
(define (whose-turn T3Grid)
  (cond
    [(= (symbol-counter T3Grid 'X) (symbol-counter T3Grid 'O)) 'X]
    [(> (symbol-counter T3Grid 'X) (symbol-counter T3Grid 'O)) 'O]))

;; (row-counter row player) consumes a row and a player symbol
;; and produces a count of how many symbols 'X or 'O there are
;; example:
(check-expect (row-counter (list 'X 'O 'O 'O '_) 'O) 3)
;; row-counter: (listof Sym) Sym -> Int

(define (row-counter row player)
  (cond
    [(empty? row) 0]
    [(symbol=? player (first row)) (+ 1 (row-counter (rest row) player))]
    [(not (symbol=? player (first row))) (+ 0 (row-counter (rest row) player))]))

;; (symbol-counter T3Grid player) consumes a T3Grid and a player symbol
;; and produces a count of how many symbols 'X or 'O there are
;; example:
(check-expect (symbol-counter grid2 'O) 4)
;; symbol-counter: T3Grid Sym -> Int

(define (symbol-counter T3Grid player)
  (cond
    [(empty? T3Grid) 0]
    [else (+ (row-counter (first T3Grid) player) (symbol-counter (rest T3Grid) player))]))

;; Tests:
(check-expect (whose-turn grid1) 'X)
(check-expect (whose-turn grid2) 'X)
(check-expect (whose-turn grid3) 'O)


;;
;; b)
;;

;; (grid-ref T3Grid row-num column-num) consumes T3Grid, a row number, and column number and
;; produces the symbol located at the location (position numbers start counting from 0)

;; Examples:
(check-expect (grid-ref grid2 1 2) 'O)
(check-expect (grid-ref grid2 0 0) 'X)
(check-expect (grid-ref grid1 2 2) '_)

;; grid-ref: T3Grid Int Int -> Sym
(define (grid-ref T3Grid row-num column-num)
  (list-ref (list-ref T3Grid row-num) column-num))`

;; Tests:
(check-expect (grid-ref grid3 0 0) 'O)
(check-expect (grid-ref grid3 4 4) 'X)
(check-expect (grid-ref grid3 1 3) '_)

;;
;; c)
;;

;; (get-column T3Grid column-num) consumes a column number and
;; produces a list of symbols in said column

;; Examples:
(check-expect (get-column grid1 0) (list '_ '_ '_))
(check-expect (get-column grid2 1) (list 'O 'X '_ '_ '_))
(check-expect (get-column grid3 1) (list 'O 'X '_ '_ 'X))

;; get-column: T3Grid Int -> (listof Sym)
(define (get-column T3Grid column-num)
  (cond
    [(empty? T3Grid) empty]
    [(cons? T3Grid) (cons (list-ref (first T3Grid) column-num)
                          (get-column (rest T3Grid) column-num))]))

;; Tests:
(check-expect (get-column grid2 2) (list 'O 'O '_ '_ '_))
(check-expect (get-column grid3 2) (list 'O 'O 'X '_ 'X))
(check-expect (get-column grid3 3) (list 'O '_ '_ '_ 'X))
(check-expect (get-column grid3 0) (list 'O 'X '_ '_ '_))

;;
;; d)
;;

;; (will-win? T3Grid row-num column-num player) consumes a T3Grid, row number, column number,
;; and player ('X or 'O) to produce true if the player would win by placing a marker at the location
;; and false otherwise
;; Note: if the location is not blank, then it is an illegal move and the player cannot win

;; Examples:
(check-expect (will-win? grid3 4 0 'X) true)
(check-expect (will-win? grid3 0 4 'X) false)
(check-expect (will-win? grid3 0 4 'O) true)
(check-expect (will-win? grid3 4 0 'O) false)
(check-expect (will-win? grid3 2 2 'O) false)
(check-expect (will-win? grid3 3 2 'O) false)

;; will-win?: T3Grid Int Int Sym -> Bool
;; Requires: row-num and column-num must be a valid, placeable location wihtin the T3Grid
(define (will-win? T3Grid row-num column-num player)
  (cond
    [(not (symbol=? '_ (grid-ref T3Grid row-num column-num))) false]
    [(symbol=? '_ (grid-ref T3Grid row-num column-num))
     (cond
       [(= (- (length (first T3Grid)) 1)
           (row-counter (list-ref T3Grid row-num) player)) true]
       [(= (- (length (get-column T3Grid column-num)) 1)
           (row-counter (get-column T3Grid column-num) player)) true]
       [else false])]))

;; Tests:
(check-expect (will-win? grid1 0 0 'X) false)
(check-expect (will-win? grid1 0 1 'X) false)
(check-expect (will-win? grid1 0 2 'X) false)
(check-expect (will-win? grid1 1 0 'X) false)
(check-expect (will-win? grid1 1 1 'X) false)
(check-expect (will-win? grid1 1 2 'X) false)
(check-expect (will-win? grid1 2 0 'X) false)
(check-expect (will-win? grid1 2 1 'X) false)
(check-expect (will-win? grid1 2 2 'X) false)

(check-expect (will-win? grid1 0 0 'O) false)
(check-expect (will-win? grid1 0 1 'O) false)
(check-expect (will-win? grid1 0 2 'O) false)
(check-expect (will-win? grid1 1 0 'O) false)
(check-expect (will-win? grid1 1 1 'O) false)
(check-expect (will-win? grid1 1 2 'O) false)
(check-expect (will-win? grid1 2 0 'O) false)
(check-expect (will-win? grid1 2 1 'O) false)
(check-expect (will-win? grid1 2 2 'O) false)
