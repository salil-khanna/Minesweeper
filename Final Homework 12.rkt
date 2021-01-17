;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Final Homework 12|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3
;; COPIED DATA DEFS FROM HW10
;; A Content is one of:
;; - Nat
;; - #t
;; and represents either a count of the number of mines surrounding
;; a cell or a mine itself

(define CONTENT-BLANK 0)
(define CONTENT-MINE #t)

;; content-template : Content -> ???
(define (content-template c)
  (cond [(number? c) (... c ...)]
        [(boolean? c) ...]))

(define-struct hidden [con])
(define-struct visible [con])
(define-struct flagged [con])

;; A Cell is one of:
;; - (make-hidden Content)
;; - (make-visible Content)
;; - (make-flagged Content)
;; and represents either a hidden cell, a visible cell, or a flagged cell

(define CELL-H0 (make-hidden 0))
(define CELL-V0 (make-visible 0))
(define CELL-F0 (make-flagged 0))

(define CELL-H1 (make-hidden 1))
(define CELL-V1 (make-visible 1))
(define CELL-F1 (make-flagged 1))

(define CELL-HM (make-hidden #t))
(define CELL-VM (make-visible #t))
(define CELL-FM (make-flagged #t))

;; cell-template : Cell -> ???
(define (cell-template cell)
  (cond [(hidden? cell) (... (hidden-con cell) ...)]
        [(visible? cell) (... (visible-con cell) ...)]
        [(flagged? cell) (... (flagged-con cell) ...)]))

;; A Board is a [List-of [List-of Cell]
;; and represents a grid of cells that make up a game board

(define BOARD-EMPTY '())
(define BOARD-SEMI-EMPTY '(() () () ()))
(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))
(define BOARD-3X3-MID (list (make-list 3 CELL-H1)
                            (list CELL-H1 CELL-HM CELL-H1)
                            (make-list 3 CELL-H1)))
(define BOARD-LOSE (list (list CELL-VM)))
(define FLAG-BOARD-3X3 (list (make-list 3 CELL-H1)
                             (list CELL-H1 CELL-FM CELL-H1)
                             (make-list 3 CELL-H1)))
(define REVEAL-BOARD-3X3 (list (make-list 3 CELL-H1)
                               (list CELL-H1 CELL-VM CELL-H1)
                               (make-list 3 CELL-H1)))
;; board-template : Board -> ???
(define (board-template b)
  (cond [(empty? b) ...]
        [(cons? b) (... (row-template (first b))
                        (board-template (rest b)) ...)]))

;; row-template : [List-of Cell] -> ???
(define (row-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc) (... (cell-template (first loc))
                          (row-template (rest loc)) ...)]))

(define-struct game [board rev?])
;; A Game is a (make-game Board Boolean)
;; and represents a game of Minesweeper with a board of cells and a flag that is
;; #t if the mouse is revealing cells and #f if it is flagging them

(define GAME-EMPTY (make-game BOARD-EMPTY #t))
(define GAME-2X2-T (make-game BOARD-2X2-BLANK #t))
(define GAME-2X2-F (make-game BOARD-2X2-BLANK #f))
(define GAME-3X3-T (make-game BOARD-3X3-MID #t))
(define GAME-3X3-F (make-game BOARD-3X3-MID #f))
(define GAME-LOSE (make-game BOARD-LOSE #t))
(define GAME-FLAG (make-game FLAG-BOARD-3X3 #t))
(define GAME-REVEAL (make-game REVEAL-BOARD-3X3 #t))

;; game-template : Game -> ???
(define (game-template g)
  (... (board-template (game-board g))
       (game-rev? g) ...))

(require 2htdp/universe) ; so I can use mouse=?


;; mine-sweeper : Nat Nat -> Game
;; Play the minesweeper game with a square board of the given size and the
;; given number of mines
(define (mine-sweeper size mines)
  (mine-sweeper-from (make-game (generate-mine-board size mines) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-mine-board : Nat Nat -> Board
;; Generate a board of the given size with the given number of mines
(check-expect (generate-mine-board 0 0) '())
(check-expect (generate-mine-board 3 0) (make-list 3 (make-list 3 (make-hidden 0))))
(check-expect (board-count (generate-mine-board 4 5) cell-mine?) 5)
(check-error (generate-mine-board 2 7))
(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot fit " (number->string mines) " mines in a "
                            (number->string size) "x" (number->string size) " board."))
      (add-mines (make-list size (make-list size (make-hidden 0))) mines)))

;; add-mines : Board Nat -> Board
;; Add the given number of mines to the given board
;; ASSUMPTION: There is space for the given number of mines on the given board
(check-expect
 (add-mines (list (list (make-hidden 0))) 1)
 (list (list (make-hidden #t))))
(check-expect (board-count (add-mines BOARD-2X2-BLANK 2) cell-mine?) 2)
(define (add-mines board to-add)
  (if (zero? to-add) board
      (local [(define size (length board))
              (define row (random size))
              (define col (random size))]
        (if (cell-mine? (board-ref board row col))
            (add-mines board to-add)
            (add-mines (add-mine-at board row col) (sub1 to-add))))))

;; board-ref : Board Nat Nat -> Cell
;; Get the cell at the given row and column
;; ASSUMPTION: This is a valid location on the given board
(check-expect (board-ref BOARD-2X2-BLANK (random 2) (random 2)) (make-hidden 0))
(check-expect (board-ref BOARD-3X3-MID 1 1) (make-hidden #t))
(define (board-ref board row col)
  (list-ref (list-ref board row) col))

;; add-mine-at : Board Nat Nat -> Board
;; Add a mine at the given row and column and update the counts around it
;; ASSUMPTION: The cell at this location is not a mine
(check-expect
 (add-mine-at BOARD-2X2-BLANK 0 1)
 (list (list (make-hidden 1) (make-hidden #t))
       (list (make-hidden 1) (make-hidden 1))))
(check-expect (board-count (add-mine-at BOARD-3X3-MID 0 2) cell-mine?) 2)
(define (add-mine-at board row col)
  (local [(define size (length board))
          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given location and update by either adding a mine
          ;; or updating the count if it is neighboring the new mine
          (define (update-cell r c)
            (cond [(and (= row r) (= col c)) (make-hidden #t)]
                  [(and (<= (sub1 row) r (add1 row))
                        (<= (sub1 col) c (add1 col)))
                   (add-to-count (board-ref board r c))]
                  [else (board-ref board r c)]))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; add-to-count : Cell -> Cell
;; Add 1 to the given cell's count if it is not a mine
(check-expect (add-to-count (make-visible 0)) (make-visible 1))
(check-expect (add-to-count (make-hidden #t)) (make-hidden #t))
(check-expect (add-to-count (make-flagged 7)) (make-flagged 8))
(define (add-to-count cell)
  (local [;; add-to-content : Content -> Content
          ;; Add 1 to the given content if it is not a mine
          (define (add-to-content c)
            (if (number? c) (add1 c) c))]
    (cond [(visible? cell) (make-visible (add-to-content (visible-con cell)))]
          [(hidden?  cell) (make-hidden (add-to-content (hidden-con cell)))]
          [(flagged? cell) (make-flagged (add-to-content (flagged-con cell)))])))

;; board-count : Board [Cell -> Boolean] -> Nat
;; Count the number of cells on the given board that pass the given predicate
(check-expect (board-count BOARD-2X2-BLANK hidden?) 4)
(check-expect (board-count BOARD-3X3-MID cell-mine?) 1)
(define (board-count board check?)
  (foldr (λ (row sofar) (foldr (λ (cell sofar) (if (check? cell) (add1 sofar) sofar)) sofar row))
         0
         board))

;; cell-mine? : Cell -> Boolean
;; Is this cell a mine?
(check-expect (cell-mine? (make-visible 0)) #f)
(check-expect (cell-mine? (make-hidden #t)) #t)
(check-expect (cell-mine? (make-flagged 5)) #f)
(define (cell-mine? cell)
  (boolean?
   (cond [(visible? cell) (visible-con cell)]
         [(hidden?  cell) (hidden-con cell)]
         [(flagged? cell) (flagged-con cell)])))

;;;;;;;;;;;;;;;;;;;;;;;;;

;; mine-sweeper-from : Game -> Game
;; Play the minesweeper game with the given initial game state
(define (mine-sweeper-from g)
  (big-bang g
    [to-draw draw-game]
    [on-mouse change-if-click]
    [on-key change-mouse-state]
    [stop-when game-over? draw-game]))

(define CELL-SIZE 30) ; for pixel->grid conversion
(define CELL-HIDDEN (square CELL-SIZE "solid" "gainsboro"))
(define CELL-VISIBLE (square CELL-SIZE "solid" "dimgray"))
(define CELL-FLAGGED (overlay
                      (triangle CELL-SIZE "solid" "red")
                      (square CELL-SIZE "solid" "gainsboro")))
(define CELL-MINE ~embed:1~)



; draw-game : Game -> Image
; the handler for big bang that draws the world state of the game
(check-expect (draw-game GAME-3X3-T) (above
                                      (overlay/align "left"
                                                     "top"
                                                     (draw-board (game-board GAME-3X3-T))
                                                     (BACKGROUND (length (game-board GAME-3X3-T))))
                                      (INFORMATION GAME-3X3-T)))
(check-expect (draw-game GAME-FLAG) (above
                                     (overlay/align "left"
                                                    "top"
                                                    (draw-board (game-board GAME-FLAG))
                                                    (BACKGROUND (length (game-board GAME-FLAG))))
                                     (INFORMATION GAME-FLAG)))
(check-expect (draw-game GAME-REVEAL) (above
                                       (overlay/align "left"
                                                      "top"
                                                      (draw-board (game-board GAME-REVEAL))
                                                      (BACKGROUND (length (game-board GAME-REVEAL))))
                                       (INFORMATION GAME-REVEAL)))


(define (draw-game g)
  (local [(define LENGTH (length (game-board g)))
          (define back (BACKGROUND LENGTH))]
    (above (overlay/align "left" "top" (draw-board (game-board g)) back)
           (INFORMATION g))))


; BACKGROUND: Num -> Image
; creates a game background dependent on the length of the minefield
(check-expect (BACKGROUND 3) (rectangle 250 120 "solid" "white"))
(check-expect (BACKGROUND 9) (rectangle 270 300 "solid" "white"))
(define (BACKGROUND l)
  (if (< (* l CELL-SIZE) 250)
      (rectangle 250 (* (+ 1 l) CELL-SIZE) "solid" "white")
      (rectangle (* l CELL-SIZE) (* (+ 1 l) CELL-SIZE) "solid" "white")))


; draw-board: Board -> Image
; Creates an image from the game-board with the current cell states
(check-expect (draw-board BOARD-3X3-MID) (above (beside CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN)
                                                (square 1 "solid" "white")
                                                (beside CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN)
                                                (square 1 "solid" "white")
                                                (beside CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN)))
(check-expect (draw-board FLAG-BOARD-3X3) (above (beside CELL-HIDDEN
                                                         (square 1 "solid" "white")
                                                         CELL-HIDDEN
                                                         (square 1 "solid" "white")
                                                         CELL-HIDDEN)
                                                 (square 1 "solid" "white")
                                                 (beside CELL-HIDDEN
                                                         (square 1 "solid" "white")
                                                         CELL-FLAGGED
                                                         (square 1 "solid" "white")
                                                         CELL-HIDDEN)
                                                 (square 1 "solid" "white")
                                                 (beside CELL-HIDDEN
                                                         (square 1 "solid" "white")
                                                         CELL-HIDDEN
                                                         (square 1 "solid" "white")
                                                         CELL-HIDDEN)))
(check-expect (draw-board REVEAL-BOARD-3X3) (above (beside CELL-HIDDEN
                                                           (square 1 "solid" "white")
                                                           CELL-HIDDEN
                                                           (square 1 "solid" "white")
                                                           CELL-HIDDEN)
                                                   (square 1 "solid" "white")
                                                   (beside CELL-HIDDEN
                                                           (square 1 "solid" "white")
                                                           CELL-MINE
                                                           (square 1 "solid" "white")
                                                           CELL-HIDDEN)
                                                   (square 1 "solid" "white")
                                                   (beside CELL-HIDDEN
                                                           (square 1 "solid" "white")
                                                           CELL-HIDDEN
                                                           (square 1 "solid" "white")
                                                           CELL-HIDDEN)))
(define (draw-board b)
  (cond [(empty? (rest b)) (draw-list (first b))] 
        [(cons?  b) (above (draw-list (first b))
                           (square 1 "solid" "white"); creates a gap between rows
                           (draw-board (rest b)))]))


; draw-list : [List-of Cells] ->Image
; creates images out of the cells with the current cell-state
(check-expect (draw-list (first BOARD-3X3-MID)) (beside CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN
                                                        (square 1 "solid" "white")
                                                        CELL-HIDDEN))
(check-expect (draw-list (second FLAG-BOARD-3X3)) (beside CELL-HIDDEN
                                                          (square 1 "solid" "white")
                                                          CELL-FLAGGED
                                                          (square 1 "solid" "white")
                                                          CELL-HIDDEN))
(check-expect (draw-list (second REVEAL-BOARD-3X3)) (beside CELL-HIDDEN
                                                            (square 1 "solid" "white")
                                                            CELL-MINE
                                                            (square 1 "solid" "white")
                                                            CELL-HIDDEN))

(define (draw-list loc)
  (cond [(empty? (rest loc))  (draw-cell (first loc))]
        [(cons?  loc) (beside (draw-cell (first loc))
                              (square 1 "solid" "white"); creates a gap between cells
                              (draw-list (rest loc)))]))          

; draw-cell : Cell -> Image
; Draws a cell in the state that it is in
(check-expect (draw-cell (make-visible #t)) CELL-MINE)
(check-expect (draw-cell (make-visible 3)) (overlay (text (number->string 3)
                                                          15 "black")
                                                    CELL-VISIBLE))
(check-expect (draw-cell (make-visible 0)) CELL-VISIBLE)
(check-expect (draw-cell (make-flagged #t)) CELL-FLAGGED)
(check-expect (draw-cell (make-hidden 3)) CELL-HIDDEN)

(define (draw-cell c)
  (cond [(hidden? c) CELL-HIDDEN]
        [(visible? c) (cond [(boolean? (visible-con c)) CELL-MINE]
                            [(number? (visible-con c)) (if (zero? (visible-con c))
                                                           CELL-VISIBLE
                                                           (overlay
                                                            (text
                                                             (number->string (visible-con c))
                                                             15 "black")
                                                            CELL-VISIBLE))])]
        [(flagged? c) CELL-FLAGGED]))


(define TEXT-SIZE (round (/ CELL-SIZE 3)))
; INFORMATION: Game -> Image
; creates an image containg information about the current state of the game
(check-expect (INFORMATION GAME-3X3-F)
              (above/align "left"
                           (text "Mines in board: 1" 10 "black")
                           (text "Amount of Cells flagged: 0" 10 "black")
                           (text "Mouse state is currently in: Flagging Mode" 10 "black")))
(check-expect (INFORMATION GAME-LOSE)
              (above/align "left"
                           (text "Mines in board: 1" 10 "red")
                           (text "Amount of Cells flagged: 0" 10 "red")
                           (text "Mouse state is currently in: Revealing Mode" 10 "red")))
(define (INFORMATION g)
  (above/align "left"
               (text (string-append "Mines in board: " (number->string (mine-counter g)))
                     TEXT-SIZE (text-color g))
               (text (string-append "Amount of Cells flagged: " (number->string (flag-counter g)))
                     TEXT-SIZE (text-color g))
               (text (string-append "Mouse state is currently in: " (mouse-status g))
                     TEXT-SIZE (text-color g))))


; mine-counter: Game -> Nat 
; Counts the number of mines on a game-board
(check-expect (mine-counter GAME-3X3-T) 1)
(check-expect (mine-counter GAME-2X2-T) 0)
(define (mine-counter g)
  (local [(define BOARD (game-board g))
          ; mine-counter-board : Board -> Nat 
          ; gets the amount of mines on a board 
          (define (mine-counter-board b)
            (cond [(empty? b) 0]
                  [(cons?  b)
                   (+ (length (filter cell-mine? (first b))) (mine-counter-board (rest b)))]))]
    (mine-counter-board BOARD)))

; flag-counter: Game -> Nat
; Counts the number of flagged-cells on a game-board
(check-expect (flag-counter GAME-2X2-T) 0)
(define (flag-counter g)
  (local [(define BOARD (game-board g))
          ; flag-counter-board : Board -> Nat
          ; gets the amount of mines on a Board
          (define (flag-counter-board b)
            (cond [(empty? b) 0]
                  [(cons?  b)
                   (+ (length (filter flagged? (first b))) (flag-counter-board (rest b)))]))]
    (flag-counter-board BOARD)))

; mouse-status : Game -> String
; returns the mouse-status as a string, whether in reveal or flagged
(check-expect (mouse-status GAME-2X2-T) "Revealing Mode")
(check-expect (mouse-status GAME-2X2-F) "Flagging Mode")
(define (mouse-status g)
  (if (game-rev? g)
      "Revealing Mode"
      "Flagging Mode"))

; text-color: Game -> String
; determines the current color of the text based off the current state of the game
(check-expect (text-color GAME-3X3-T) "black")
(check-expect (text-color GAME-LOSE)"red")
(check-expect (text-color 
               (make-game (make-list 2 (make-list 2 CELL-FM)) #t))
              "green")
(define (text-color g) 
  (cond [(not (game-over? g)) "black"]
        [(board-lose? (game-board g)) "red"]
        [(board-win? (game-board g)) "green"]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; change-if-click : Game Number Number MouseEvent -> Game
;; Change the board if the user clicked on a space that needs to be revealed/flagged/unflagged
(check-expect (change-if-click GAME-3X3-T 10 5 "drag") GAME-3X3-T)
(check-expect
 (change-if-click GAME-2X2-T 5 5 "button-down")
 (make-game (make-list 2 (make-list 2 CELL-V0)) #t))
(check-expect
 (change-if-click GAME-2X2-F 5 5 "button-down")
 (make-game (list (list CELL-F0 CELL-H0) (make-list 2 CELL-H0)) #f))

(define (change-if-click g x y me)
  (if (mouse=? me "button-down")
      (if (game-rev? g)
          (make-game (reveal-cell (game-board g) (pixel->grid y) (pixel->grid x)) #t)
          (make-game (flag-cell (game-board g) (pixel->grid y) (pixel->grid x)) #f))
      g))

;; reveal-cell : Board Nat Nat -> Board
;; Reveal the cell at the given row and column
(check-expect
 (reveal-cell BOARD-3X3-MID 1 1)
 (list (make-list 3 (make-hidden 1))
       (list (make-hidden 1) (make-visible #t) (make-hidden 1))
       (make-list 3 (make-hidden 1))))
(check-expect
 (reveal-cell BOARD-2X2-BLANK 0 1)
 (make-list 2 (make-list 2 (make-visible 0))))
(define (reveal-cell board row col)
  (local [(define cell (board-ref board row col))
          (define clickable? (hidden? cell))
          ;; is-blank? : Content -> Boolean
          (define (is-blank? c)
            (and (number? c) (zero? c)))]
    (if (and clickable? (is-blank? (hidden-con cell)))
        (reveal-all-cells
         (reveal-single-cell board row col)
         (generate-neighbor-coordinates row col (length board)))
        (reveal-single-cell board row col))))

;; reveal-all-cells : Board [List-of Posn] -> Board
;; Reveal all the cells in the given list (x=row, y=col)
(check-expect (reveal-all-cells BOARD-3X3-MID '()) BOARD-3X3-MID)
(check-expect
 (reveal-all-cells BOARD-2X2-BLANK (list (make-posn 0 0) (make-posn 1 1)))
 (make-list 2 (make-list 2 (make-visible 0))))
(define (reveal-all-cells board all-coordinates)
  (foldr (λ (coord sofar) (reveal-cell sofar (posn-x coord) (posn-y coord))) board all-coordinates))

;; generate-neighbor-coordinates : Nat Nat Nat -> [List-of Posn]
;; Produces a list of coordinates of neighboring positions on a board of the given size
(check-expect
 (generate-neighbor-coordinates 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (generate-neighbor-coordinates 1 1 3)
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)
       (make-posn 1 0) (make-posn 1 2)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2)))
(define (generate-neighbor-coordinates row col boardsize)
  (local [(define can-sub1-row? (> row 0))
          (define can-add1-row? (< row (sub1 boardsize)))
          (define can-sub1-col? (> col 0))
          (define can-add1-col? (< col (sub1 boardsize)))]
    (append (if (and can-sub1-row? can-sub1-col?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-sub1-row? (list (make-posn (sub1 row) col)) '())
            (if (and can-sub1-row? can-add1-col?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-sub1-col? (list (make-posn row (sub1 col))) '())
            (if can-add1-col? (list (make-posn row (add1 col))) '())
            (if (and can-add1-row? can-sub1-col?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add1-row? (list (make-posn (add1 row) col)) '())
            (if (and can-add1-row? can-add1-col?) (list (make-posn (add1 row) (add1 col))) '()))))

;; reveal-single-cell : Board Nat Nat -> Board
;; Reveal the cell at the given location (no flooding)
(check-expect
 (reveal-single-cell BOARD-3X3-MID 1 1)
 (list (make-list 3 (make-hidden 1))
       (list (make-hidden 1) (make-visible #t) (make-hidden 1))
       (make-list 3 (make-hidden 1))))
(check-expect
 (reveal-single-cell BOARD-2X2-BLANK 0 1)
 (list (list (make-hidden 0) (make-visible 0))
       (make-list 2 (make-hidden 0))))
(define (reveal-single-cell board row col)
  (local [(define size (length board))
          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given location and make it visible
          ;; if it matches the desired location
          (define (update-cell r c)
            (if (and (= r row) (= c col))
                (make-cell-visible (board-ref board r c))
                (board-ref board r c)))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; make-cell-visible : Cell -> Cell
;; Make the given cell visible if it isn't flagged
(check-expect (make-cell-visible (make-visible 7)) (make-visible 7))
(check-expect (make-cell-visible (make-hidden #t)) (make-visible #t))
(check-expect (make-cell-visible (make-flagged 0)) (make-flagged 0))
(define (make-cell-visible cell)
  (cond [(visible? cell) cell]
        [(hidden? cell) (make-visible (hidden-con cell))]
        [(flagged? cell) cell]))

;; pixel->grid : Number -> Nat
;; Convert from pixel coordinates to grid coordinates
(check-expect (pixel->grid (add1 CELL-SIZE)) 1)
(check-expect (pixel->grid (sub1 (* 3 CELL-SIZE))) 2)
(define (pixel->grid n)
  (floor (/ n CELL-SIZE)))

;; flag-cell : Board Nat Nat -> Board
;; Flag the cell at the given row and column
(check-expect (flag-cell '() 4 5) '())
(check-expect
 (flag-cell BOARD-2X2-BLANK 1 0)
 (list (make-list 2 (make-hidden 0))
       (list (make-flagged 0) (make-hidden 0))))
(define (flag-cell board row col)
  (local [(define size (length board))
          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given position and flag it if necessary
          (define (update-cell r c)
            (if (and (= r row) (= c col))
                (make-cell-flagged (board-ref board r c))
                (board-ref board r c)))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; make-cell-flagged : Cell -> Cell
;; Flag the cell if it is hidden, unflag if flagged, otherwise leave alone
(check-expect (make-cell-flagged (make-visible 7)) (make-visible 7))
(check-expect (make-cell-flagged (make-hidden #t)) (make-flagged #t))
(check-expect (make-cell-flagged (make-flagged 3)) (make-hidden 3))
(define (make-cell-flagged cell)
  (cond [(visible? cell) cell]
        [(hidden?  cell) (make-flagged (hidden-con  cell))]
        [(flagged? cell) (make-hidden (flagged-con cell))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; change-mouse-state : Game KeyEvent -> Game
;; Change the state of the mouse if the user pressed the space bar
(check-expect (change-mouse-state GAME-EMPTY "x") GAME-EMPTY)
(check-expect (change-mouse-state GAME-2X2-F " ") GAME-2X2-T)
(define (change-mouse-state g key)
  (if (key=? key " ") (make-game (game-board g) (not (game-rev? g))) g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; game-over? : Game -> Boolean
;; Did the user either win or lose?
(check-expect (game-over? GAME-LOSE) #t)
(check-expect (game-over? GAME-EMPTY) #t)
(check-expect (game-over? GAME-3X3-T) #f)
(define (game-over? g)
  (or (board-win? (game-board g))
      (board-lose? (game-board g))))

;; board-win? : Board -> Boolean
;; Did the user reveal all the non-mine squares?
(check-expect (board-win? BOARD-EMPTY) #t)
(check-expect (board-win? BOARD-3X3-MID) #f)
(check-expect (board-win? (make-list 2 (make-list 2 CELL-FM))) #t)
(define (board-win? board)
  (local [;; mine-or-visible? : Cell -> Boolean
          ;; Is the given cell either a mine or visible?
          (define (mine-or-visible? cell)
            (cond [(visible? cell) #t]
                  [(hidden?  cell) (boolean? (hidden-con cell))]
                  [(flagged? cell) (boolean? (flagged-con cell))]))]
    (andmap (λ (row) (andmap mine-or-visible? row)) board)))

;; board-lose? : Board -> Boolean
;; Is there any visible mine square on the board?
(check-expect (board-lose? BOARD-3X3-MID) #f)
(check-expect
 (board-lose?
  (list (list (make-hidden #t) (make-visible 0))
        (list (make-flagged #t) (make-visible #t))))
 #t)
(define (board-lose? board)
  (local [;; visible-mine? : Cell -> Boolean
          ;; Is the given cell a mine that is visible?
          (define (visible-mine? cell)
            (and (visible? cell) (boolean? (visible-con cell))))]
    (ormap (λ (row) (ormap visible-mine? row)) board)))