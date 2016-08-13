(require (file "~/helper.rkt"))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Hanoi3.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)

;;=======CONSTANTS========

(define POLES 3)
(define RINGS 3) ; number of rings (1 is the smalles, 4 is the largest)
(define ALL-VALS (build-list RINGS add1)) 



;; Ring is Natural
;; ring with its number as a size from 1
(define RING1 1) ; smallest ring
(define RING4 4) ; largest ring



;; Pole is (listof Ring)
;; a pole with rings
(define POLE1 (list 1 2 3 4))
;  1
;  2
;  3
;  4 

(define POLE2 (list 1 2 4))
;  1
;  2
;  4

(define POLE3 empty)           ;empty pole



;; Game is (listof Pole)
;; list of poles

(define BLANK
  (make-list POLES '()))
; (list '()
;       '()
;       '())


(define GAME1        ; invalid
  (list '()          ; |  |  |
        '(3 2)       ; |  3  1   
        '(1 4)))     ; |  2  4

(define GAME2        ;     
  (list '(3 4)       ; |  |  |
        '(1)         ; 3  |  |
        '(2)))       ; 4  1  2

(define STARTB
  (cons (build-list RINGS add1) (make-list (- POLES 1) '())))

; (list '(1 2 3 4)
;       '()
;       '())

;  1  |  |
;  2  |  |
;  3  |  |
;  4  |  |

(define WINB
  (reverse STARTB))

; (list '()
;       '()
;       '(1 2 3 4))

; |  |  1  
; |  |  2  
; |  |  3 
; |  |  4



;;=======FUNCTIONS========


;; Game -> Game
;; produce a solution for g; or false if g is unsolvable
;; Assume: g is valid

(define (solve g)
  (local [(define (solve--g g visited)
            (if (solved? g)
                (list (reverse visited) (length visited))
                (solve--log (next-games-w/filter g visited) visited)))
          
          (define (solve--log log visited)
            (cond [(empty? log) false]
                  [else
                   (local [(define try (solve--g (first log) (cons (make--situation (first log)) visited)))]
                     (if (not (false? try))
                         try
                         (solve--log (rest log) visited)))]))]
    (solve--g g (list (make--situation g)))))



;; Game -> Boolean
;; produce true if game is solved

; (check-expect (solved? WINB) true)
; (check-expect (solved? GAME2) false)

#;
(define (solved? g)
  (if (= (length (list-ref g (- POLES 1))) RINGS)
      (andmap = (list-ref g (- POLES 1)) ALL-VALS)
      false))

(define (solved? g)
  (ormap (lambda(x) (= (length x) RINGS)) (rest g)))


;; Game (listof Game) -> (listof Game)
;; produce all the games possible after one steps, where each game has not been visited yet

(define (next-games-w/filter g visited)
  (local [(define (newlist log visited)
            (cond [(empty? log) empty]
                  [else
                   (if (and (not (empty-pole-inbetween? (remove-initial-empty-poles (make--situation (first log)))))
                            (not (member? (make--situation (first log)) visited)))
                       
                       (cons (first log) (newlist (rest log) (cons (make--situation (first log)) visited)))
                       (newlist (rest log) visited))]))]
    (newlist (next-games g) visited)))



;; Game -> (listof Game)
;; produce all the games possible after one steps

; (check-expect (next-games STARTB) (list (list '(2 3 4)
;                                               '(1)
;                                               '())
;                                         (list '(2 3 4)
;                                               '()
;                                               '(1))))
; (check-expect (next-games GAME2) (list (list '(1 3 4)
;                                              '()
;                                              '(2))
;                                        (list '(3 4)
;                                              '()
;                                              '(1 2))
;                                        (list '(2 3 4)
;                                              '(1)
;                                              '())))



(define (next-games g)
  (local [(define (next--games lor)
            (cond [(empty? lor) empty]
                  [else
                   (append (ring-to-game g (first lor))
                           (next--games (rest lor)))]))]
    (next--games (can-move-rings g))))



;; Game Ring -> (listof Game)
;; produce all games possible after moving the given ring

; (check-expect (ring-to-game STARTB 1) (list (list '(2 3 4)
;                                                   '(1)
;                                                   '())
;                                             (list '(2 3 4)
;                                                   '()
;                                                   '(1))))
; (check-expect (ring-to-game GAME2 1) (list (list '(1 3 4)
;                                                  '()
;                                                  '(2))
;                                            (list '(3 4)
;                                                  '()
;                                                  '(1 2))))


(define (ring-to-game g r)
  (local [(define (ring-game poles)
            (cond [(empty? poles) empty]
                  [else
                   (cons (putit (takeit g r) r (first poles))
                         (ring-game (rest poles)))]))]
    (ring-game (find-poles g r))))



;; Game Ring -> (listof Natural)
;; find valid poles to move the given ring to

; (check-expect (find-poles STARTB 1) (list 1 2))
; (check-expect (find-poles GAME2 2) (list 0))
; (check-expect (find-poles GAME2 1) (list 0 2))


(define (find-poles g r)
  (local [(define (find-pole g rsf acc gprev)
            (cond [(empty? g) rsf]
                  [else
                   (find-pole (rest g)
                              (if (and (changed? (putit g r 0) gprev) (or (empty? (first g)) (< r (first (first g)))))
                                  (cons acc rsf)
                                  rsf)
                              (add1 acc)
                              (rest gprev))]))]
    (find-pole (takeit g r) empty 0 g)))
