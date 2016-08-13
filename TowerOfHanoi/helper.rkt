;;==========HELPERS=============

;; Game -> (listof RING)
;; produce the list of rings that can be moved 

; (check-expect (can-move-rings STARTB) (list 1))
; (check-expect (can-move-rings GAME2) (list 1 2))


(define (can-move-rings g)
  (filter (λ(x) (and (can-take? g x) (can-drop? g x))) ALL-VALS))



;; Game Ring -> Game
;; produce the new game without the given ring

; (check-expect (takeit STARTB 1) (list '(2 3 4)
;                                       '()
;                                       '()))
; ;1 | |     | | |  
; ;2 | |     2 | |   
; ;3 | |  -> 3 | |
; ;4 | |     4 | |
; 
; 
; (check-expect (takeit GAME2 2) (list '(3 4)
;                                      '(1)
;                                      '()))


(define (takeit g r)
  (cond [(empty? g) empty]
        [else
         (cons (if (empty? (first g))
                   (first g)
                   (if (= (first (first g)) r)
                       (rest (first g))
                       (first g)))
               (takeit (rest g) r))]))


;; Game Ring Natural -> Game
;; Put a ring onto a given pole

; (check-expect (putit (takeit STARTB 1) 1 1) (list '(2 3 4)
;                                                   '(1)
;                                                   '()))
; (check-expect (putit (takeit GAME2 2) 2 2) (list '(3 4)
;                                                  '(1)
;                                                  '(2)))


(define (putit g r pole)
  (if (= pole 0)
      (cons (cons r (first g)) (rest g))
      (cons (first g) (putit (rest g) r (sub1 pole)))))



;; Game Ring -> Boolean
;; produce true if the ring can be taken
;; ASSUME: Board is not BLANK

; (check-expect (can-take? STARTB 1) true)
; (check-expect (can-take? STARTB 2) false)
; (check-expect (can-take? GAME2 2) true)


(define (can-take? g r)
  (cond [(empty? g) false]
        [else
         (if (not (empty? (first g)))
             (if (= (first (first g)) r)
                 true
                 (can-take? (rest g) r))
             (can-take? (rest g) r))]))



;; Game Ring -> Boolean
;; produce true if the ring can be placed in the game
;; ASSUME: Board not BLANK and Valid

; (check-expect (can-drop? (list '(2 3 4)
;                                '()
;                                '()) 1) true)
; (check-expect (can-drop? (list '(1 4)
;                                '()
;                                '(2)) 3) true)
; (check-expect (can-drop? (list '(3)
;                                '(2)
;                                '(1)) 4) false)



(define (can-drop? g r)
  (cond [(empty? g) false]
        [else
         (if (not (empty? (first g)))
             (if (> (first (first g)) r)
                 true
                 (can-drop? (rest g) r))
             true)]))


;; Game Game -> Boolean
;; true if situation is unchanged if ring is put

; (check-expect (changed? (putit (takeit STARTB 1) 1 0) STARTB) false)
; (check-expect (changed? (putit (takeit STARTB 1) 1 2) STARTB) true)


(define (changed? g gprev)
  (or (not (= (length (make--situation g)) (length (make--situation gprev))))
      (not (andmap = (make--situation g) (make--situation gprev)))))


;; Game -> (listof Ring)
;; rewrite Game (which is list of list of rings) as a list of rings. 0 is the end of a pole

; (check-expect (make--situation (list '(1 2 3 4)
;                                      '()
;                                      '()))
;               (list 1 2 3 4 0 0 0))
; (check-expect (make--situation (list '(1 4)
;                                      '(2)
;                                      '(3)))
;               (list 1 4 0 2 0 3 0))
; (check-expect (make--situation (list '(3)
;                                      '()
;                                      '(1 2)))
;               (list 3 0 0 1 2 0))



(define (make--situation g)
  (cond [(empty? g) empty]
        [else
         (append (first g) '(0) (make--situation (rest g)))]))



(check-expect (empty-pole-inbetween? (list 1 2 4 0 0 5 3 0 0 0 0)) true)
(check-expect (empty-pole-inbetween? (list 1 5 4 0 1 2 3 0 0 0 0)) false)

(define (empty-pole-inbetween? lox)     
  (local [(define acc false)
          (define prev (first (rest lox)))
          (define prevprev (first lox))]
    (begin
      (for-each (lambda (x)
                  (if (and (= prev 0) (= prevprev 0) (not (= x 0)))
                  (set! acc true)
                  (begin (set! prevprev prev)
                         (set! prev x))))
                (rest (rest lox)))
      acc)))



(check-expect (remove-initial-empty-poles (list 0 0 0 1 2 3 0 4 5 0)) (list 1 2 3 0 4 5 0))
(check-expect (remove-initial-empty-poles (list 1 2 3 0 4 5 0)) (list 1 2 3 0 4 5 0))

(define (remove-initial-empty-poles lox)     
  (local [(define acc lox)
          (define first 0)]
    (begin
      (for-each (lambda (x)
                  (begin (if (not (= x 0))
                             (set! first 1)
                             (set! first first))
                  (if (and (= x 0) (= first 0))
                  (set! acc (rest acc))
                  (set! acc acc))))
                lox)
      acc)))

(solve STARTB)