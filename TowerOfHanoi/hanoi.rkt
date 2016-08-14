(require (file "~/helper.rkt"))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Hanoi4.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)
;; all changes are based on 3.0
(define POLES 5)
(define RINGS 4)

(define ALL-VALS (build-list RINGS add1))

(define STARTB
  (cons (build-list RINGS add1) (make-list (- POLES 1) '())))

(define (solve g)
  (local [(define (solve--gm g visited)
            (if (solve? g)
                (list (reverse visited) (- (length visited) 1))
                (solve--log (next-games-w/filter g visited) visited)))
          
          (define (solve--log log visited)
            (cond [(empty? log) false]
                  [else
                   (local [(define try (solve--gm (first log) (cons (make--situation (first log)) visited)))]
                     (if (not (false? try))
                         try
                         (solve--log (rest log) visited)))]))
          ]
    (solve--gm g (list (make--situation g)))))

;; criterion to win
(define (solve? g)
  (ormap (λ(x) (= (length x) RINGS)) (rest g)))


;; filter w/ previous situation
(define (next-games-w/filter g pre)
  (local [(define (newlst g pre)
            (cond [(empty? g) empty]
                  [else
                   (if (member? (make--situation (first g)) pre)
                       (newlst (rest g) pre)
                       (cons (first g) (newlst (rest g) (cons (make--situation (first g)) pre))))]))]
    (newlst (next-games g) pre)))




;; next possible moves
(define (next-games g)
  (local [(define (next--games vals)
            (cond [(empty? vals) empty]
                  [else
                   (append (val-to-game g (first vals))
                           (next--games (rest vals)))]))]
    (next--games (can-move-vals g))))




;; value to game board
(define (val-to-game g val)
  (local [(define (val-game movs)
            (cond [(empty? movs) empty]
                  [else
                   (cons (putit (takeit g val) val (first movs))
                         (val-game (rest movs)))]))]
    (val-game (;find-poles-w/filter
               find-poles
               g val))))






;; find valid pole to move for a value !!!!!!!!!!!!!!!!!!!!!! this changed  !!!! (3.2)
(define (find-poles g num)
  (local [(define (find-pole g rsf acc g1 ginitial)
            (cond [(empty? g) rsf] ; here
                  [else
                   (find-pole (rest g)
                              (if (and (changed? (putit g num 0) g1)
                                       (if (num-alone? ginitial num)
                                           
                                           (andmap (λ(x) (< num x)) (first g))
                                           (or (empty? (first g)) (< num (first (first g))))))
                                  (cons acc rsf)
                                  rsf)
                              (add1 acc)
                              (rest g1)
                              ginitial)]))]
    (find-pole (takeit g num) empty 0 g g)))

(define (find-fixed g r)
  (local [(define (find-the-fixed g acc)
            (cond [(empty? g) empty]
                  [else
                   (if (member? r (first g))
                       (list acc)
                       (find-the-fixed (rest g) (add1 acc)))]))]
    (find-the-fixed g 0)))


;; movable values !!!!!!!!!!!!!!!!!!!!!! this changed !!!!!!!! 
(define (can-move-vals g)
  ; here (3.2)
  (reverse (filter (λ(x) (and (can-take? g x) (can-drop? g x)
                              (not (fixed? g x)) ;; !!!! here(4.0)
                              )) ALL-VALS)))


;; take the move one
(define (takeit g num)
  (cond [(empty? g) empty]
        [else
         (cons (if (empty? (first g))
                   (first g)
                   (if (= (first (first g)) num)
                       (rest (first g))
                       (first g)))
               (takeit (rest g) num))]))

;; put the move one at a position
(define (putit g num pos)
  (cond [(empty? g) empty]
        [else
         (if (= pos 0)
             (cons (cons num (first g)) (putit (rest g) num (sub1 pos)))
             (cons (first g) (putit (rest g) num (sub1 pos))))]))

;; ASSUME: Board is not BLANK
(define (can-take? g num)
  (cond [(empty? g) false]
        [else
         (if (not (empty? (first g)))
             (if (= (first (first g)) num)
                 true
                 (can-take? (rest g) num))
             (can-take? (rest g) num))]))

;; ASSUME: Board not BLANK and Valid
(define (can-drop? g num)
  (cond [(empty? g) false]
        [else
         (if (not (empty? (first g)))
             (if (> (first (first g)) num)
                 true
                 (can-drop? (rest g) num))
             true)]))

;; check if situation is unchanged if ring is put
(define (changed? g pre)
  (or (not (= (length (make--situation g)) (length (make--situation pre))))
      (not (andmap = (make--situation g) (make--situation pre)))))

;; A situation
(define (make--situation g)
  (cond [(empty? g) empty]
        [else
         (append (first g) '(0) (make--situation (rest g)))]))


;; !!!!!!!!!!!!!!! NEW feature in 4.0 
;; make the number fixed, e.g in four rings, if 4 is not on the first pole, then cannot move 4 again, if 3 is upon 4 and 4 is fixed, cannot move 3 again
#;#;#;#;#;
(check-expect (fixed? (list '(1 2 3)
                            '(4)
                            '())
                      4) true)
(check-expect (fixed? (list '(3 4)
                            '(1 2)
                            '())
                      3) false)
(check-expect (fixed? (list '(4)
                            '(1 2 3)
                            '())
                      4) false)
(check-expect (fixed? (list '(1 2)
                            '(3 4)
                            '())
                      3) true)

(check-expect (fixed? (list '(1)
                            '(4)
                            '(2 3))
                      2)
              false)
(define (fixed? g r)
  (cond [(= r RINGS) (if (member? r (first g))
                           false
                           true)]
        [else
         (local [(define (isfixed? lop r) 
                   (cond [(empty? lop) false]
                         [else
                          (if (andmap (λ(x) (member? x (first lop))) (filter (λ(x) (> x (- r 1))) ALL-VALS))
                              true
                              (isfixed? (rest lop) r))]))]
           (isfixed? (rest g) r))]))


;; avoid the move from empty pole to empty pole


(define (num-alone? g num)
  (local [(define pole 0)
          (define position  (first(find-fixed g num)))
          (define acc false)]
    (begin
      (for-each (λ(x)
                  (begin
                    (if (and (= pole position) (empty? x))
                        (set! acc true)
                        void)
                    (set! pole (+ pole 1))))
                (takeit g num))
      acc)))




(solve STARTB)