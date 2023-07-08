;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")));saucer
(define CTR-INVADER (/ (image-width INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define PLACE-TANK (- HEIGHT TANK-HEIGHT/2))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders
(define LOI0 empty)
(define LOI1 (list I1 I2 I3))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-invader (rest loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))
;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of Missile
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-invader (rest lom)))]))
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main s)
  (big-bang s                   ; Game
    (on-tick   advance-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
     (stop-when game-over)      ; Game -> Boolean
    (on-key   handle-key)))    ; Game Integer Integer MouseKey  -> Game

;; Game -> Game
;; produce the next missles, invaders and tank
(check-random (advance-game G0) (make-game (advance-invaders (game-invaders G0) (game-missiles G0))
                                           (advance-missiles (game-missiles G0) (game-invaders G0))
                                           (advance-tank (game-tank G0))))

;(define (advance-game s) s)
(define (advance-game s)
  (make-game (advance-invaders (game-invaders s) (game-missiles s))
             (advance-missiles (game-missiles s) (game-invaders s))
             (advance-tank (game-tank s))))
;; ListOfInvader ListOfMissile -> ListOfInvader
;; produces randomly generated invaders that move in a 45 degree angle
(check-random (advance-invaders empty empty) (collision-invaders (next-invaders (spawn-invaders empty)) empty))
(define (advance-invaders loi lom)
  (collision-invaders (next-invaders (spawn-invaders loi)) lom))
;; ListOfInvader -> ListOfInvader
;; produce the list of invaders according to a INVADER-RATE
(check-random (spawn-invaders empty) (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty))
(check-expect (spawn-invaders (list (make-invader 20 50 1.5))) (list (make-invader 20 50 1.5)))
;(define (spawn-invaders loi) loi)
(define (spawn-invaders loi)
  (cond [(empty? loi) (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty)]
        [else (if (<= (random INVADE-RATE) 2) 
                   (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
                   loi)]))

;; ListOfInvader -> ListOfInvader
;; produce every invaders x coordinate increased by INVADER-X-SPEED and y coordinate by increased Invader-Y-SPEED
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders (list (make-invader 20 50 1.5)))
              (list (make-invader (+ INVADER-X-SPEED 20) (+ INVADER-Y-SPEED 50) 1.5)))
(check-expect (next-invaders (list (make-invader 200 50 -1.5)))
              (list (make-invader (- 200 INVADER-X-SPEED) (+ INVADER-Y-SPEED 50) -1.5)))
;(define (next-invaders loi) loi)
(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (pos-invader (first loi))
                   (next-invaders (rest loi)))]))
;;Invader -> Invader
;; produces a change in direction when an Invader hits the edge of the screen

;(define (pos-invader invader) invader)
(define (pos-invader invader)
  (cond [(> (invader-x invader) (- WIDTH CTR-INVADER))
         (make-invader (- WIDTH CTR-INVADER) (+ (invader-y invader) INVADER-Y-SPEED) (-(invader-dx invader)))]
        [(< (invader-x invader) CTR-INVADER)
         (make-invader  CTR-INVADER (+ (invader-y invader) INVADER-Y-SPEED) (-(invader-dx invader)))]
        [else (next-invader invader)]))
;; Invader -> Invader
;; produces an invaders x coordinate increased by INVADER-X-SPEED and y coordinate increased by Invader-Y-SPEED
(check-expect (next-invader (make-invader 20 50 1.5)) (make-invader (+ INVADER-X-SPEED 20) (+ INVADER-Y-SPEED 50) 1.5))
(check-expect (next-invader (make-invader 200 50 -1.5)) (make-invader (- 200 INVADER-X-SPEED ) (+ INVADER-Y-SPEED 50) -1.5))
;(define (next-invader invader) invader)
(define (next-invader invader)
  (cond [(> (invader-dx invader) 0) (make-invader
                                     (+ INVADER-X-SPEED (invader-x invader))
                                     (+ INVADER-Y-SPEED (invader-y invader))
                                     (invader-dx invader))]
        [(< (invader-dx invader) 0) (make-invader
                                      (- (invader-x invader) INVADER-X-SPEED)
                                      (+ INVADER-Y-SPEED (invader-y invader))
                                      (invader-dx invader))]))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; produces a list of removed invaders if they collided with a missile
(check-expect (collision-invaders empty empty) empty)
(check-expect (collision-invaders (cons (make-invader 200 140 1.5) empty) empty) (cons (make-invader 200 140 1.5) empty))
(check-expect (collision-invaders (list (make-invader 300 150 1.5)) (list (make-missile 300 140))) empty)
(check-expect (collision-invaders (list (make-invader 20 5 1.5) (make-invader 300 150 1.5)) (list (make-missile 300 140)))
              (list (make-invader 20 5 1.5)))
;(define (collision-invaders loi lom) loi)

(define (collision-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [(collision-invader? lom (first loi)) (collision-invaders (rest loi) lom)]
        [else (cons (first loi)
                   (collision-invaders (rest loi) lom))])) 
;; Invader ListOfMissile -> Boolean
(check-expect (collision-invader? (list (make-missile 300 140)) (make-invader 300 150 1.5)) true)
(check-expect (collision-invader? (list (make-missile 300 140)) (make-invader 20 5 1.5)) false)
;(define (collision-invader? lom invader) false)
(define (collision-invader? lom invader)
  (cond [(empty? lom) false]
        [else (if (check-collision? (first lom) invader)
                  true
                   (collision-invader? (rest lom) invader))]))

;; ListOfMissile ListOfInvaders -> ListOfMissile
;; produces a missile from the tank and up until the screen
(check-expect (advance-missiles empty empty) empty)
(check-expect (advance-missiles (list (make-missile 150 200)) empty) (list (make-missile 150 (- 200 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 150 (- 1 0))) empty) empty)
(check-expect (advance-missiles (cons (make-missile 150 200)
                                      (cons (make-missile 200 160) empty))
                                (cons (make-invader 200 150 10) empty)) (cons (make-missile 150 (- 200 MISSILE-SPEED)) empty))
              
;(define (advance-missiles lom loi) empty)
(define (advance-missiles lom loi)
  (on-screen (collision-missile (next-missiles lom) loi)))

;; ListOfMissile -> ListOfMissile
;; produce all y coordinate decreased by MISSILE-SPEED in the list
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list (make-missile 150 200)))
              (list (make-missile 150 (- 200 MISSILE-SPEED))))
(check-expect (next-missiles (list (make-missile 150 200) (make-missile 200 140)))
              (list (make-missile 150 (- 200 MISSILE-SPEED)) (make-missile 200 (- 140 MISSILE-SPEED))))
;(define (next-missiles lom) lom)
(define (next-missiles loi)
  (cond [(empty? loi) empty]
        [else (cons (next-missile (first loi))
                   (next-missiles (rest loi)))]))
;; Missile -> Missile
;; produces a missile with increased y coordinate by MISSILE-SPEED
(check-expect (next-missile (make-missile 150 200)) (make-missile 150 (- 200 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 200 140)) (make-missile 200 (- 140 MISSILE-SPEED)))
;(define (next-missile m) m)
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissiles ListOfInvader -> ListOfMissile
;; produces a list in which the missile dissapears when it hits the invader
(check-expect (collision-missile empty empty) empty)
(check-expect (collision-missile (cons (make-missile 300 140) empty) empty) (cons (make-missile 300 140) empty))
(check-expect (collision-missile (cons (make-missile 200 140) empty) (cons (make-invader 200 150 10) empty)) empty)
(check-expect (collision-missile (cons (make-missile 150 200)
                                      (cons (make-missile 200 140) empty))
                                            (cons (make-invader 200 150 10) empty))
              (cons (make-missile 150 200) empty))
;(define (collision-missile lom loi) lom)
(define (collision-missile lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [(collision? (first lom) loi) (collision-missile (rest lom) loi)]
        [else (cons (first lom) (collision-missile (rest lom) loi))]))
;; Missile ListOfInvader -> Boolean
;; produces true if missile hits an invader
(check-expect (collision? (make-missile 200 140) (list (make-invader 200 150 10))) true)
(check-expect (collision? (make-missile 250 200) (list (make-invader 200 150 10))) false)
;(define (collision? m loi) false)
(define (collision? m loi)
  (cond [(empty? loi) false]
        [else (if (check-collision? m (first loi))
                  true
                   (collision? m (rest loi)))]))
;; Missile Invader -> Boolean
;; produce true if missile is in territory of invader
(check-expect (check-collision? (make-missile 200 140) (make-invader 200 150 10)) true)
(check-expect (check-collision? (make-missile 250 200) (make-invader 200 150 10)) false)

;(define (check-collision? m invader) true);
(define (check-collision? m invader)
  (and (and (<= (- (invader-x invader) HIT-RANGE) (missile-x m))
            (<= (missile-x m) (+ (invader-x invader) HIT-RANGE)))
       (and (<= (- (invader-y invader) HIT-RANGE) (missile-y m))
            (<= (missile-y m) (+ (invader-y invader) HIT-RANGE)))))

;; ListOfMissile -> ListOfMissile
;;produce a list where all missiles beyond the screen are deleted from the list
(check-expect (on-screen empty) empty)
(check-expect (on-screen (list (make-missile 20 (- 0 1)))) empty)
(check-expect (on-screen (list (make-missile  20 10) (make-missile 20 (- 0 1))))(list (make-missile  20 10)))
;(define (on-screen lom) lom)

(define (on-screen lom)
  (cond [(empty? lom) empty]
        [else (if (on-screen? (first lom))
                  (cons (first lom) (on-screen (rest lom)))
                   (on-screen (rest lom)))]))
;; Missile -> Boolean
;; produce true if the missile is still on the screeen
(check-expect (on-screen? (make-missile 20 (- 0 1))) false)
(check-expect (on-screen? (make-missile  20 10)) true)
;(define (on-screen? m) true)
(define (on-screen? m)
  (<= 0 (missile-y m) HEIGHT))

;; Tank -> Tank
;; produces the next x coordinate of the tank depending on the sign dx
(check-expect (advance-tank (make-tank 20 1)) (make-tank (+ 20 TANK-SPEED) 1))
(check-expect (advance-tank (make-tank 20 -1)) (make-tank (- 20 TANK-SPEED) -1))

(check-expect (advance-tank (make-tank  WIDTH  1)) (make-tank (- WIDTH 10) -1))
(check-expect (advance-tank (make-tank 0 -1)) (make-tank (+ 0 10) 1))

;(define (advance-tank t) t)
(define (advance-tank t)
  (cond [(and (>= (tank-x t) WIDTH) (= (tank-dir t) 1)) (make-tank (- WIDTH 10) -1)]
        [(and (<= (tank-x t) 0) (= (tank-dir t) -1)) (make-tank (+ 0 10) 1)]
        [(= (tank-dir t) 1) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
        [(= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]))

;; Game -> Image
;; render images of invaders, missles and tank on the MTS
(check-expect (render-game (make-game empty empty (make-tank 20 1))) 
              (render-loinvader (game-invaders (make-game empty empty (make-tank 20 1)))
                                (render-lom (game-missiles (make-game empty empty (make-tank 20 1)))
                                            (place-image TANK 20 PLACE-TANK BACKGROUND))))
;(define (render-game s) BACKGROUND)
(define (render-game s)
  (render-loinvader (game-invaders s)
                    (render-lom (game-missiles s)
                                (render-tank (game-tank s)))))
;; ListOfInvader -> Image
;; produces Image of list of invaders
(check-expect (render-loinvader empty BACKGROUND) BACKGROUND)
(check-expect (render-loinvader (list (make-invader 200 100 1.5)) BACKGROUND) (place-image INVADER 200 100 BACKGROUND))
(check-expect (render-loinvader (list (make-invader 200 100 1.5) (make-invader 250 50 -1.5)) BACKGROUND)
              (place-image INVADER 200 100 (place-image INVADER 250 50 BACKGROUND)))
;(define (render-loinvader loi img) img)
(define (render-loinvader loi img)
  (cond [(empty? loi) img]
        [else (render-loinvader-on (first loi)
                   (render-loinvader (rest loi) img))]))
;; Invader Image -> Image
;; render the alien image on img
(check-expect (render-loinvader-on (make-invader 200 100 1.5) BACKGROUND) (place-image INVADER 200 100 BACKGROUND))
;(define (render-loinvader-on invader img) img)
(define (render-loinvader-on invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))

;; ListOfMissile -> Image
;; produces image of list of missiles
(check-expect (render-lom empty BACKGROUND) BACKGROUND)
(check-expect (render-lom (list (make-missile 200 100)) BACKGROUND) (place-image MISSILE 200 100 BACKGROUND))
(check-expect (render-lom (list (make-missile 200 100) (make-missile 30 40)) BACKGROUND)
              (place-image MISSILE 200 100 (place-image MISSILE 30 40 BACKGROUND)))
;(define (render-lom lom img) BACKGROUND)
(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else (render-lom-on (first lom)
                   (render-lom (rest lom) img))]))
;; Missile Image -> Image
;; renders the image of a missile on img
(check-expect (render-lom-on (make-missile 200 100) BACKGROUND) (place-image MISSILE 200 100 BACKGROUND))
(check-expect (render-lom-on (make-missile 30 40)  BACKGROUND) (place-image MISSILE 30 40 BACKGROUND))
;(define (render-lom-on m img) img)
(define (render-lom-on m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; Tank -> Image
;; prduces the image of a tank
(check-expect (render-tank (make-tank 20 1)) (place-image TANK 20 PLACE-TANK BACKGROUND))
;(define (render-tank t) empty-image)
(define (render-tank t)
  (place-image TANK (tank-x t) PLACE-TANK BACKGROUND))

;; Game Key -> Game
;; produces all the key events of the game
(check-expect (handle-key (make-game empty empty (make-tank 20 1)) "left")
              (make-game empty empty
                         (make-tank (- 20 TANK-SPEED) -1)))
(check-expect (handle-key (make-game empty empty (make-tank 20 1)) "right") (make-game empty empty
                                                                                       (make-tank (+ 20 TANK-SPEED) 1))
              )
;(define (handle-key s me) s)
(define (handle-key s key)
  (make-game (game-invaders s)
             (handle-missile (game-missiles s) (game-tank s) key)
             (handle-tank (game-tank s) key)))



;; ListOfMissile Tank Key -> ListOfMissile
;; handles all key events of the missiles
(check-expect (handle-missile empty (make-tank 20 1) " ") (list (make-missile 20 PLACE-TANK)))
(check-expect (handle-missile empty (make-tank 20 1) "a") empty)
(check-expect (handle-missile (list (make-missile 50 PLACE-TANK)) (make-tank 20 1) " ")
              (list (make-missile 20 PLACE-TANK) (make-missile 50 PLACE-TANK)))
;(define (handle-missile lom t key) empty)
(define (handle-missile lom t key)
  (cond [(key=? key " ") (cons (make-missile (tank-x t) PLACE-TANK) lom)]
        [else lom]))


;; Tank Key -> Tank
;; when the left arrow and right arrow are pressed the tank changes direction
(check-expect (handle-tank (make-tank 20 1) "left") (make-tank  (- 20 TANK-SPEED) -1))
(check-expect (handle-tank (make-tank 20 1) "right") (make-tank (+ 20 TANK-SPEED) 1))
;(define (handle-tank t key) t)
(define (handle-tank t key)
  (cond [(key=? key "left") (make-tank  (- (tank-x t) TANK-SPEED)  -1)]
        [(key=? key "right") (make-tank  (+ (tank-x t) TANK-SPEED) 1)]
        [else (make-tank (tank-x t) (tank-dir t))]))
;; Game -> Boolean
;; produces true if the invader reaches the bottom of the screen

(define (game-over g)
  (if (invaders-landed? (game-invaders g))
      true
      false))

;; ListOfInvader -> Boolean
;; produces true if an invader reaches the bottom of the screen

(define (invaders-landed? loi)
  (cond [(empty? loi) false]
        [else (if (>= (invader-y (first loi)) (- HEIGHT (/ (image-height INVADER) 2)))
                  true
                  (invaders-landed? (rest loi)))]))
(main G0)