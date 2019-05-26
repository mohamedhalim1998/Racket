;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



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

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions:

;; game -> game
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                   ; game
    (on-tick   tock)     ; game -> game
    (to-draw   render)   ; game -> Image
;    (stop-when ...)      ; game -> Boolean
    (on-key    handle-key)))    ; game KeyEvent -> game

;; game -> game
;; produce the next game screen
(check-expect (tock G0) (make-game empty empty (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0))))
(check-expect (tock G1) (make-game empty empty (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1))))
(check-expect (tock G2) (make-game
                         (list (make-invader 152 102 1))
                         (list (make-missile 150 290))
                         (make-tank 52 1)))
(check-expect (tock G3) G3)
(check-expect (tock (make-game (list I1) (list M2) T1)) (make-game empty empty (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1))))

;; (define (tock ws) G0)


(define (tock s)
  (if (landed? (game-invaders s))
      s
      (make-game (update-loinvader (add-invader(reduce-invaders(game-missiles s) (game-invaders s))))
                 (update-lom (reduce-missile(game-missiles s) (game-invaders s)))
                 (update-tank (game-tank s)))))


;; Tank -> Tank
;; produce the next tank
(check-expect (update-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect (update-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect (update-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))
;; (define (update-tank t) t)
(define (update-tank t)
      (if (going-right?(tank-dir t))
      (if (> (+ (tank-x t) TANK-SPEED) WIDTH)
          t
          (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))
      (if (< (- (tank-x t) TANK-SPEED) 0)
          t
          (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))))

;; Natural[-1,1] -> Boolean
;; produce true if the tank going right (1)
(check-expect (going-right? 1) true)
(check-expect (going-right? -1) false)
;; (define (going-right? n) false)

(define (going-right? n)
  (= 1 n))
 

;; LOM -> LOM
;; update evrey missile in the list
(check-expect (update-lom empty) empty)
(check-expect (update-lom (list M1)) (list (make-missile (missile-x M1) (-  (missile-y M1) MISSILE-SPEED)) ))
(check-expect (update-lom (list M1 M2)) (list (make-missile (missile-x M1) (-  (missile-y M1) MISSILE-SPEED))
                                              (make-missile (missile-x M2) (-  (missile-y M2) MISSILE-SPEED)) ))

;; (define (update-lom lom) empty)
#;
(define (update-lom lom)
  (cond[(empty? lom) empty]
       [else (... (fn-for-missile (first lom))
                  (update-lom (rest lom)))]))

(define (update-lom lom)
  (cond[(empty? lom) empty]
       [else (cons (update-missile (first lom))
                   (update-lom (rest lom)))]))
;; Missile -> Missile
;; update one missile
(check-expect (update-missile M1) (make-missile (missile-x M1) (- (missile-y M1)MISSILE-SPEED)))
(check-expect (update-missile M2) (make-missile (missile-x M2) (- (missile-y M2)MISSILE-SPEED)))
;; (define (update-missile m) m)

(define (update-missile m)
  (make-missile (missile-x m) (-  (missile-y m) MISSILE-SPEED)))

;; Missile loinvader -> Boolean
;; produce true if the missile hit invader
(check-expect (hit-invader? M2 (list I1)) true) 
(check-expect (hit-invader? M3 (list I1 )) true)
(check-expect (hit-invader? M1  (list (make-invader 200 395 10))) false)

;; (define (hit-invader? m loinvader) false)
(define (hit-invader? m loi)
  (cond[(empty? loi) false]
       [else (if (and (<= (abs (-(missile-x m) (invader-x (first loi)))) 10)
                      (<= (abs (-(missile-y m) (invader-y (first loi)))) 10))
                 true
                 (hit-invader? m (rest loi))
                 )]))
;; lom  invader -> Boolean
;; produce true if the missile hit invader
(check-expect (hit-missile? (list M1) I1) false) 
(check-expect (hit-missile? (list M1 M2) I1) true)
(check-expect (hit-missile? (list M1)   (make-invader 200 395 10)) false)

;; (define (hit-missile? m loinvader) false)
(define (hit-missile? lom i)
  (cond[(empty? lom) false]
       [else (if (and (<= (abs (-(missile-x (first lom)) (invader-x i))) 10)
                      (<= (abs (-(missile-y (first lom)) (invader-y i))) 10))
                 true
                 (hit-missile? (rest lom) i)
                 )]))
;; update evrey invader
(check-expect (update-loinvader empty) empty)
(check-expect (update-loinvader (list I1)) (list (make-invader
                                                  (+ (* INVADER-X-SPEED (invader-dx I1)) (invader-x I1))
                                                  (+ INVADER-Y-SPEED (invader-y I1)) (invader-dx I1))))
(check-expect (update-loinvader (list I1 I2)) (list (make-invader
                                                     (+ (* INVADER-X-SPEED (invader-dx I1)) (invader-x I1))
                                                     (+ INVADER-Y-SPEED (invader-y I1)) (invader-dx I1))
                                                    (make-invader
                                                     (+ (* INVADER-X-SPEED (invader-dx I2)) (invader-x I2))
                                                     (+ INVADER-Y-SPEED (invader-y I2)) (invader-dx I2))))
;; (define (update-loinvader loinvader)empty )
(define (update-loinvader loinvader)
  (cond [(empty? loinvader) empty]
        [else (cons (update-invader (first loinvader))
                    (update-loinvader (rest loinvader)))]))
;; invader -> invader
;; update  the given invader
(check-expect (update-invader I1) (make-invader (+ (* (invader-dx I1) INVADER-X-SPEED) (invader-x I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1)) )
;; (define (update-invader i) i)
(define (update-invader i)
  (if (> (invader-dx i) 0)
      (if(>= (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i)) WIDTH)
        (make-invader (invader-x i) (invader-y i) (-(invader-dx i)))
        (make-invader (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i)))
      (if(<= (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i)) 0)
        (make-invader (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i)) (+ (invader-y i) INVADER-Y-SPEED) (-(invader-dx i)))
        (make-invader (+ (* (invader-dx i) INVADER-X-SPEED) (invader-x i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i)))
      ))
(define G5 (make-game (list (make-invader 150 0 1)) empty T0))

;; loinvader -> boolean
;; produce true if one of invader landed
(check-expect (landed? empty) false)
(check-expect (landed? (list I1)) false)
(check-expect (landed? (list I2)) true)
(check-expect (landed? (list I1 I2)) true)
;; (define (landed? loinvader) false)
(define (landed? loinvader)
  (cond [(empty? loinvader) false]
        [else (if (>= (invader-y (first loinvader)) HEIGHT)
                  true
                  (landed? (rest loinvader)))]))
;; lom  loi -> lom
;; produce the remaing missile
(check-expect (reduce-missile empty empty) empty)
(check-expect (reduce-missile (list M1) (list I1)) (list M1))
(check-expect (reduce-missile (list M1 M2)(list I1)) (list M1))
(check-expect (reduce-missile (list M1 (make-missile 150 -1))(list I1)) (list M1))

;; (define (reduce-missile lom) empty)
(define (reduce-missile lom loi)
  (cond[(empty? lom) empty]
       [else (if (or (< (missile-y (first lom)) 0) (hit-invader? (first lom) loi))
                 (reduce-missile (rest lom) loi)
                 (cons (first lom) (reduce-missile (rest lom) loi)))]))

;; lom  loi -> lom
;; produce the remaing invaders
(check-expect (reduce-invaders empty empty) empty)
(check-expect (reduce-invaders (list M1) (list I1)) (list I1))
(check-expect (reduce-invaders (list M1 M2)(list I1)) empty)
(check-expect (reduce-invaders (list M1 M2)(list (make-invader 150 200 10)  I1)) (list(make-invader 150 200 10) ))

;; (define (reduce-invaders lom) empty)
(define (reduce-invaders lom loi)
  (cond[(empty? loi) empty]
       [else (if (hit-missile? lom (first loi))
                 (reduce-invaders lom (rest loi))
                 (cons (first loi) (reduce-invaders lom (rest loi))))]))
;; game -> Image
;; render game
(check-expect (render G0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) MTS))
(check-expect (render G1) (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) MTS))
(check-expect (render G2)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image MISSILE (missile-x M1) (missile-y M1)               
                                        (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) MTS))))
;; (define (render ws) MTS)

(define (render s)
   (render-loinvader (game-invaders s) (render-lom (game-missiles s) (render-tank (game-tank s) MTS))))
;; Tank Image-> Image
;; render the tank img at postion
(check-expect (render-tank T0 MTS) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) MTS))
(check-expect (render-tank T1 MTS) (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) MTS))
; (define (render-tank t img) empty-image)
(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))
;; loi Image -> image
;; render loi
(check-expect (render-loinvader empty MTS) MTS)
(check-expect (render-loinvader (list I1) MTS) (place-image INVADER (invader-x I1) (invader-y I1)  MTS))
(check-expect (render-loinvader (list I1 I2) MTS) (place-image INVADER (invader-x I2) (invader-y I2) (place-image INVADER (invader-x I1) (invader-y I1) MTS)))

;; (define (render-loinvader loi img) img)
(define (render-loinvader loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                           (render-loinvader (rest loi) img))]))



;; lom Image -> image
;; render lom
(check-expect (render-lom empty MTS) MTS)
(check-expect (render-lom (list M1) MTS) (place-image MISSILE (missile-x M1) (missile-y M1)  MTS))
(check-expect (render-lom (list M1 M2) MTS) (place-image  MISSILE (missile-x M2) (missile-y M2) (place-image MISSILE (missile-x M1) (missile-y M1) MTS)))
;; (define (render-lom lom img) img)
(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                           (render-lom (rest lom) img))]))
;; game key -> game
(check-expect (handle-key G0 " ") (make-game
                                   empty
                                   (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT (image-height TANK)))  (game-missiles G0))
                                   (game-tank G0)))
(check-expect (handle-key G0 "left") (make-game
                                   empty
                                   empty
                                   (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (handle-key G0 "right") (make-game
                                   empty
                                   empty
                                   (make-tank (tank-x (game-tank G0)) 1)))

(define (handle-key g ke)
  (cond [(key=? ke " ") (make-game
                                   (game-invaders g)
                                   (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (image-height TANK)))  (game-missiles g))
                                   (game-tank g))]
        [(key=? ke "left") (make-game
                                   (game-invaders g)
                                   (game-missiles g)
                                   (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game
                                   (game-invaders g)
                                   (game-missiles g)
                                   (make-tank (tank-x (game-tank g)) 1))]
                           
        [else g]))

;; loi -> loi
;; produce new invaders
(define (add-invader loi)
 (cond [(empty? loi) (cons (make-invader (random WIDTH) 0 1) empty)]
       [else (if (>= (count loi) 5)
                 loi
                   (cons (make-invader (random WIDTH) 0 1) loi))]))
;; loi -> Natrtural
;; count the invaders
(check-expect (count empty) 0)
(check-expect (count (list I1)) 1)
(check-expect (count (list I1 I2)) 2)
;; (define (count loi) 0)
(define (count loi)
  (cond[(empty? loi) 0]
       [else (+ 1 (count (rest loi)))]))
     
              
