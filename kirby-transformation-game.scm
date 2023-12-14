; kirby-transformation-game.scm

;;; CSC-151-02 (Fall 2023)
;;; Final Project: Kirby Transformation
;;; Authors: Khanh Do, Paul Lim, Seojin Pyoun, Sarah Qi
;;; Acknowledgement: We would like to acknowledge whoever created Kirby because it is so cute!
;;; GitHub repo: github.com/khanhdo05/kirby-game 

(import image)
(import lab)
(import canvas)
(import music)

(define canv (make-canvas 1000 600))

; -------------------
;;; BASE ROUND BALL |
; -------------------

;;; (ball length main-color shadow-color) -> drawing?
;;;   length: integer?, non-negative
;;;   main-color: string?, or predefined rgb color.
;;;  shadow-color: string?, or predefined rgb color.
;;; Draws a round object as a base image to make different objects for Kirby to eat.
(define ball
  (lambda (length main-color shadow-color)
    (overlay/offset -3 -3 
                    (circle (* length 0.87) "solid" main-color)
                    (circle length "solid" shadow-color))))

; ------------------------
;;; Object 0: Basic Ball |
; ------------------------

;; Color Palette
(define darker-pink (color 248 187 198 1))
(define star-yellow (color 255 230 69 1))

;; Star

;;; (make-star length fill color) -> drawing?
;;;   length: integer?, non-negative
;;;   fill: string?
;;;   color: string?
;;; Draws an image of a star.
(define make-star
  (lambda (length fill color)
    (path length
          length
          (list (pair 0 (* length (/ 3 8))) ; 1
                (pair (* length (/ 3 8)) (* length (/ 3 8))) ; 2
                (pair (/ length 2) 0) ; 3
                (pair (* length (/ 5 8)) (* length (/ 3 8))) ; 4
                (pair length (* length (/ 3 8))) ; 5
                (pair (* length 0.75) (* length (/ 5 8))) ; 6
                (pair (* length (/ 7 8)) length) ; 7
                (pair (/ length 2) (* length 0.75)) ; 8
                (pair (* length (/ 1 8)) length) ; 9
                (pair (* length 0.25) (* length (/ 5 8))) ;10
                (pair 0 (* length (/ 3 8)))
          )
          fill
          color)))

;;; (star length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws an image of a star with outline
(define star
  (lambda (length color1 color2)
    (overlay (make-star (+ length 0.8) "outline" color1)
             (make-star length "solid" color2))))

;; Complete ball

;;; (basic-ball length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the image of a complete basic Kirby ball.
(define basic-ball
  (lambda (length) 
    (overlay (star (* length 1.2) "white" star-yellow)
             (ball length "pink" darker-pink))))

;; Display to test
;(basic-ball 40)

; -------------------------
;;; OBJECT 1: TENNIS BALL |
; -------------------------

;; Color Palette
(define lemon-grass-green (color 197 230 54 1))
(define darker-lemon-grass-green (color 179 223 40 1))

;; White Curve

;;; (create-curve length) -> drawing?
;;;   length: integer?, non-negative
;;; Creates a single, thin white curve.
(define create-curve 
  (lambda (length)
    (ellipse (* length 1.4) (* length 0.8) "outline" "white")))

;;; (thick-curve length) -> drawing?
;;;   length: integer?, non-negative
;;; Creates a single, thick white curve.
(define thick-curve
  (lambda (length)
    (|> (range length (+ (* 0.1 length) length))
        (section map create-curve _)
        (section apply overlay _))))

;; Complete Tennis Ball
;;; (tennis-ball length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws a complete tennis ball with radius length.
(define tennis-ball
  (lambda (length)
    (let ([ball (ball length lemon-grass-green darker-lemon-grass-green)] ; Draw the ball
          [curve (thick-curve length)]) ; Draw the white lines across the ball
      (overlay/align "middle" "bottom" curve (overlay/align "middle" "top" curve ball)))))

;; Display to test
;(tennis-ball 40)

; ------------------------
;;; OBJECT 2: MARIO LOGO |
; ------------------------

;; Color Palette
(define darker-red (color 207 26 27 1))

;; Big M
;;; (letter-m length) -> drawing?
;;;   length : integer?, non-negative
;;; Creates the big letter M in the Mario logo.
(define letter-m
  (lambda (length color) 
    (path length                                          ; horizontal image size
          length                                          ; vertical image size
          (list (pair 0 (* length 0.75))                  ; 1
                (pair (* length 0.25) 0)                  ; 2
                (pair (/ length 2) (* length (/ 3 8)))    ; 3
                (pair (* length 0.75) 0)                  ; 4
                (pair length (* length 0.75))             ; 5
                (pair (* length (/ 7 8)) length)          ; 6
                (pair (* length 0.75) (* length (/ 3 8))) ; 7
                (pair (/ length 2) (/ length 2))          ; 8
                (pair (* length 0.25) (* length (/ 3 8))) ; 9
                (pair (/ length 8) length)                ; 10
          )
          "solid"                                         ; fill style
          color)))                                        ; color

;; Complete Mario Ball
;;; (mario-ball length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws a complete Mario ball for Kirby to eat.
(define mario-ball
  (lambda (length color1 color2)
    (overlay (letter-m (* length 1.35) color1)
             (ball length color2 darker-red))))

;; Display to test
;(mario-ball 40 "white" "red")

; --------------------------
;;; OBJECT 3: Medical Ball |
; --------------------------

;; Color Palette
(define bone-white (color 249 246 238 1))
(define darker-bone-white (color 237 234 222 1))

;; Red Cross

;;; (red-cross length) -> drawing?
;;;   length: integer?, non-negative
;;; Creates the red cross for the logo of the medical ball.
(define red-cross
  (lambda (length)
    (overlay
      (rectangle (* length 0.75) (* length 0.25) "solid" "red")
      (rectangle (* length 0.25) (* length 0.75) "solid" "red"))))

;; Complete Medical Ball

;;; (medical-ball length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws a complete medical ball for Kirby to eat.
(define medical-ball
  (lambda (length)
    (overlay (red-cross (* length 1.65))
             (ball length bone-white darker-bone-white))))

;; Display to test
;(medical-ball 40)

; -----------------------------
;;; OBJECT 4: Christmas Ball   |
; -----------------------------

;; Color Palette
(define darker-blue (color 49 40 102 0.9))

;;; (tree length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the green part of the noel tree.
(define tree
  (lambda (length)
    (overlay/offset (- (/ length 8)) (* length (/ 11 40)) 
                    (triangle (* length (/ 23 40)) "solid" "seagreen")
                    (overlay/offset (- (/ length 20)) (/ length 4) 
                                    (triangle (* length (/ 28 40)) "solid" "seagreen")
                                    (triangle 33 "solid" "seagreen")))))

;;; (tree-body length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the trunk and align that to make the whole tree.
(define tree-body
  (lambda (length)
    (above (circle (/ length 10) "solid" "gold") 
           (tree length)
           (rectangle (/ length 10) (/ length (/ 10 3)) "solid" "brown"))))

;;; (christmas-ball length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the complete christmas ball for Kirby to eat.
(define christmas-ball
  (lambda (length)
    (overlay (tree-body length) (ball length "darkslateblue" darker-blue))))

;; Display to test
;(christmas-ball 40)
        
; ------------------------
;;; Kirby 0: Basic Kirby |
; ------------------------

;;; (base-head size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the image of the base head for basic kirby.
(define base-head
  (lambda (size)
    (overlay/offset 
      (- (* 0.0005 size)) 
      (- (* 0.0005 size)) 
      (circle size "solid" "pink")
      (circle (* 1.008 size) "solid" "palevioletred"))))

;;; (arm size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws one arm.
(define arm
  (lambda (size)
    (overlay/offset 
      (- (* 0.0005 size)) 
      (- (* 0.0005 size)) 
      (ellipse (* 0.7 size) (* 0.5 size) "solid" "pink")
      (ellipse (* 0.71 size) (* 0.51 size) "solid" "palevioletred"))))

;;; (arms size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws two arms.
(define arms
  (lambda (size)
    (beside (arm size) (circle (* 0.6 size) "solid" "white") (arm size))))

;;; (head size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the body (head with arms) of kirby.
(define head
  (lambda (size)
    (overlay/align "middle" "center" (base-head size) (arms size))))

;;; (blush size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws blush
(define blush
  (lambda (size)
    (beside (ellipse (* 0.3 size) (* 0.15 size) "solid" "hotpink")
            (rectangle (* 0.8 size) (* 0.1 size) "solid" "pink")
            (ellipse (* 0.3 size) (* 0.15 size) "solid" "hotpink"))))

;;; (balck-eye-base size) -> drawing?
(define black-eye-base
  (lambda (size)
    (overlay/offset
      (- (* 0.04 size))
      (- (* 0.02 size))
      (ellipse (* 0.17 size) (* 0.3 size) "solid" "white")
      (ellipse (* 0.25 size) (* 0.6 size) "solid" "black"))))

;;; (eye-blue size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws blue part of eye.
(define eye-blue
  (lambda (size)
    (overlay/offset
      0
      (* 0.04 size)
      (circle (* 0.05 size) "solid" "black")
      (ellipse (* 0.14 size) (* 0.2 size) "solid" "mediumblue"))))

;;; (eye size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws one eye.
(define eye
  (lambda (size)
    (overlay/offset (- (* 0.07 size)) (- (* 0.35 size)) 
                    (eye-blue size) 
                    (black-eye-base size))))

;;; (face-1 size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws a face with one eye.
(define face-1
  (lambda (size)
    (overlay/offset (- (* 1.5 size)) (- (* 0.4 size)) 
                    (eye size) 
                    (head size))))

;;; (mouth size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws a mouth.
(define mouth
  (lambda (size)
    (overlay/align "middle" "bottom" 
                   (ellipse (* 0.3 size) (* 0.17 size) "solid" "red") 
                   (circle (* 0.2 size) "solid" "black"))))

;;; (mouth-and-blush size) -> drawing?
;;;   size: integer?, non-negative
;;; Align the mouth and blush.
(define mouth-and-blush
  (lambda (size)
    (above (blush size) (mouth size))))

;;; (face-2 size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the face with two eyes.
(define face-2
  (lambda (size)
    (overlay/offset (- (* 0.85 size)) (- (* 0.4 size)) 
                    (eye size) 
                    (face-1 size))))

;;; (face size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete face.
(define face
  (lambda (size)
    (overlay/offset (- (* 0.6 size)) (- size) 
                    (mouth-and-blush size) 
                    (face-2 size))))

;;; (feet size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the feet.
(define feet
  (lambda (size)
    (beside 
      (ellipse (* 0.7 size) (* 0.5 size) "solid" "red") 
      (circle (* 0.15 size) "solid" "white") 
      (ellipse (* 0.7 size) (* 0.5 size) "solid" "red"))))

;;; (basic-kirby size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete basic kirby.
(define basic-kirby
  (lambda (size)
    (overlay/align "middle" "bottom" 
                   (face size) 
                   (feet size))))

;;; (basic-kirby-offset size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the basic kirby but offset feet for animation.
(define basic-kirby-offset
  (lambda (size)
    (overlay/offset (* 0.46 size)
                    (* 1.6 size)
                    (face size) 
                    (feet size))))

; --------------------------------
;;; Kirby 1: Tennis Player Kirby |
; --------------------------------

;; Color Palette
(define bg-yellow (color 255 240 0 1))
  
;;; (racket-frame size) -> image?
;;;   size : number?, non-negative
;;; Draws the frame of the racket. 
(define racket-frame
  (lambda (size)
    (overlay
      (ellipse (* 0.8 size) size "solid" bg-yellow)
      (ellipse (* 0.86 size) (* 1.06 size) "solid" "blue"))))

;;; (racket-vertical-string size) -> image?
;;;   size : number?, non-negative
;;; Draws one vertical string of the racket, length = size.
(define racket-vertical-string
  (lambda (size)
    (rectangle 2 size "solid" "black")))

;;; (racket-horizontal-string size) -> image?
;;;   size : number?, non-negative
;;; Draws one horizontal string of the racket, length = size.
(define racket-horizontal-string
  (lambda (size)
    (rectangle (* 0.8 size) 2 "solid" "black")))

;;; (racket-frame-with-strings-1 size) -> image?
;;;   size : number?, non-negative
;;; Draws the half the frame of the racket with vertical strings.
(define racket-frame-with-strings-1
  (lambda (size)
    (overlay/offset (- (* 0.14 size))
                    (- (* 0.16 size))
                    (racket-vertical-string (* 0.73 size))
                    (overlay/offset (- (* 0.28 size))
                                    (- (* 0.06 size))
                                    (racket-vertical-string (* 0.94 size))
                                    (overlay (racket-vertical-string size) 
                                             (racket-frame size))))))

;;; (racket-frame-with-strings-2 size) -> image?
;;;   size : number?, non-negative
;;; Draws the full frame of the racket with vertical strings.
(define racket-frame-with-strings-2
  (lambda (size)
    (overlay/offset (- (* 0.7 size))
                    (- (* 0.16 size))
                    (racket-vertical-string (* 0.73 size))
                    (overlay/offset (- (* 0.56 size))
                                    (- (* 0.06 size))
                                    (racket-vertical-string (* 0.94 size))
                                    (racket-frame-with-strings-1 size)))))

;;; (racket-frame-with-strings-3 size) -> image?
;;;   size : number?, non-negative
;;; Draws the frame of the racket with full vertical strings and half horizontal strings.
(define racket-frame-with-strings-3
  (lambda (size)
    (overlay/offset (- (* 0.14 size))
                    (- (* 0.16 size))
                    (racket-horizontal-string (* 0.72 size))
                    (overlay/offset (- (* 0.05 size))
                                    (- (* 0.33 size))
                                    (racket-horizontal-string (* 0.95 size))
                                    (overlay (racket-horizontal-string size) 
                                             (racket-frame-with-strings-2 size))))))

;;; (racket-frame-with-strings size) -> image?
;;;   size : number?, non-negative
;;; Draws the frame of the racket with full vertical strings and full horizontal strings.
(define racket-frame-with-strings
  (lambda (size)
    (overlay/offset (- (* 0.14 size))
                    (- (* 0.88 size))
                    (racket-horizontal-string (* 0.72 size))
                    (overlay/offset (- (* 0.05 size))
                                    (- (* 0.7 size))
                                    (racket-horizontal-string (* 0.95 size))
                                    (racket-frame-with-strings-3 size)))))

;;; (racket-connect size) -> image?
;;;   size : number?, non-negative
;;; Draws the connecting part of the racket. 
(define racket-connect
  (lambda (size)
    (rotate 180 
      (overlay
        (triangle (* 0.59 size) "solid" bg-yellow)
        (triangle (* 0.73 size) "solid" "black")))))

;;; (racket-connected size) -> image?
;;;   size : number?, non-negative
;;; Draws the connecting part of the racket combined with the racket frame. 
(define racket-connected
  (lambda (size)
    (overlay/offset (* 0.065 size) (* 0.8 size) 
                    (racket-frame-with-strings size) 
                    (racket-connect size))))

;;; (racket-handle size) -> image?
;;;   size : number?, non-negative
;;; Draws the handle of the racket. 
(define racket-handle
  (lambda (size)
    (rectangle (* 0.2 size) (* 0.6 size) "solid" "black")))

;;; (racket size) -> image?
;;;   size : number?, non-negative
;;; Draws the full racket. 
(define racket
  (lambda (size)
    (overlay/offset (- (* 0.33 size)) (- (* 1.24 size)) 
                    (racket-handle size) 
                    (racket-connected size))))

;;; (headband size) -> image?
;;;   size : number?, non-negative
;;; Draws a headband with a stripe.
(define headband
  (lambda (size)
    (overlay (rectangle (* 1.6 size) (* 0.08 size) "solid" "white")
             (rectangle (* 1.6 size) (* 0.2 size) "solid" "black"))))

;;; (kirby-with-headband size) -> image?
;;;   size : number?, non-negative
;;; Draws kirby with the headband.
(define kirby-with-headband
  (lambda (size)
    (overlay/offset (- (* 0.5 size))
                    (- (* 0.2 size))
                    (headband size)
                    (basic-kirby size))))

;;; (kirby-with-racket size) -> image?
;;;   size : number?, non-negative
;;; Draws kirby with the tennis racket.
(define kirby-with-racket
  (lambda (size)
    (overlay/offset (- (* 0.28 size)) (- (* 0.55 size)) 
                    (kirby-with-headband size) 
                    (racket size))))

;;; (tennis-kirby-ball-down size) -> image?
;;;   size : number?, non-negative
;;; Draws tennis kirby with the ball on the ground. 
(define tennis-kirby-ball-down
  (lambda (size)
    (overlay/offset (* 2.6 size) (* 2.2 size) 
                    (kirby-with-racket size) 
                    (tennis-ball (* 0.18 size)))))

;;; (tennis-kirby-ball-down size) -> image?
;;;   size : number?, non-negative
;;; Draws tennis kirby with the ball in his hand.
(define tennis-kirby-ball-up
  (lambda (size)
    (overlay/offset (* 2.6 size) (* 1.6 size) 
                    (kirby-with-racket size) 
                    (tennis-ball (* 0.18 size)))))
        
; ------------------------
;;; Kirby 2: Mario Kirby |
; ------------------------

;; Color Palette
(define mushroom-base-color (color 255 227 184 1))
(define mustache-brown (color 65 38 37 1))

;; MUSHROOM

;;; (mushroom-base length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws a base body for the mushroom.
(define mushroom-base
  (lambda (length)
    (ellipse length (/ length 1.5) "solid" mushroom-base-color)))

;;; (mushroom-eye length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws one eye for the mushroom body.
(define mushroom-eye
  (lambda (length)
    (overlay/offset (- (/ length 66.667)) (- (/ length 66.667))
                    (ellipse (* length 0.04) (* length 0.08) "solid" "white")
                    (ellipse (* length 0.07) (* length 0.17) "solid" "black"))))

;;; (mushroom-eyes length) -> drawing?
;;;   length: integer?, non-negative
;;; Aligns to make two eyes.
(define mushroom-eyes
  (lambda (length)
    (beside (mushroom-eye length) 
            (square (/ length 6) "solid" "transparent") 
            (mushroom-eye length))))

;;; (mushroom-body length) -> drawing?
;;;   length: integer?, non-negative
;;; Puts the eyes on top of the mushroom body.
(define mushroom-body
  (lambda (length)
    (overlay/offset (- (/ length 2.98507463)) (- (/ length 2.85714286)) 
                    (mushroom-eyes length) 
                    (mushroom-base length))))

;;; (mushroom-head length color) -> drawing?
;;;   length: integer?, non-negative
;;;   color: string?
;;; Draws the hat/head part of the mushroom.
(define mushroom-head
  (lambda (length color)
    (let ([white-ellipse (ellipse (/ length 4) (/ length 2.2222) "solid" "white")]
          [red-head (ellipse (* length 1.3) (* length 1.02) "solid" color)]
          [white-circle (circle (* length 0.25) "solid" "white")])
    (overlay/align "right" "center"
                   white-ellipse
                   (overlay/align "left" "center"
                                  white-ellipse
                                  (overlay white-circle 
                                           red-head))))))

;;; (mushroom length color) -> drawing?
;;;   length: integer?, non-negative
;;;   color: string?
;;; Draws the complete image of the mushroom.
(define mushroom
  (lambda (length color)
    (overlay/offset (/ length 6.667) (/ length 1.58730159) 
                    (mushroom-head length color) 
                    (mushroom-body length))))

;; MARIO HAT

;;; (house length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws an image of the part of the hat that looks like a house.
(define house
  (lambda (length)
    (path length
          length
          (list (pair (/ length 2) 0) ; 1
                (pair (* length (/ 7 8)) (* length 0.25)) ; 2
                (pair (* length (/ 7 8)) (/ length 2)) ; 3
                (pair (/ length 8) (/ length 2)) ; 4
                (pair (/ length 8) (* length 0.25)) ;5
          )
          "solid"
          "red")))

;;; (pre-cap length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws an image of the part of the hat that is cap.
(define pre-cap
  (lambda (length)
    (path length
          length
          (list (pair 0 (* length (/ 5 8))) ; 1
                (pair (/ length 8) (/ length 2)) ; 2
                (pair (* length (/ 7 8)) (/ length 2)) ; 3
                (pair length (* length (/ 5 8)))
          )
          "solid"
          darker-red)))

;;; (cap length) -> drawing?
;;;   length: integer?, non-negative
;;; Finishes the look for the cap
(define cap
  (lambda (length)
    (overlay/offset 0 (- (/ length 1.66667)) 
                    (ellipse length (/ length 20) "solid" darker-red)
                    (pre-cap length))))

;;; (mario-hat length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the who
(define mario-hat
  (lambda (length)
    (overlay/offset (- (/ length 2.73972603)) (- (/ length 5.71428571)) 
                    (mario-ball 27 "red" "white") 
                    (overlay (house length) (cap length)))))

;; MUSTACHE

;;; (mustache length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws mustache.
(define mustache
  (lambda (length)
    (let* ([dot (circle (* length 0.1) "solid" mustache-brown)]
           [align (lambda (img) (overlay/offset (/ length 8) 0 dot img))])
      (apply beside (map align (make-list 2 (align dot)))))))

;; COMPLETE MARIO KIRBY

;;; (mario-wears-hat size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws image of mario kirby with hat on.
(define mario-wears-hat
  (lambda (size)
    (overlay/offset (- (/ size 4)) (/ size 1.08695652) 
                    (mario-hat (* size 2)) 
                    (basic-kirby size))))

;;; (mario-kirby size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws image of mario kirby with hat and mustache.
(define mario-kirby
  (lambda (size)
    (overlay/offset (- (/ size 1.58730159)) (- (* size 2))
                    (mustache (* size 1.5)) 
                    (mario-wears-hat size))))

;;; (mario-kirby-1 size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete image of Mario Kirby.
(define mario-kirby-1
  (lambda (size)
    (overlay/offset (- (* size 3.1)) (- (/ size 1.66667))
                    (mushroom size "red")
                    (overlay/offset (/ size 1.11111111) (- (* size 1.8))
                                    (mushroom (/ size 1.11111111) "green")
                                    (mario-kirby size)))))

;;; (mario-kirby-2 size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete image of Mario Kirby.
(define mario-kirby-2
  (lambda (size)
    (overlay/offset (- (* size 3.1)) (- (* size 1.8))
                    (mushroom size "red")
                    (overlay/offset (/ size 1.11111111) (- (/ size 1.2))
                                    (mushroom (/ size 1.11111111) "green")
                                    (mario-kirby size)))))

;; Display to test
;(mario-kirby 120)

; -------------------------
;;; Kirby 3: Doctor Kirby |
; -------------------------

;;; (hat length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the hat of the doctor kirby.
(define hat
  (lambda (length)
    (overlay
      (overlay
        (overlay
          (rectangle (* length 0.75) (* length 0.25) "solid" "red")
          (rectangle (* length 0.25) (* length 0.75) "solid" "red"))
        (overlay
          (circle (* length 0.5) "solid" (color 249 246 238 1))
          (circle (* length 0.6) "solid" (color 237 234 222 1))))
      (rectangle (* length 2.0) (* length 0.8) "solid" (color 237 234 222 1)))))

;;; (glasses length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the glasses of doctor kirby.
(define glasses
  (lambda (length)
    (beside/align "bottom"
        (rectangle (* length 0.15) (* length 1.2) "solid" "blue")
    (beside
      (overlay
        (circle (* length 0.3) "solid" "transparent")
        (circle (* length 0.4) "outline" "blue"))
      (rectangle (* length 0.2) (* length 0.2) "solid" "blue") 
      (overlay
        (circle (* length 0.3) "solid" "transparent")
        (circle (* length 0.4) "outline" "blue")))
      (rectangle (* length 0.15) (* length 1.2) "solid" "blue"))))

;;; (syringe length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the syringe of doctor kirby.
(define syringe
  (lambda (length)
     (above
       (rectangle (* length 0.1) (* length 0.5) "outline" "black")
       (above
         (rectangle (* length 0.5) (* length 0.75) "solid" "black")
         (rectangle (* length 0.5) (* length 1.5) "solid" "gray"))
       (above
         (rectangle (* length 0.25) (* length 0.375) "solid" "black")
         (ellipse (* length 0.5) (* length 0.25) "solid" "black")))))

;;; (water-drops length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws the water drops of the syringe.
(define water-drops
  (lambda (length)
    (above
      (beside (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue")
              (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue")
              (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue"))
      (beside (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue") 
              (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue"))
      (ellipse (* length 0.25) (* length 0.75) "solid" "lightblue"))))

;;; (syringe-with-water-drops length) -> drawing?
;;;   length: integer?, non-negative
;;; Draws syringe with water drops.
(define syringe-with-water-drops
  (lambda (length)
    (above
      (water-drops (* length 0.2)) (syringe length))))

;;; (doctor-kirby-with-hat size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws doctor kirby with hat.
(define doctor-kirby-with-hat
  (lambda (size)
    (overlay/offset (-(* size 0.5)) (* size 0.5)
      (hat (* 0.8 size)) (basic-kirby-offset size))))

;;; (doctor-kirby-with-glasses size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws doctor kirby with glasses.
(define doctor-kirby-with-glasses
  (lambda (size)
    (overlay
      (glasses (* 0.75 size)) (doctor-kirby-with-hat size))))

;;; (doctor-kirby size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete doctor kirby.
(define doctor-kirby
  (lambda (size)
    (overlay/offset (- (* size 0.05)) (* size 0.5)
      (doctor-kirby-with-glasses size) (syringe (* size 0.5)))))

;;; (doctor-kirby-with-water-drops size) -> drawing?
;;;   size: integer?, non-negative
;;; Draws the complete doctor kirby wtih water drops.
(define doctor-kirby-with-water-drops
  (lambda (size)
    (overlay/offset (- (* size 0.05)) (* size 0.5)
      (doctor-kirby-with-glasses size) (syringe-with-water-drops (* size 0.5)))))

; Display to test
;(doctor-kirby 120)

; ------------------------
;;; Kirby 4: Santa Kirby |
; ------------------------

;;; (santa-mustache size) -> drawing?
;;;   size : integer? (non-negative)
;;; Returns a white mustache. 
(define santa-mustache
  (lambda (size)
                (path size
                      size
                      (list (pair (* size 1.15) (* size 0))      ; 1st point
                            (pair (* size 1.8) (* size 0.19))    ; 2nd point
                            (pair (* size 2.075) (* size 0.575)) ; 3rd point
                            (pair (* size 1.8) (* size 0.575))   ; 4th point
                            (pair (* size 1.96) (* size 0.96))   ; 5th point 
                            (pair (* size 1.73) (* size 0.925))  ; 6th point
                            (pair (* size 1.15) (* size 1.4))    ; 7th point
                            (pair (* size 0.575) (* size 0.925)) ; 8th pint
                            (pair (* size 0.345)  (* size 0.96)) ; 9 th pint
                            (pair (* size 0.5) (* size 0.575))   ; 10th pint
                            (pair (* size 0.23) (* size 0.575))  ; 11th pint
                            (pair (* size 0.5) (* size 0.19))    ; 12th point
                        )     
                      "solid"
                      "white" )))

;;; (nose size) -> drawing?
;;;  size : integer? (non-negative)
;;; Returns a dark red nose. 
(define nose
  (lambda (size)
    (ellipse (* size 0.45) (* size 0.25) "solid" "firebrick")))

;;; (mustache-with-nose size) -> drawing?
;;;  size : integer? (non-negative)
;;; Returns a combined nose and white mustache. 
(define mustache-with-nose
  (lambda (size)
     (overlay/offset (* -0.9 size) (* 0.1 size) 
                     (nose size) 
                     (santa-mustache size))))

;;; (mustache-with-nose size) -> drawing?
;;;  size : integer? (non-negative)
;;; Returns a combined nose, white mustache, and dark red nose.
(define mustache-with-nose-and-blush
  (lambda (size)
     (overlay/offset (* 0.4 size) (* 0 size) 
                     (mustache-with-nose size) 
                     (blush size))))

;;; (santa-face size) -> drawing?
;;;  size : integer? (non-negative)
;;; Returns a santa kirby face.
(define santa-face
  (lambda (size)
    (overlay/offset (- (* 0.18 size)) (- (* 1 size)) 
                    (mustache-with-nose-and-blush size) 
                    (face-2 size))))

;;; (basic-santa-kirby size) -> drawing?
;;;   size : integer? (non-negative)
;;; Returns a basic santa kirby with just head, arm, mustache with blush and nose, and feet.
(define basic-santa-kirby
  (lambda (size)
    (overlay/offset (* 0.47 size) (* 1.5 size) 
                    (santa-face size) 
                    (feet size))))

;;; (santa-hat-base size) -> drawing?
;;;   size : integer? (non-negative)
;;; Returns a base santa hat.
(define santa-hat-base
  (lambda (size)
    (overlay/offset (* 0.16 size) (* -1.1 size) 
      (overlay (ellipse (* 1.95 size) (* 0.45 size) "solid" "white")
                        (ellipse (* 2 size) (* 0.5 size) "solid" "goldenrod"))
       (overlay (triangle (* 1.55 size) "solid" "firebrick") 
                (triangle (* 1.7 size) "solid" "maroon")))))

;;; (santa-hat size) -> drawing?
;;;   size : integer? (non-negative)
;;; Returns a final santa hat with the white ball on top of santa-hat-base.
(define santa-hat
  (lambda (size)
    (overlay/offset (* -0.78 size) (* 0.23 size) 
                    (overlay (circle (* 0.2 size) "solid" "white")
                             (circle (* 0.22 size) "solid" "goldenrod"))
                    (santa-hat-base size))))

;;; (circle-1 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the outermost red circle.
(define circle-1 
  (lambda (size)
    (circle (* 0.5 size) "solid" "red")))

;;; (circle-2 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the 2nd outermost orange circle.
(define circle-2 
  (lambda (size)
    (circle (* 0.4 size) "solid" "orange")))

;;; (circle-3 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the 3rd outermost yellow circle.
(define circle-3 
  (lambda (size)
    (circle (* 0.3 size) "solid" "yellow")))

;;; (circle-4 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the 4th outermost green circle.
(define circle-4 
  (lambda (size)
    (circle (* 0.2 size) "solid" "green")))

;;; (circle-5 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the 5th outermost blue circle.
(define circle-5 
  (lambda (size)
    (circle (* 0.1 size) "solid" "blue")))

;;; (circle-6 size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns the innermost violet circle.
(define circle-6 
  (lambda (size)
    (circle (* 0.05 size) "solid" "violet")))

;;; (candy size) -> drawing?
;;;   size : integer? (non-negative)
;;; Returns a drawing with all the circles on top of each other.
(define candy 
  (lambda (size)
    (overlay (circle-6 size) 
             (circle-5 size) (circle-4 size) (circle-3 size) 
             (circle-2 size) (circle-1 size))))

;;; (stick size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns a brown stick drawing.
(define stick
  (lambda (size)
    (rectangle (* 0.1 size) (* 0.9 size) "solid" "brown")))

;;; (lollipop size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns a final drawing of lollipop.
(define lollipop
   (lambda (size)
     (above (candy size) (stick size))))

;;; (santa-kirby-with-lollipop size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns a drawing with kirby holding the lollipop
(define santa-kirby-with-lollipop
  (lambda (size)
      (overlay/offset (* -0.7 size) (* -0.4 size) 
                      (basic-santa-kirby size) 
                      (beside (square 15 "solid" "transparent") 
                              (rotate 345 (lollipop size))))))

;;; (santa-kirby size) -> drawing?
;;;   size : integer? (non-negative)
;;;; Returns a final Santa kirby drawing.
(define santa-kirby
  (lambda (size)
     (overlay/offset (* -0.97 size) (* 1.1 size) 
                     (santa-hat size) 
                     (santa-kirby-with-lollipop size))))

;;; (diamond length color) -> drawing?
;;;   length: integer? (non-negative)
;;;   color: string?
;;; Draws a rotated 45 degrees square to make a diamond. Adapted from 
;;; mini-project 5.
(define diamond
  (lambda (length color) 
    (path length                           ; horizontal image size
          length                           ; vertical image size
          (list (pair (/ length 2) 0)      ; top point
                (pair length (/ length 2)) ; far right point
                (pair (/ length 2) length) ; bottom point
                (pair 0 (/ length 2))      ; far left point
                (pair (/ length 2) 0))     ; top point (need to return)
          "outline"                        ; fill style
          color)))                         ; color

;;; (base-snowflake length color n) -> drawing?
;;;   length: integer? (non-negative)
;;;   color: string?
;;;   n: integer? (non-negative)
;;; Draws fractal kolam with the given visual properties. Adapted from mini-project 5.
(define base-snowflake
  (lambda (length color n)
    (match n
      [0 null]
      [1 (diamond length color)]
      [_ (let* ([make-diamond (base-snowflake (/ length 3) color (- n 1))]
                [space (square (/ length 3) "solid" "transparent")]
                [top-bot-row (beside space make-diamond space)]
                [mid-row (beside make-diamond make-diamond make-diamond)])
           (above top-bot-row 
                  mid-row 
                  top-bot-row))])))

;;; (snowflake size) -> drawing?
;;;  size: integer? (non-negative)
;;; Returns a complete snowflake image.
(define snowflake
  (lambda (size)
  (base-snowflake (* 0.80 size) "lightblue" 4)))

;;; (santa-kirby-snowflake-up size) -> drawing?
;;;  size: integer? (non-negative)
;;; Returns an image when snowflake is at the top. Just began falling.
(define santa-kirby-snowflake-up
  (lambda (size)
    (overlay/offset (* -2.85 size) (* -0.2 size) 
                    (snowflake size)
                    (santa-kirby size))))

;;; (santa-kirby-snowflake-middle size) -> drawing?
;;;  size: integer? (non-negative)
;;; Returns an image when snowflake is in the midst of falling. 
(define santa-kirby-snowflake-middle
  (lambda (size)
    (overlay/offset (* -2.85 size) (* -1.75 size) 
                    (snowflake size)
                    (santa-kirby size))))

;;; (santa-kirby-snowflake-down size) -> drawing?
;;;   size: integer?, non-negative
;;; Returns an image when snowflake falls down.
(define santa-kirby-snowflake-down
  (lambda (size)
    (overlay/offset (* -2.85 size) (* -3.5 size) 
                    (snowflake size)
                    (santa-kirby size))))
        
;; Display to test
;(santa-kirby 120)

; ---------------------
;;; Background canvas |
; ---------------------

;; Color Palette
(define bg-star (color 255 252 0 1))

;; Background: 1000 x 600 
(define background (rectangle 1000 600 "solid" bg-yellow))

;; Decoration items

;;; Make 5 rows of 10 dots
(define dots
  (let* ([dot (overlay (circle 5 "solid" bg-star) 
                       (square 20 "solid" "transparent"))]
         [1-row (apply beside (make-list 10 dot))])
    (apply above (make-list 5 1-row))))

;;; Basic stars
(define star-0 (star 40 bg-star bg-star))
(define star-1 (star 60 bg-star bg-star))
(define star-2 (star 80 bg-star bg-star))
(define star-3 (star 100 bg-star bg-star))
(define star-4 (star 150 bg-star bg-star))
(define star-5 (star 200 bg-star bg-star))

;;; Blocks of decoration
(define block-0 (square 10 "solid" "transparent"))
(define block-1 (overlay/offset -5 0 (rotate 40 star-0) (square 70 "solid" "transparent")))
(define block-2 (overlay/offset -8 -8 (rotate 10 star-0) (square 70 "solid" "transparent")))
(define block-3 (overlay/offset 0 0 (rotate -40 star-1) (square 90 "solid" "transparent")))
(define block-4 (overlay/offset -5 0 (rotate 60 star-2) (square 120 "solid" "transparent")))
(define block-5 (overlay/offset -10 -5 (rotate 20 star-2) (square 130 "solid" "transparent")))
(define block-6 (overlay/offset 0 2 (rotate 20 star-3) (square 130 "solid" "transparent")))
(define block-7 (overlay/offset 0 2 (rotate -20 star-3) (square 130 "solid" "transparent")))
(define block-8 (overlay/offset 0 10 (rotate 30 star-4) (square 200 "solid" "transparent")))
(define block-9 (overlay/offset 2 10 (rotate -30 star-4) (square 200 "solid" "transparent")))
(define block-10 (overlay/offset 9 20 (rotate 20 star-5) (square 240 "solid" "transparent")))
(define block-11 (overlay/offset 9 20 (rotate -20 star-5) (square 240 "solid" "transparent")))
(define block-12 (rotate 10 (overlay dots (rectangle 200 100 "solid" "transparent"))))
(define block-13 (rotate -40 (overlay dots (rectangle 200 100 "solid" "transparent"))))

;;; Rows of decoration
(define row-1 (beside block-13 block-3 block-0 block-4 block-2 block-0 block-9 block-2 block-8))
(define row-2 (beside block-1 block-5 block-3 block-9 block-4 block-3 block-2 block-6 block-0 block-3)) ; height: 200
(define row-3 (beside block-0 block-4 block-2 block-7 block-0 block-2 block-3 block-1 block-9 block-12)) ; height: 240
(define row-4 (beside block-8 block-6 block-1 block-0 dots block-0 block-0 block-9 block-1 block-3))

;;; Full background
(define bg
  (overlay/offset 0 -400
                  row-4
                  (overlay/offset 0 -250 
                                  row-3
                                  (overlay/offset 0 -125
                                                  row-2 
                                                  (overlay/offset 0 0 
                                                                  row-1 
                                                                  background)))))

; -------------------------------------
;;; Basic Background (The Full Image) |
; -------------------------------------

;; The 5 balls, placed above each other in the following order: basic, tennis, mario,
;; doctor, santa.
(define bg-balls
  (overlay/offset 0
                  110
                  (basic-ball 40)
                  (overlay/offset 0
                                  110
                                  (tennis-ball 40)
                                  (overlay/offset 0
                                                  110
                                                  (mario-ball 40 "white" "red")
                                                  (overlay/offset 0 
                                                                  110 
                                                                  (medical-ball 40)
                                                                  (christmas-ball 40))))))

        
;; The background for all kirbies.
(define basic-background
  (overlay/offset -700 -40 bg-balls bg))
        
; ----------------
;;; Sound Effect |
; ----------------

;; The sound effect for clicking on a ball, without mod percussion.
(define largest-sound
  (lambda (midi-note)
    (mod (dynamics 127) (note midi-note qn))))

;; The sound effect for clicking on a ball, with mod percussion.
(define boom-end-sound
  (mod percussion (largest-sound 39)))
        
; --------------------
;;; Ball Click Effect|
; --------------------
        
;; The vector for determining the state of the displayed kirby.
(define current-kirby
  (vector "title"))
        
;; When the user clicks on a ball, kirby transforms into the type of kirby described
;; by the ball, with a short sound effect. If the user clicks anywhere else, kirby
;; remains the same.
(ignore 
  (canvas-onclick canv
    (lambda (x y)
      (begin 
        (cond 
          [(and (and (<= 700 x) (<= x 780)) (and (<= 40 y) (<= y 120)))
           (begin
             (play-composition boom-end-sound)
             (vector-set! current-kirby 0 "basic"))]
          [(and (and (<= 700 x) (<= x 780)) (and (<= 150 y) (<= y 230)))
           (begin
             (play-composition boom-end-sound)
             (vector-set! current-kirby 0 "tennis"))]
          [(and (and (<= 700 x) (<= x 780)) (and (<= 260 y) (<= y 340)))
           (begin
             (play-composition boom-end-sound)
             (vector-set! current-kirby 0 "mario"))]
          [(and (and (<= 700 x) (<= x 780)) (and (<= 370 y) (<= y 450)))
           (begin
             (play-composition boom-end-sound)
             (vector-set! current-kirby 0 "doctor"))]
          [(and (and (<= 700 x) (<= x 780)) (and (<= 480 y) (<= y 560)))
           (begin
             (play-composition boom-end-sound)
             (vector-set! current-kirby 0 "santa"))]
          [else
           (vector-set! current-kirby 0 (vector-ref current-kirby 0))])))))
        
;; The beginning page: a page with text descriptions on how to play the game.
;; When user clicks on a ball, draws the animation for each kirby.
(ignore
  (animate-with
    (lambda (time)
      (begin
        (draw-drawing canv basic-background 0 0)
          (cond
            [(equal? (vector-ref current-kirby 0) "title")
             (begin
               (draw-text canv "Kirby Transformation!" 50 180 "solid" "hotpink" "60px comic sans ms") 0 0
               (draw-text canv "Click on balls to make Kirby appear!" 115 230 "solid" "hotpink" "30px comic sans ms") 0 0
               (draw-text canv "Instruction Manual" 250 300 "solid" "hotpink" "20px comic sans ms") 0 0
               (draw-text canv "·Use the mouse to interact with the game." 140 340 "solid" "hotpink" "20px Courier") 0 0
               (draw-text canv "·Click on a power ball to transform Kirby." 140 370 "solid" "hotpink" "20px Courier") 0 0
               (draw-text canv "·Click on the pink power ball to return." 140 400 "solid" "hotpink" "20px Courier") 0 0
               (draw-text canv "·Enjoy and have fun!" 140 430 "solid" "hotpink" "20px Courier") 0 0)]
            [(equal? (vector-ref current-kirby 0) "basic")
             (begin
               (draw-drawing canv basic-background 0 0)
               (draw-drawing canv
                             (if (odd? (round (* 60 time)))
                                 (basic-kirby 120)
                                 (basic-kirby-offset 120))
                             180
                             180))]
            [(equal? (vector-ref current-kirby 0) "tennis")
             (begin
               (draw-drawing canv basic-background 0 0)
               (draw-drawing canv
                             (if (odd? (round (* 60 time)))
                                 (tennis-kirby-ball-down 120)
                                 (tennis-kirby-ball-up 120))
                             147
                             114))]
            [(equal? (vector-ref current-kirby 0) "mario")
             (begin
               (draw-drawing canv basic-background 0 0)
               (draw-drawing canv 
                             (if (odd? (round (* 60 time)))
                                 (mario-kirby-1 120)
                                 (mario-kirby-2 120))
                             50
                             80))]
            [(equal? (vector-ref current-kirby 0) "doctor")
             (begin
               (draw-drawing canv basic-background 0 0)
               (draw-drawing canv
                             (let* ([roundedT (round (* 0.002 time))]
                                    [modT (modulo roundedT 2)])
                               (cond
                                 [(equal? 0 modT) (doctor-kirby 120)]
                                 [(equal? 1 modT) (doctor-kirby-with-water-drops 120)]))
                             180
                             120))]
            [(equal? (vector-ref current-kirby 0) "santa")
             (begin
               (draw-drawing canv basic-background 0 0)
               (draw-drawing canv
                             (let* ([roundedT (round (* 0.002 time))]
                                    [modT (modulo roundedT 3)])
                               (cond 
                                 [(equal? 0 modT) (santa-kirby-snowflake-up 120)]
                                 [(equal? 1 modT) (santa-kirby-snowflake-middle 120)]
                                 [(equal? 2 modT) (santa-kirby-snowflake-down 120)]))
                             100
                             60))])))))

;; The canvas.        
canv

; -----------
;;; The BGM |
; -----------

;;; The main part of the bgm.
(define main-bgm
  (seq (note 72 qn)
       (rest qn)
       (note 67 qn)
       (rest qn)
       (note 63 qn)
       (note 62 qn)
       (note 60 qn)
       (rest qn)
       (note 60 qn)
       (note 62 qn)
       (note 63 qn)
       (note 60 qn)
       (note 58 qn)
       (note 60 qn)
       (note 55 qn)
       (rest qn)
       (note 72 qn)
       (rest qn)
       (note 67 qn)
       (rest qn)
       (note 63 qn)
       (note 62 qn)
       (note 60 qn)
       (note 60 en)
       (note 62 en)
       (note 63 qn)
       (note 65 qn)
       (note 62 qn)
       (note 58 qn)
       (note 60 qn)
       (note 55 qn)
       (note 60 qn)
       (rest qn)))

;;; One part of the underlying bgm.
(define underlying-bgm-1
  (seq (note 57 en)
       (par (note 72 en)
            (note 69 en)
            (note 63 en))
       (note 52 en)
       (par (note 72 en)
            (note 69 en)
            (note 63 en))
       (note 57 en)
       (par (note 72 en)
            (note 69 en)
            (note 63 en))
       (note 52 en)
       (par (note 72 en)
            (note 69 en)
            (note 63 en))))

;;; Another part of the underlying bgm.
(define underlying-bgm-2
  (seq (note 62 en)
       (par (note 74 en)
            (note 69 en)
            (note 65 en))
       (note 57 en)
       (par (note 74 en)
            (note 69 en)
            (note 66 en))
       (note 62 en)
       (par (note 74 en)
            (note 69 en)
            (note 66 en))
       (note 57 en)
       (par (note 74 en)
            (note 69 en)
            (note 66 en))))

;;; Another part of the underlying bgm.
(define underlying-bgm-3
  (seq (note 52 en)
       (par (note 74 en)
            (note 69 en)
            (note 65 en))
       (note 59 en)
       (par (note 74 en)
            (note 69 en)
            (note 66 en))
       (note 63 en)
       (par (note 71 en)
            (note 67 en)
            (note 63 en))
       (note 56 en)
       (par (note 71 en)
            (note 67 en)
            (note 63 en))))

;;; The full underlying bgm.
(define underlying-bgm
  (seq (repeat 2 underlying-bgm-1)
       underlying-bgm-2
       (repeat 3 underlying-bgm-1)
       underlying-bgm-3
       underlying-bgm-1))

;;; The full bgm, the main and underlying bgm combined, repeated 20 times.
(description "Play Me!")
(repeat 20
  (mod (tempo qn 180) 
     (par (mod (dynamics 80) (mod (instrument 60) main-bgm))
          (mod (dynamics 10) (mod (instrument 57) underlying-bgm)))))
