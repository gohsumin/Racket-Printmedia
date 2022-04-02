#lang racket
(require loudhum)
(require 2htdp/image)

;;; ABOUT
;;; The purpose of this program was to produce an image that could be used for
;;; a screen-printing.
;;;
;;; USAGE
;;; Install loudhum by entering https://github.com/grinnell-cs/loudhum.git from
;;; File --> Install Package.
;;; Define the image to filter:
(define image (bitmap/file "./unfiltered_images/hwang_small.jpg"))
;;; Define the number of dots:
(define num-dots 20)
;;; Save, run, and all the run function from the console:
;;;  > run

(define width (image-width image))
(define height (image-height image))
(define colors-vec (list->vector (image->color-list image)))

(define average-pixels
  (lambda (pixels-list)
    (let ([avg (lambda (color1 color2)
                 (color (quotient (+ (color-red color1)
                                     (color-red color2)) 2)
                        (quotient (+ (color-green color1)
                                     (color-green color2)) 2)
                        (quotient (+ (color-blue color1)
                                     (color-blue color2)) 2)
                        (quotient (+ (color-alpha color1)
                                     (color-alpha color2)) 2)))])
      (reduce avg pixels-list))))

(define get-colors
  (lambda (indices)
    (map (section vector-ref colors-vec <>) indices)))

(define range-2
  (lambda (lb ub)
    (drop (range ub) lb)))

(define reduce-row
  (lambda (n row-num)
    (let* ([hor-group-size (quotient width n)]
           [start-point (* row-num width)]
           [range1 (range-2 (* width row-num)
                            (+ (* width row-num) n))]
           [grouped-pixels-2 (map (lambda (n)
                                    (range-2 (+ start-point
                                                (* n hor-group-size))
                                             (+ start-point
                                                (* (add1 n) hor-group-size))))
                                  (range n))]
           [#|
            '((0 1 2) (3 4 5) (6 7 8) (9 10 11) (12 13 14) (15 16 17) (18 19 20)
            (21 22 23) (24 25 26) (27 28 29) (30 31 32) (33 34 35) (36 37 38)
            (39 40 41) (42 43 44) (45 46 47) (48 49 50) (51 52 53) (54 55 56)
            (57 58 59) (60 61 62) (63 64 65) (66 67 68) (69 70 71) (72 73 74)
            (75 76 77) (78 79 80) (81 82 83) (84 85 86) (87 88 89) (90 91 92)
            (93 94 95) (96 97 98))
            |#
            grouped-pixels (map (lambda (num)
                                  (range-2 (* num hor-group-size)
                                           (+ hor-group-size
                                              (* num hor-group-size))))
                                range1)])
      (map (lambda (lst)
             (average-pixels
              (get-colors lst)))
           grouped-pixels-2))))

(define reduce-rows
  (lambda (n)
    (list->vector
     (map (section reduce-row n <>)
          (range height)))))

(define combine-rows
  (lambda (row1 row2)
    (let ([cast-list (lambda (val)
                       (if (list? val)
                           val
                           (print "not list\n")
                           ))])
      (map (lambda (color1 color2)
             (average-pixels (list color1 color2)))
           (cast-list row1) (cast-list row2)))))

(define n-dots-per-row
  (lambda (n color-list)
    (when (or (> n (quotient width 2)) (> n (quotient height 2)))
      (error (string-append
              "numbers of dots per row must be smaller than or equal to "
              (number->string (min (quotient width 2) (quotient height 2))))))
    (let* ([hor-group-size (quotient width n)] 
           [num-dots-vert (quotient height hor-group-size)] 
           [reduced-rows-vec (reduce-rows n)]
           [grouped-rows (map
                          (lambda (num)
                            (range-2 (* num hor-group-size)
                                     (+ hor-group-size (* num hor-group-size))))
                          (range num-dots-vert))]
           [combine-row-group (lambda (row-group)
                                (reduce
                                 combine-rows
                                 (map (section vector-ref reduced-rows-vec <>)
                                      row-group)))]
           [draw-row (lambda (row)
                       (reduce beside
                               (map (lambda (ref-color)
                                      (overlay
                                       (circle 5 'solid
                                               (filter ref-color color-list))
                                       (circle 5.2 'solid (color 0 0 0 0))))
                                    row)))])
      (reduce above
              (map
               (section draw-row <>)
               (map
                (section combine-row-group <>)
                grouped-rows))))))

;;; Procedure
;;;  • filter
;;; Parameters
;;;  • ref-color
;;;    - A color struct from the 2htdp/image class
;;;    - The color to be matched to one of the colors in the given list
;;;  • color-list
;;;    - A list of colors
;;;    - (not (empty? color-list))
;;;    - The output of this procedure must be one of these values
;;; Purpose
;;;  • Return the color from color-list that best matches ref-color
;;; Produces
;;;  • _result
;;;    - A color struct
;;;    - (number? (index-of color-list _result))
(define filter
  (lambda (ref-color color-list)
    (let ([colors color-list])
      (let kernel ([smallest-difference-so-far (* 3 255)]
                   [closest-so-far (car colors)] ;ref-color]
                   [remaining colors])
        (cond [(null? remaining)
               closest-so-far]
              [(= (color-alpha ref-color) 0)
               closest-so-far]
              [else
               (let* ([current (car remaining)]
                      [red-diff (abs (- (color-red ref-color)
                                        (color-red current)))]
                      [green-diff (abs (- (color-green ref-color)
                                          (color-green current)))]
                      [blue-diff (abs (- (color-blue ref-color)
                                         (color-blue current)))]
                      [diff-sum (+ red-diff green-diff blue-diff)])
                 (if (< diff-sum smallest-difference-so-far)
                     (kernel diff-sum
                             current
                             (cdr remaining))
                     (kernel smallest-difference-so-far
                             closest-so-far
                             (cdr remaining))))])))))

;;; Procedure
;;;  • color-test
;;; Parameter
;;;  • color-list
;;;    - A list of colors
;;;    - (not (empty? color-list))
;;; Purpose
;;;  • Visualize the given colors
(define color-test
  (lambda (color-list)
    (reduce (section above/align 'left <> <>)
            (map (lambda (ref-color)
                   (let ([red-text (string-append
                                    "R: "
                                    (number->string (color-red ref-color)))]
                         [green-text (string-append
                                      "G: "
                                      (number->string (color-green ref-color)))]
                         [blue-text (string-append
                                     "B: "
                                     (number->string (color-blue ref-color)))])
                     (overlay/align
                      "left" "center"
                      (beside (circle 10 'solid ref-color)
                              (text (string-append "  "
                                                   red-text "   "
                                                   green-text "   "
                                                   blue-text)
                                    15 'black))
                      (rectangle 200 30 "solid" "white"))))
                 color-list))))

;;; COLOR SCHEMES
;;; Each scheme consists of triadic colors, sourced from:
;;; https://www.crispedge.com/generate-triadic-color/

; calming silver lavender | warm neutral | diorite
(define l1 (list (color 177 156 192)
                 (color 192 177 156)
                 (color 156 192 177)))

; dull magenta | victorian garden | canyon iris
(define l2 (list (color 141 74 82)
                 (color 82 141 74)
                 (color 74 82 141)))

(define color-feed l1)

(define darken-color
  (lambda (colr amt)
    (make-color
     (- (color-red colr) amt)
     (- (color-green colr) amt)
     (- (color-blue colr) amt))))

(define brighten-color
  (lambda (colr amt)
    (make-color
     (+ (color-red colr) amt)
     (+ (color-green colr) amt)
     (+ (color-blue colr) amt))))

(define expand-color
  (lambda (colr max-sub max-add)
    (let ([sub-size 20]
          [add-size 20])
      (let kernel ([so-far (list colr)]
                   [proc brighten-color]
                   [delta add-size]
                   [remaining max-add])
        (cond [(and (equal? proc darken-color) (< remaining sub-size))
               so-far]
              [(and (equal? proc brighten-color) (< remaining add-size))
               (kernel so-far darken-color sub-size max-sub)]
              [else
               (kernel (cons (proc colr remaining) so-far)
                       proc
                       delta
                       (- remaining delta))])))))

(define max-sub
  (lambda (colr)
    (min (color-red colr) (color-green colr) (color-blue colr))))

(define max-add
  (lambda (colr)
    (min (- 255 (color-red colr))
         (- 255 (color-green colr))
         (- 255 (color-blue colr)))))

(define expand-colors
  (lambda (lst)
    (let ([max-sub (min 150 (reduce min (map max-sub lst)))]
          [max-add (min 150 (reduce min (map max-add lst)))])
      (reduce append
              (map (lambda (colr) (expand-color colr max-sub max-add))
                   lst)))))
                      
(define color-list (expand-colors color-feed))

(define run (n-dots-per-row num-dots color-list))

