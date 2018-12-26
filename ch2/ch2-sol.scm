(load "../ch1/ch1.scm")

;; Exercise 2.1

(define (make-rat n d)
  (let ((abs-n (abs n))
	(abs-d (abs d))
	(sign (if (boolean=? (>= n 0) (> d 0)) 1 -1)))
    (let ((g (gcd abs-n abs-d)))
      (cons (* sign (/ abs-n g)) (/ abs-d g)))))

;; Exercise 2.2

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p)))

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
		       (x-point (end-segment s)))
	      (average (y-point (start-segment s))
		       (y-point (end-segment s)))))

;; Exercise 2.3

(define make-rectangle cons)
(define lower-left-rectangle car)
(define upper-right-rectangle cdr)

(define (perimeter r)
  (let ((lower-left (lower-left-rectangle r))
	(upper-right (upper-right-rectangle r)))
    (* 2 (+ (abs (- (x-point lower-left)
		    (x-point upper-right)))
	    (abs (- (y-point lower-left)
		    (y-point upper-right)))))))
(define (area r)
  (let ((lower-left (lower-left-rectangle r))
	(upper-right (upper-right-rectangle r)))
    (* (abs (- (x-point lower-left)
	       (x-point upper-right)))
       (abs (- (y-point lower-left)
	       (y-point upper-right))))))

(define (make-rectangle lower-left width height)
  (cons lower-left (cons width height)))
(define lower-left-rectangle car)
(define (upper-right-rectangle r)
  (let ((lower-left (lower-left-rectangle r))
	(width (car (cdr r)))
	(height (cdr (cdr r))))
    (make-point (+ (x-point lower-left) width)
		(+ (y-point lower-left) height))))

;; Exercise 2.4

;; Using the substitution model, we have
;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x
;; Thus, (car (cons x y)) yields x, as desired.

(define (my-cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5

(define (my-cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (my-car x)
  (if (= (remainder x 2) 0)
      (+ 1 (car (/ x 2)))
      0))

(define (my-cdr x)
  (if (= (remainder x 3) 0)
      (+ 1 (cdr (/ x 3)))
      0))

;; Exercise 2.6

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; Exercise 2.7

(define lower-bound car)
(define upper-bound cdr)

;; Exercise 2.8

;; To subtract an interval from another, Alyssa adds the first by the negative of the second. Note that bounds of the negative of an interval are the negative of the upper bound and the negative of the lower bound, in that order.

(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y))
		  (- (lower-bound y)))))

;; Exercise 2.9

;; Note that the sum of two intervals x and y is the interval defined by
;; (make-interval (+ (lower-bound x) (lower-bound y))
;;                (+ (upper-bound x) (upper-bound y)))
;; Thus, the width of the sum of x and y is
;; (/ (- (+ (upper-bound x) (upper-bound y))
;;       (+ (lower-bound x) (lower-bound y)))
;;    2)
;; The above expression is equivalent to the following
;; (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;;    (/ (- (upper-bound y) (lower-bound y)) 2))
;; Note that (/ (- (upper-bound x) (lower-bound x)) 2) is the width of x, and that (/ (- (upper-bound y) (lower-bound y)) 2) is the width of y. Hence, the width of the sum of two intervals is the sum of the width of x and the width of y. This shows that the width of the sum of two intervals is a function only of the widths of the intervals being added.

;; Note that the width of the multiplication of two intervals is not a function only of the widths of the intervals being multiplied, and that the same is true for division. We will give two examples as follows.

;; For instance, the multiplication of two intervals (make-interval 1 3) and (make-interval 3 5) each of width 1 is (make-interval 3 15), which has width 6, while the multiplication of two intervals (make-interval 1 3) and (make-interval 5 7) each of width 1 is (make-interval 5 21), which has width 8.

;; As another example, the division of two intervals (make-interval 6 12) and (make-interval 1 3), of widths 3 and 1 respectively, is (make-interval 2 12), which has width 5, while the division of two intervals (make-interval 6 12) and (make-interval 4 6), of widths 3 and 1 respectively, is (make-interval 1 3), which has width 1.

;; Exercise 2.10

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
	   (>= (upper-bound y) 0))
      (error 'div-interval "Divide an interval that spans zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))

;; Exercise 2.11

(define (mul-interval x y)
  (define (greater-than-zero? x)
    (>= (lower-bound x) 0))
  (define (less-than-zero? x)
    (<= (upper-bound x) 0))
  (define (span-zero? x)
    (and (<= (lower-bound x) 0)
	 (>= (upper-bound x) 0)))
  (cond ((and (greater-than-zero? x) (greater-than-zero? y))
	 (make-interval (* (lower-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (greater-than-zero? x) (less-than-zero? y))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (upper-bound y))))
	((and (greater-than-zero? x) (span-zero? y))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (less-than-zero? x) (greater-than-zero? y))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (lower-bound y))))
	((and (less-than-zero? x) (less-than-zero? y))
	 (make-interval (* (upper-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (less-than-zero? x) (span-zero? y))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (span-zero? x) (greater-than-zero? y))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (span-zero? x) (less-than-zero? y))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (lower-bound y))))
	(else (make-interval (min (* (lower-bound x) (upper-bound y))
				  (* (upper-bound x) (lower-bound y)))
			     (max (* (upper-bound x) (upper-bound y))
				  (* (lower-bound x) (lower-bound y)))))))

;; Exercise 2.12

(define (make-center-percent center percent)
  (make-center-width center (* percent center)))

(define (percent i)
  (/ (width i) (center i)))

;; Exxercise 2.13

;; Suppose x is an interval whose center is c and whose percentage tolerance is p. Suppose y is an interval whose center is c' and whose percentage tolerance is p'. Suppose furthermore that the endpoints of x and of y are all positive. From exercise 2.11, the product of x and y is an interval whose lower bound is
;; (c - cp)(c' - c'p') = cc' - cc'p - cc'p' + cc'pp'
;; and whose upper bound is
;; (c + cp)(c' + c'p') = cc' + cc'p + cc'p' + cc'pp'
;; Thus, the width of the product of x and y is
;; ((cc' + cc'p + cc'p' + cc'pp') - (cc' - cc'p - cc'p' + cc'pp')) / 2 = cc'p + cc'p',
;; and the center of the product of x and y is
;; ((cc' + cc'p + cc'p' + cc'pp') + (cc' - cc'p - cc'p' + cc'pp')) / 2 = cc' + cc'pp'.
;; Therefore, the percentage tolerance of the product of x and y is
;; (cc'p + cc'p') / (cc' + cc'pp') = (p + p') / (1 + pp').
;; This shows that the percentage tolerance of the product of two intervals equals (p + p') / (1 + pp'), where p and p' are the percentage tolerance of the two intervals being multiplied, respectively.

;; Exercise 2.14

;; Let A be the interval [0.9, 1.1]. Let B be the interval [1.9, 2.1]. To compute A / A, we first compute 1 / A, which is [1 / 1.1, 1 / 0.9]. Thus, A / A = A * (1 / A) yields [0.9 / 1.1, 1.1 / 0.9] = [9 / 11, 11 / 9] = 1.02 +- 19.8%. To compute A / B, we first compute 1 / B, which is [1 / 2.1, 1 / 1.9]. Thus, A / B = A * (1 / B) yields [0.9 / 2.1, 1.1 / 1.9] = [3 / 7, 11 / 19] = 0.504 +- 14.9%.

;; Notice, however, that A is a single variable, so the value of A / A *should* always be 1. The reason why A / A does not produce 1 is that it regards the A in the numerator and the A in the denominator as two different variables. This explains why Lem got different answers for the two ways of computing. Note that par1 uses each of R1 and R2 twice in the expression, so the value computed by par1 regard two R1 and two R2 as different variables, respectively. In constrast, par2 only uses each of R1 and R2 once in the expression, so the value computed by par2 does not have the issue mentioned above.

;; Exercise 2.15

;; Eva is correct. If there are variables that represent uncertain numbers being repeated, then Alyssa's system will regard the repeated variables as difference uncertain variables, although they are actually the same variable. That is, Alyssa's system introduces more uncertainties, and therefore it produces looser error bounds for repeated variables.

;; Exercise 2.16

;; As noted in Exercises 2.14 and 2.15, repeated variables in an algebraic expression introduce more uncertainties to the expression. Thus, if the number of each variable in two equivalent algebraic expressions is different, then these two equivalent algebraic expressions may lead to different answers.

;; The task to devise an interval-arithmetic package that does not have this shortcoming is impossible. Note that either in the applicative-order evaluation or in the normal-order evaluation, the variables will be evaluated to its value at some point. Thus, it is impossible for a system to identify if two uncertain intervals correspond to the same variable or not. This concludes that this task is impossible.
