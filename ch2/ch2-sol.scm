(load "../ch1/ch1.scm")
(load "ch2.scm")

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

;; Exercise 2.17

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; Exercise 2.18

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
	result
	(reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items nil))

;; Exercise 2.19

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; The order of the list coin-values does not affect the answer produced by cc. Note that with the modified version of cc, every element in the list coin-values will be considered once and only once. Also note that the recurrence relation used in cc does not depend on the order of the list coin-values. Therefore, we will consider all combinations of coins that add up to the value of amount. This shows that the order of the list coin-values does not affect the answer produced by cc.

;; Exercise 2.20

(define (same-parity first . rest)
  (define (recurse rest)
    (cond ((null? rest) nil)
	  ((= (remainder first 2)
	      (remainder (car rest) 2))
	   (cons (car rest) (recurse (cdr rest))))
	  (else (recurse (cdr rest)))))
  (cons first (recurse rest)))

;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22

;; The reason why the first square-list produces the reverse order is that the square of the first element of the list items is first cons'ed to the list answer, the square of the second is then cons'ed to the list answer, etc. Repeating this process, the square of the last element is last cons'ed to the list answer, which implies that it is the first element of answer. Hence, the first square-list produces the reverse order.

;; Note that using cons to append an element to the beginning of a list requires the first argument to be an element, and the second argument to be a list. However, in the second square-list, the first argument of cons is answer, which is a list, and the second argument is (square (car things)), which is an element. Hence, the second square-list does not work.

;; Exercise 2.23

(define (for-each proc items)
  (if (not (null? items))
      (begin (proc (car items))
	     (for-each proc (cdr items)))))

;; Exercise 2.24

;; The result printed by the interpreter is (1 (2 (3 4))).

;; The corresponding box-and-pointer structured is given as follows.
;; .. -> ./
;; |     |
;; v     v
;; 1     .. -> ./
;;       |     |
;;       v     v
;;       2     .. -> ./
;;             |     |
;;             v     v
;;             3     4

;; The interpretation of this as a tree is given as follows.
;;                     .
;;                    / \
;;                   1   .
;;                      / \
;;                     2   .
;;                        / \
;;                       3   4

;; Exercise 2.25

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; Exercise 2.26

;; (1 2 3 4 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))

;; Exercise 2.27

(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
	  ((not (pair? items)) items)
	  (else (iter (cdr items)
		      (cons (deep-reverse (car items)) result)))))
  (iter items nil))

;; Exercise 2.28

(define (fringe tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

;; Exercise 2.29

;; a.

(define left-branch-mobile car)
(define right-branch-mobile cadr)

(define branch-length car)
(define branch-structure cadr)

;; b.

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch-mobile mobile))
     (branch-weight (right-branch-mobile mobile))))

;; c.

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))

(define (balanced? mobile)
  (and (= (branch-torque (left-branch-mobile mobile))
	  (branch-torque (right-branch-mobile mobile)))
       (branch-balanced? (left-branch-mobile mobile))
       (branch-balanced? (right-branch-mobile mobile))))

;; d.

;; Only the selectors of mobiles and of branches need to be changed as follows. In addition, if the predicate list? is used in the procedures total-weight and balanced?, then it needs to be changed to pair? (which is not the case here).

(define left-branch-mobile car)
(define right-branch-mobile cdr)

(define branch-length car)
(define branch-structure cdr)

;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

;; Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (cons (car s) subset))
			  rest)))))

;; If the set s is empty, then the only subset of s is the empty set, so the procedure subsets directly returns (list nil) (note that this is a list that contains one element, which is the empty list). Otherwise, subsets of s are divided into two cases: subsets that do not contain (car s), and those that contain (car s). Subsets that do not contain (car s) can simply be obtained by calling subsets on (cdr s). Further, adding (car s) to each subset of s that does not contain (car s) forms subsets of s that do contain (car s). Hence, the set of all subsets of s is the combination of the two parts.

;; Exercise 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* higher-terms x)))
	      0
	      coefficient-sequence))

;; Exercise 2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37

(define (matrix-*-vector m v)
  (map (lambda (row)
	 (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col)
		  (dot-product row col))
		cols))
	 m)))

;; Exercise 2.38

;; The value of (fold-right / 1 (list 1 2 3)) is 3 / 2.
;; The value of (fold-left / 1 (list 1 2 3)) is 1 / 6.
;; The value of (fold-right list nil (list 1 2 3)) is (1 (2 (3 nil))).
;; The value of (fold-left list nil (list 1 2 3)) is (((nil 1) 2) 3).

;; If op satisfies the commutative law (i.e., (op a b) and (op b a) produces the same value), then fold-right and fold-left will produce the same values for any sequence.

;; Exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map (lambda (pair)
	 (list (car pair)
	       (cadr pair)
	       (+ (car pair) (cadr pair))))
       (filter (lambda (pair)
		 (prime? (+ (car pair) (cadr pair))))
	       (unique-pairs n))))

;; Exercise 2.41

(define (triples-sum-to s)
  (filter (lambda (triple)
	    (= (+ (car triple) (cadr triple) (caddr triple)) s))
	  (flatmap (lambda (i)
		     (flatmap (lambda (j)
				(map (lambda (k)
					   (list i j k))
					 (enumerate-interval 1 s)))
			      (enumerate-interval 1 s)))
		   (enumerate-interval 1 s))))

;; Exercise 2.42

(define make-position cons)
(define position-row car)
(define position-col cdr)

(define (position-equal? pos1 pos2)
  (and (= (position-row pos1)
	  (position-row pos2))
       (= (position-col pos1)
	  (position-col pos2))))

(define empty-board nil)

(define (adjoin-position row col rest)
  (cons (make-position row col) rest))

(define (safe? k positions)
  (let ((k-position (car (filter (lambda (position)
				   (= k (position-col position)))
				 positions))))
    (andmap (lambda (position)
	      (or (position-equal? k-position position)
		  (and (not (= (position-row k-position)
			       (position-row position)))
		       (not (= (position-col k-position)
			       (position-col position)))
		       (not (= (- (position-col k-position)
				  (position-row k-position))
			       (- (position-col position)
				  (position-row position))))
		       (not (= (+ (position-col k-position)
				  (position-row k-position))
			       (+ (position-col position)
				  (position-row position)))))))
	    positions)))

;; Exercise 2.43

;; Note that in Exercise 2.42, mapping on (queen-cols (- k 1)) is the outer mapping, which implies that (queen-cols (- k 1)) is only called once in (queen-cols k). In contrast, mapping on (queen-cols (- k 1)) is the inner mapping in Louis' version, and the outer mapping is over a list of length board-size. Thus, (queen-cols (- k 1)) is called board-size times in (queen-cols k).

;; Let T(k) be the time required when (queen-cols k) is called for solving the eight-queens puzzle with the original version, and let T'(k) be the time required when (queen-cols k) is called with Louis' version. Let n denote the value of board-size. Then
;; T(k) = T(k - 1) + g(k - 1) * n
;; T'(k) = n * (T'(k - 1) + g(k - 1))
;; Note that g(k) << T(k). Then T'(k) / T(k) ~ n * T'(k - 1) / T(k - 1). Since T'(0) = T(0) = Theta(1), then T'(k) / T(k) ~ n^k. Let T' denote the time required for solving the eight-queens puzzle with Louis' version; i.e., T' = T'(n) and n = 8. Then T' / T = T'(n) / T(n) ~ n^n. This shows that T' ~ n^n * T = 8^8 * T.

;; Exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;; Exercise 2.45

(define (split op1 op2)
  (define (recurse painter n)
    (if (= n 0)
	painter
	(let ((smaller (recurse painter (- n 1))))
	  (op1 painter (op2 smaller smaller)))))
  resurse)

;; Exercise 2.46

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

;; Exercise 2.47

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;; Exercise 2.48

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;; Exercise 2.49

;; a.

(define (outline-painter)
  (segment->painter (list (make-segment (make-vect 0.0 0.0)
					(make-vect 0.0 1.0))
			  (make-segment (make-vect 0.0 1.0)
					(make-vect 1.0 1.0))
			  (make-segment (make-vect 1.0 1.0)
					(make-vect 1.0 0.0))
			  (make-segment (make-vect 1.0 0.0)
					(make-vect 0.0 0.0)))))

;; b.

(define (x-painter)
  (segment->painter (list (make-segment (make-vect 0.0 1.0)
					(make-vect 1.0 0.0))
			  (make-segment (make-vect 0.0 0.0)
					(make-vect 1.0 1.0)))))

;; c.

(define (diamond-painter)
  (segment->painter (list (make-segment (make-vect 0.0 0.5)
					(make-vect 0.5 1.0))
			  (make-segment (make-vect 0.5 1.0)
					(make-vect 1.0 0.5))
			  (make-segment (make-vect 1.0 0.5)
					(make-vect 0.5 0.0))
			  (make-segment (make-vect 0.5 0.0)
					(make-vect 0.0 0.5)))))

;; d.

(define (wave)
  (segment->painter (list (make-segment (make-vect 0.0 0.7)
					(make-vect 0.2 0.55))
			  (make-segment (make-vect 0.2 0.55)
					(make-vect 0.35 0.65))
			  (make-segment (make-vect 0.35 0.65)
					(make-vect 0.45 0.6))
			  (make-segment (make-vect 0.45 0.6)
					(make-vect 0.35 0.85))
			  (make-segment (make-vect 0.35 0.85)
					(make-vect 0.45 1.0))
			  (make-segment (make-vect 0.55 1.0)
					(make-vect 0.65 0.85))
			  (make-segment (make-vect 0.65 0.85)
					(make-vect 0.55 0.6))
			  (make-segment (make-vect 0.55 0.6)
					(make-vect 0.65 0.65))
			  (make-segment (make-vect 0.65 0.65)
					(make-vect 1.0 0.4))
			  (make-segment (make-vect 1.0 0.35)
					(make-vect 0.55 0.5))
			  (make-segment (make-vect 0.55 0.5)
					(make-vect 0.7 0.0))
			  (make-segment (make-vect 0.6 0.0)
					(make-vect 0.5 0.4))
			  (make-segment (make-vect 0.5 0.4)
					(make-vect 0.4 0.0))
			  (make-segment (make-vect 0.3 0.0)
					(make-vect 0.45 0.5))
			  (make-segment (make-vect 0.45 0.5)
					(make-vect 0.2 0.45))
			  (make-segment (make-vect 0.2 0.45)
					(make-vect 0.0 0.65)))))

;; Exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

;; Exercise 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter painter1
					  (make-vect 0.0 0.0)
					  (make-vect 1.0 0.0)
					  split-point))
	  (paint-above (transform-painter painter2
					  split-point
					  (make-vect 1.0 0.5)
					  (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-above frame)
	(paint-below frame)))))

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
		     (rotate90 painter1))))

;; Exercise 2.52

;; a.

(define (wave)
  (segment->painter (list (make-segment (make-vect 0.0 0.7)
					(make-vect 0.2 0.55))
			  (make-segment (make-vect 0.2 0.55)
					(make-vect 0.35 0.65))
			  (make-segment (make-vect 0.35 0.65)
					(make-vect 0.45 0.6))
			  (make-segment (make-vect 0.45 0.6)
					(make-vect 0.35 0.85))
			  (make-segment (make-vect 0.35 0.85)
					(make-vect 0.45 1.0))
			  (make-segment (make-vect 0.55 1.0)
					(make-vect 0.65 0.85))
			  (make-segment (make-vect 0.65 0.85)
					(make-vect 0.55 0.6))
			  (make-segment (make-vect 0.55 0.6)
					(make-vect 0.65 0.65))
			  (make-segment (make-vect 0.65 0.65)
					(make-vect 1.0 0.4))
			  (make-segment (make-vect 1.0 0.35)
					(make-vect 0.55 0.5))
			  (make-segment (make-vect 0.55 0.5)
					(make-vect 0.7 0.0))
			  (make-segment (make-vect 0.6 0.0)
					(make-vect 0.5 0.4))
			  (make-segment (make-vect 0.5 0.4)
					(make-vect 0.4 0.0))
			  (make-segment (make-vect 0.3 0.0)
					(make-vect 0.45 0.5))
			  (make-segment (make-vect 0.45 0.5)
					(make-vect 0.2 0.45))
			  (make-segment (make-vect 0.2 0.45)
					(make-vect 0.0 0.65))
			  (make-segment (make-vect 0.4 0.85)
					(make-vect 0.5 0.65))
			  (make-segment (make-vect 0.5 0.65)
					(make-vect 0.6 0.85)))))

;; b.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1)))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter up)
		(below right corner)))))

;; c.

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate 180
				  identity flip-horiz)))
    (combine4 (corner-split painter n))))

;; Exercise 2.53

;; (a b c)
;; ((george))
;; ((y1 y2))
;; (y1 y2)
;; #f
;; #f
;; (red shoes blue socks)

;; Exercise 2.54

(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))
      (eq? a b)))

;; Exercise 2.55

;; Note that in Scheme, 'abracadabra is equivalent to (quote abracadabra). Thus, ''abracadabra is equivalent to '(quote abracadabra); i.e., it quotes a list of symbols, namely 'quote and 'abracadabra. Therefore, the car of ''abracadabra is the car of '(quote abracadabra), which is 'quote.

;; Exercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
				  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
	((and (exponentiation? exp) (number? (exponent exp)))
	 (make-product (exponent exp)
		       (make-product (make-exponentiation (base exp)
							  (- (exponent exp) 1))
				     (deriv (base exp) var))))
	(else (error "unknown expression type: DERIV" exp))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent))
	 (expt base exponent))
	(else (list '** base exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;; Exercise 2.57

(define (make-sum first second . rest)
  (let ((terms (cons first (cons second rest))))
    (let ((numbers (filter number? terms))
	  (nonnumbers (filter (lambda (x) (not (number? x))) terms)))
      (if (>= (length numbers) 1)
	  (let ((sum (fold-right + 0 numbers)))
	    (cond ((null? nonnumbers) sum)
		  ((= sum 0)
		   (if (> (length nonnumbers) 1)
		       (cons '+ nonnumbers)
		       (car nonnumbers)))
		  (else (cons '+ (cons sum nonnumbers)))))
	  (cons '+ nonnumbers)))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      (apply make-sum (cddr x))))

(define (make-product first second . rest)
  (let ((terms (cons first (cons second rest))))
    (let ((numbers (filter number? terms))
	  (nonnumbers (filter (lambda (x) (not (number? x))) terms)))
      (if (>= (length numbers) 1)
	  (let ((product (fold-right * 1 numbers)))
	    (cond ((null? nonnumbers) product)
		  ((= product 0) 0)
		  ((= product 1)
		   (if (> (length nonnumbers) 1)
		       (cons '* nonnumbers)
		       (car nonnumbers)))
		  (else (cons '* (cons product nonnumbers)))))
	  (cons '* nonnumbers)))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier x) (cadr x))
(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      (apply make-product (cddr x))))

;; Exercise 2.58

;; a.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2))
	 (+ a1 a2))
	(else (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;; b.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2))
	 (+ a1 a2))
	(else (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x) (member '+ x) #t))
(define (addend s)
  (define (iter s terms)
    (if (eq? (car s) '+)
	(if (= (length terms) 1)
	    (car terms)
	    (reverse terms))
	(iter (cdr s) (cons (car s) terms))))
  (iter s nil))
(define (augend s)
  (let ((terms (cdr (member '+ s))))
    (if (= (length terms) 1)
	(car terms)
	terms)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (member '* x) (not (sum? x))))
(define (multiplier p)
  (define (iter p terms)
    (if (eq? (car p) '*)
	(if (= (length terms) 1)
	    (car terms)
	    (reverse terms))
	(iter (cdr p) (cons (car p) terms))))
  (iter p nil))
(define (multiplicand p)
  (let ((terms (cdr (member '* p))))
    (if (= (length terms) 1)
	(car terms)
	terms)))

;; Exercise 2.59

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (car set1)
		  (union-set (cdr set1) set2))))

;; Exercise 2.60

(define (my-element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))
(define (my-adjoin-set x set)
  (cons x set))
(define (my-union-set set1 set2)
  (append set1 set2))
(define (my-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; The number of steps required by element-of-set? grows as Theta(n), which is the same as element-of-set? for the non-duplicate representation.
;; The number of steps required by adjoin-set grows as Theta(1), which is asymptotically less than adjoin-set for the non-duplicate representation.
;; The number of steps required by union-set grows as Theta(n). Note that union-set for the non-duplicate representation does an element-of-set? check for each element of set1 (through adjoin-set), so the number of steps required by union-set for the non-duplicate representation grows as Theta(n^2). Thus, the number of steps required by union-set is asymptotically less than union-set for the non-duplicate representation.
;; Note that intersection-set does an element-of-set? check for each element of set1. Thus, the number of steps required by intersection-set grows as Theta(n^2), which is the same as intersection-set for the non-duplicate representation.

;; Exercise 2.61

(define (my-adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set)
		    (my-adjoin-set x (cdr set))))))

;; If we adjoin elements to sets of many different sizes, we can expect that sometimes we will be able to stop searching at a point near the beginning of the list and that other times we will still need to examine most of the list. On the overage, we should expect to have to examine half of the items in the set. Thus, the average number of steps required will be about n / 2.

;; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set (cdr set1)
					   (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((< x2 x1)
		       (cons x2 (union-set set1 (cdr set2)))))))))

;; Exercise 2.63

;; a.

;; The two procedures both produce the same result, namely (1 3 5 7 9 11), for every tree.

;; b.

;; The two procedures do not have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list. Note that tree->list-1 needs to append two lists generated by the left subtree and by the right subtree. Hence, if T1(n) denote the number of steps required by (tree->list-1 tree) where tree is a balanced binary tree containing n nodes, then
;; T1(n) = 2 * T1(n / 2) + Theta(n)
;; Thus, T1(n) = Theta(n log n).

;; In contrast, tree->list-2 recursively calls the procedure on the right subtree, cons'es the entry at the node to the result, and then recursively calls the procedure on the left subtree. Hence, if T2(n) denotes the number of steps required by (tree->list-2 tree) where tree is a balanced binary tree containing n nodes, then
;; T2(n) = 2 * T2(n / 2) + Theta(1)
;; Thus, T2(n) = Theta(n).

;; Exercise 2.64

;; a.

;; If n equals 0, then the constructed tree is an empty tree, and all elements of elts are not included in the constructed tree. Otherwise, partial-tree recurses to obtain a balanced binary tree left-tree containing the first floor(n / 2) elements of elts as well as unused elements. The entry at the root node is the first element from the unused elements. Then, partial-tree recurses to obtain a balanced binary tree right-tree containing the next n - floor(n / 2) - 1 elements from the usused elements. It turns out that the balanced binary tree converted from the first n elements of the original elts is the entry at the root node combined with left-tree and right-tree, and that the unused elements are the unused elements from the second recursive call.

;;          7
;;        /   \
;;       3     11
;;      / \    /
;;     1   5  9

;; b.

;; Let T(n) denote the number of steps required by (partial-tree elts n) for some list elts of size at least n. From part a, we know that
;; T(n) = 2 * T(n / 2) + Theta(1)
;; Thus, T(n) = Theta(n).

;; Exercise 2.65

(define (union-set set1 set2)
  (define (merge-list list1 list2)
    (cond ((null? list1) list2)
	  ((null? list2) list1)
	  (else (let ((x1 (car list1))
		      (x2 (car list2)))
		  (cond ((= x1 x2) (cons x1 (merge-list (cdr list1)
							(cdr list2))))
			((< x1 x2) (cons x1 (merge-list (cdr list1) list2)))
			((< x2 x1) (cons x2 (merge-list list1 (cdr list2)))))))))
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (merge-list list1 list2))))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
	'()
	(let ((x1 (car list1))
	      (x2 (car list2)))
	  (cond ((= x1 x2) (cons x1 (intersection-list (cdr list1)
						       (cdr list2))))
		((< x1 x2) (intersection-list (cdr list1) list2))
		((< x2 x1) (intersection-list list1 (cdr list2)))))))
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (intersection-list list1 list2))))

;; Exercise 2.66

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry-key (key (entry set-of-records))))
	(cond ((= given-key entry-key)
	       (entry set-of-records))
	      ((< given-key entry-key)
	       (lookup given-key (left-branch set-of-records)))
	      ((> given-key entry-key)
	       (lookup given-key (right-branch set-of-records)))))))

;; Exercise 2.67

;; The result given by the decode procedure (decode sample-message sample-tree) is (A D A B B C A).

;; Exercise 2.68

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
	((member symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((member symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else (error "bad symbol: ENCODE-SYMBOL" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;; Exercise 2.69

(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((first (car set))
	    (second (cadr set)))
	(successive-merge (adjoin-set (make-code-tree first second)
				      (cddr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; Exercise 2.70

(define lyrics-tree
  (generate-huffman-tree
   '((A    2)
     (GET  2)
     (SHA  3)
     (WAH  1)
     (BOOM 1)
     (JOB  2)
     (NA  16)
     (YIP  9))))

(define lyrics-encoding
  (encode '(GET A JOB
		SHA NA NA NA NA NA NA NA NA
		GET A JOB
		SHA NA NA NA NA NA NA NA NA
		WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
		SHA BOOM)
	  lyrics-tree))

;; 84 bits are required for the encoding. The smallest number of bits that would be required to encode this song if we used a fixed-length code for the eight-symbol alphabet is 36 * 3, or 108.

;; Exercise 2.71

;;           63
;;          /  \
;;        32    31
;;             /  \
;;           16    15
;;                /  \
;;               8    7
;;                   / \
;;                  4   3
;;                     / \
;;                    2   1

;; In such a tree (general for n), n bits are required to encode the least frequent symbol, and 1 bit is required to encode the most frequent symbol.

;; Exercise 2.72

;; To encode the least frequent symbol in the alphabet, the encode-symbol procedure needs to move down the tree from the root node to the symbol's corresponding leaf. In Exercise 2.71, we have seen that n bits are required to encode the symbol, so it will encounter n nodes during the encoding process. In addition, at each node, the procedure needs to check if the symbol is in the set at that node and at its sibling. In the worst, it takes n, ..., 2 steps to perform the check at each node along the chain. Hence, the number of steps required to encode the least frequent symbol is
;; n + (n - 1) + ... + 2 = Theta(n^2).

;; To encode the most frequent symbol in the alphabet, the encoding-symbol procedure just needs to move one step to its corresponding leaf, as is illustrated by the figure in Exercise 2.71. Note that the number of steps required to check if the symbol is in the set depends on the specific implementation of the Huffman encoding tree. If the leaf is the left child of the root, then it only takes Theta(1) steps to check if the symbol is in the node's set, and thus the overall number of steps required to encode the most frequent symbol is Theta(1). If, however, the leaf is the right child of the root, the it takes Theta(n) steps to check if the symbol is in the node's set, and thus the overall number of steps required to encode the most frequent symbol is Theta(n).
