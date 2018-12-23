;; Newton's method to compute the square root of a number

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Factorial, recursively and iteratively

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
		   (+ counter 1))))
  (iter 1 1))

;; Fibonacci numbers, recursively and iteratively

(define (fib-recursive n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib-recursive (- n 1))
		 (fib-recursive (- n 2))))))

(define (fib-iterative n)
  (define (iter a b count)
    (if (= count 0)
	b
	(iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;; Counting change

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (cc amount 5))

;; Exponentiation, linearly and logarithmically

(define (expt-recursive b n)
  (if (= n 0)
      1
      (* b (expt-recursive b (- n 1)))))

(define (expt-iterative b n)
  (define (iter counter product)
    (if (= counter 0)
	product
	(iter (- counter 1)
	      (* product b))))
  (iter n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;; Euclid's Algorithm to compute the GCD of two numbers

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Primality test, searching for divisors and fermat test

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square (expmod base (/ exp 2) m))
	  m))
	(else
	 (remainder
	  (* base
	     (expmod base (- exp 1) m))
	  m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;; Summation

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))
(define (sum-cube a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2))
	  (lambda (x) (+ x dx))
	  b)
     dx))

;; Half-interval method to find roots of an equation

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; Finding fixed points of functions

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (closed-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (closed-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

;; Newton's method, generally

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Computing the square root of a number, with fixed-point and with Newton's method

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fixed-point x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-newtons-method x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))
