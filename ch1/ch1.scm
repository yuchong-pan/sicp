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
