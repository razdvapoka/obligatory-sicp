; >> PRIME TESTS

(define (smallest-divisor n)
    (find-divisor n 2))
    
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))
    
(define (prime? n)
    (= n (smallest-divisor n)))

(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m)) 
                      m))
           (else
                (remainder (* base (expmod base (- exp 1) m))
                           m))))
                           
(define (expmod2 base exp m)
        (remainder (fast-expt base exp) m))
                           
(define (fermat-test n)
    (define (try-it a)
            (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
    
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (real-time-clock) start-time))
        #f))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    #t)

(define (search-for-primes current primes-found primes-needed)
        (cond ((= primes-found primes-needed) #t)
              ((timed-prime-test current) (search-for-primes (+ current 2) (+ primes-found 1) primes-needed))
              (else (search-for-primes (+ current 2) primes-found primes-needed))))

; << PRIME TESTS

; >> ABSTRACTIONS

(define (square x)
    (* x x))

(define (cube x)
    (* x x x))

(define (sum-integers a b)
        (if (> a b)
            0
            (+ a (sum-integers (+ a 1) b))))

         
(define (sum-cubes a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))))

        
(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
      
        
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))
           
(define (iter-sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))
    
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))
           
(define (iter-product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))
    
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))
                  
(define (iter-accumulate combiner null-value term a next b)
    (define (iter combiner a result)
        (if (> a b)
            result
            (iter combiner (next a) (combiner result (term a)))))
    (iter combiner a null-value))

(define (filtered-accumulate combiner filter null-value term a next b)
    (define (iter combiner filter a result)
        (cond ((> a b) result)
              ((filter a) (iter combiner filter (next a) (combiner result (term a))))
              (else (iter combiner filter (next a) result))))
    (iter combiner filter a null-value))

(define (acc-sum term a next b)
    (iter-accumulate + 0 term a next b))
    
(define (acc-product term a next b)
    (iter-accumulate * 1 term a next b))    


(define (inc n) (+ n 1))

(define (sum-cubes a b)
    (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
    (sum identity a inc b))

(define (pi-sum a b)
    (define (pi-term x)
        (+ (/ 1.0 (* x (+ x 2)))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))
    
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2)) add-dx b) dx))
    
(define (iter-integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (iter-sum f (+ a (/ dx 2)) add-dx b) dx))
    
(define (even? x)
    ( = (remainder x 2) 0))

(define (simps-integral f a b n)
    (define (comp-h)
        (/ (- b a) n))
    (define (simps-term k)
        (define (mult k)
        (cond ((or (= k 0) (= k n)) 1)
              ((even? k) 2)
              (else 4)))
        (* (mult k) (f (+ a (* k (comp-h))))))
    (* (/ (comp-h) 3) (sum simps-term 0 inc n)))


(define (factorial x)
    (product identity 1 inc x))
    
(define (john-wallis-pi n)
    (define (jw-next x)
        (+ x 2))
    (define (jw-term x)
        (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
    (* 4 (acc-product jw-term 2 jw-next n)))

(define (sum-prime-squares a b)
    (filtered-accumulate + prime? 0 square a inc b))
    

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b))))

    (define (rel-prime? x y)
        (= (gcd x y) 1))

(define (prod-relatively-prime n)
    (define (rel-prime? x)
        (= (gcd x n) 1))
    (filtered-accumulate * rel-prime? 1 identity 1 inc n))


(define (f x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
      (+ (* x (square a))
         (* y b)
         (* a b))))    

(define (average x y)
    (/ (+ x y) 2))
    
(define (close-enough? x y)
    (< (abs (- x y)) 0.000001))    

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value)
                       (search f neg-point midpoint))
                      ((negative? test-value)
                       (search f midpoint pos-point))
                      (else midpoint))))))

(define (half-interval f a b)
    (let ((a-value (f a))
          (b-value (f b)))
      (cond ((and (negative? a-value) (positive? b-value))
             (search f a b))
            ((and (negative? b-value) (positive? a-value))
             (search f b a))
            (else
             (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
    (define (try guess)
        (newline)
        (display guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))
    
;(define (sqrt x)
;    (fixed-point (lambda (y) (average y (/ x y))) 
;                 1.0))

;(newline)
;(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;(newline)
;(display (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;(newline)
;(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 4.5))

;(newline)
;(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 4.5))


(define (cont-frac n d k)
    (define (cont-frac-inner i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (cont-frac-inner (+ i 1))))))
    (cont-frac-inner 1))
    
(define (iter-cont-frac n d k)
    (define (cont-frac-step i result)
        (if (= i 0)
            result
            (cont-frac-step (- i 1) (/ (n i) (+ (d i) result)))))
    (cont-frac-step k 0))


(define (d i)
    (if (= (remainder i 3) 2)
        (* 2.0 (+ (quotient i 3) 1.0))
        1.0))

(define (tan-cf x k)
    (iter-cont-frac
        (lambda (i) 
            (if (= i 1) 
                x 
                (* -1 (* x x))))
        (lambda (i) (+ 1 (* 2 (- i 1))))
        k))
 
(define (average-damp f)
    (lambda (x) (average x (f x))))           


(define (sqrt x)
    (fixed-point 
        (average-damp (lambda (y) (/ x y)))
        1.0))
        
(define (cube-root x)
    (fixed-point
        (average-damp (lambda (y) (/ x (square y))))
        1.0))        
         
(define dx 1)

(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x))
           dx)))
           
(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))))
        
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))
    
(define (newtons-sqrt x)
    (newtons-method (lambda (y) (- (square y) x))
                    1.0))
                    
(define (fixed-point-of-transform f tr guess)
    (fixed-point (tr f) guess))
    
(define (ft-newtons-sqrt x)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
                              newton-transform
                              1.0))
                              
(define (yao-sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                              average-damp
                              1.0))
                       
;(newline)       
;(display (ft-newtons-sqrt 2))
;(newline)
;(display (yao-sqrt 2))

(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
    
(define (cubic-zero a b c)
    (newtons-method (cubic a b c) 1))
    
(define (double f)
    (lambda (x) (f (f x))))
    
(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (rep-iter curr-f i)
        (if (= i 1)
            curr-f
            (rep-iter (compose f curr-f) (- i 1))))
    (rep-iter f n))
        
(define (triple-avg a b c)
    (/ (+ a b c) 3.0))
        
(define (smooth f)
    (lambda (x) (triple-avg (f (- x dx)) (f x) (f (+ x dx)))))
   
(define (smooth-n-times f n)
    ((repeated smooth n) f))    
    
(define (writeline x)
     (newline)
     (display x))
    
(define (test-f f a b)
    (cond ((<= a b)
           (writeline (f a))
           (test-f f (+ a 1) b))))
           
;(test-f (lambda (x) (fast-expt x 3)) 1 10)
;(newline)
;(test-f (smooth (lambda (x) (fast-expt x 3))) 1 10)
;(newline)
;(test-f (smooth-n-times (lambda (x) (fast-expt x 3)) 3) 1 10)


(define (n-root x n)
    (fixed-point
        ((repeated average-damp (- n 2)) (lambda (y) (/ x (fast-expt y (- n 1)))))
        1.0))
        
;(define base 100)
;(define pow 9)
;(define res (n-root base pow))
;(newline)
;(display res)
;(newline)
;(display (fast-expt res pow))


(define (iterative-improve test-func next-guess-func)
    (define (try x)
        (newline)
        (display x)
        (if (test-func x)
            x
            (try (next-guess-func x))))
    try)

(define (super-sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    ((iterative-improve good-enough? improve) 1.0))

;(newline)
;(test-f super-sqrt 10 20)
;(newline)
;(test-f sqrt-t 10 20)
    
(define (super-fixed-point f first-guess)
    (define (sfp-test x)
        (close-enough? x (f x)))
    ((iterative-improve sfp-test f) first-guess))

;(newline)
;(fixed-point cos 0.5)
;(newline)
;(super-fixed-point cos 0.5)


; << ABSTRACTIONS
