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

