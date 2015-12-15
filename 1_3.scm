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

#|
(newline)
(display (exact->inexact (john-wallis-pi 100)))
(newline)
(display (exact->inexact (john-wallis-pi 1000)))
(newline)
(display (exact->inexact (john-wallis-pi 10000)))
(newline)
(display (exact->inexact (john-wallis-pi 100000)))


(newline)
(display (acc-sum identity 1 inc 6))
(newline)
(display (sum identity 1 inc 6))
|#


(newline)
(display (filtered-accumulate * even? 1 identity 1 inc 6))
