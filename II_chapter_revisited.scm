; 2.2.1 Representing sequences

; List operations

(define (list-ref l n)
    (cond ((null? l) (list))
          ((= n 0)  (car l))
          (else (list-ref (cdr l) (- n 1)))))

#|          
(define (length l)
    (if (null? l)
        0
        (+ 1 (length (cdr l)))))
|#

(define (length l)
    (define (length-iter l acc_len)
        (if (null? l)
            acc_len
            (length-iter (cdr l) (+ 1 acc_len))))
    (length-iter l 0))


;(newline)
;(display (length (list 1)))

(define (append l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))))

; Exercises        
        
;2.17

(define (last-pair l)
    (cond ((null? l) (list))
          ((null? (cdr l)) (car l))
          (else (last-pair (cdr l)))))
          
;2.18

;(define (reverse l)
;    (if (null? l)
;        (list)
;        (append (reverse (cdr l)) (list (car l)))))
        
(define (reverse l)
    (define (rev-iter l rev-l)
        (if (null? l)
            rev-l
            (rev-iter (cdr l) (cons (car l) rev-l))))
    (rev-iter l (list)))
    
    
;2.19

(define (no-more? coin-values)
    (null? coin-values))
    
(define (first-denomination coin-values)
    (car coin-values))
    
(define (except-first-denomination coin-values)
    (cdr coin-values))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
            (+ (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount
                      (first-denomination coin-values))
                   coin-values)))))

;2.20

(define (same-parity first . rest)
    (define (same-parity-inner l)
        (cond ((null? l) (list))
               ((= (remainder first 2) (remainder (car l) 2)) 
                (cons (car l) (same-parity-inner (cdr l))))
               (else (same-parity-inner (cdr l)))))
    (cons first (same-parity-inner rest)))

; Mapping over lists
#|
(define (map proc items)
    (if (null? items) 
        (list)
        (cons (proc (car items)) (map proc (cdr items)))))
        
(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))
|#    
    
; Exercises

;2.21

#|
(define (square-list items)
    (if (null? items)
        (list)
        (cons (* (car items) (car items)) (square-list (cdr items)))))
|#

(define (square-list items)
    (map (lambda (x) (* x x)) items))

;2.23

(define (for-each proc items)
    (cond ((null? items) #t)
          (else 
            (proc (car items))
            (for-each proc (cdr items)))))
            
; 2.2.2 Hierarchical structures

(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                   (count-leaves (cdr tree))))))

#|                   
(newline)
(display (count-leaves (list)))

(newline)
(display (count-leaves (list 1 2)))

(newline)
(display (count-leaves (list 1 (list 2 3))))

(newline)
(display (count-leaves (list (list 2 3) 1)))
                   
(define t (list (list 1 2) (list 3 4)))
(newline)
(display (count-leaves t))

(newline)
(display (count-leaves (append t t)))
|#

;2.25

#|
(define a (list 1 3 (list 5 7) 9))
(define b (list (list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(newline)
(display a)
(newline)
(display (car (cdr (car (cdr (cdr a))))))
(newline)
(display b)
(newline)
(display (car (car b)))
(newline)
(display c)
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))))
|#

;2.26

#|
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) = (1 2 3 4 5 6)
(cons x y) = ((1 2 3) 4 5 6)
(lisr x y) = ((1 2 3) (4 5 6))
|#

;2.27

(define (deep-reverse l)
    (define (deep-rev-iter l rev-l)
        (cond ((null? l) rev-l)
              ((not (pair? l)) l)
              (else (deep-rev-iter (cdr l) 
                                   (cons (deep-reverse (car l)) 
                                         rev-l)))))
    (deep-rev-iter l (list)))
    
;2.28

(define (fringe l)
    (cond ((null? l) (list))
          ((not (pair? l)) (list l))
          (else (append (fringe (car l)) (fringe (cdr l))))))

;2.29

#|
(define (make-mobile left right)
    (list left right))
    
(define (make-branch length structure)
    (list length structure))
    
; a)    
    
(define (left-branch mobile)
    (car mobile))
    
(define (right-branch mobile)
    (car (cdr mobile)))
    
(define (branch-length branch)
    (car branch))
    
(define (branch-structure branch)
    (car (cdr branch)))
|#

;d)

(define (make-mobile left right)
    (cons left right))
    
(define (make-branch length structure)
    (cons length structure))
   
(define (left-branch mobile)
    (car mobile))
    
(define (right-branch mobile)
    (cdr mobile))
    
(define (branch-length branch)
    (car branch))
    
(define (branch-structure branch)
    (cdr branch))

; b)    
    
(define (mobile? candidate)
    (pair? candidate))
    
(define (branch-weight branch)
    (if (mobile? (branch-structure branch))
        (mobile-weight (branch-structure branch))
        (branch-structure branch)))
        
(define (branch-torque branch)
    (* (branch-length branch) 
       (branch-weight branch)))
    
(define (mobile-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))
       
(define l1 (make-branch 2 10))
(define r1 (make-branch 1 20))

(define m1 (make-mobile l1 r1))

(define l2 (make-branch 2 30))
(define r2 (make-branch 3 20))

(define m2 (make-mobile l2 r2))

(define l3 (make-branch 5 m1))

(define r3 (make-branch 3 m2))

(define mob (make-mobile l3 r3))

#|
(newline)
(display (branch-weight l1))
(newline)
(display (branch-weight r1))
(newline)
(display (mobile-weight m1))
(newline)
(display (branch-weight l2))
(newline)
(display (branch-weight r2))
(newline)
(display (mobile-weight m2))
(newline)
(display (branch-weight l3))
(newline)
(display (branch-weight r3))
(newline)
(display (mobile-weight mob))
|#

; c)

(define (mobile-balanced? mobile)
    (if (not (mobile? mobile))
        #t
        (let ((left (left-branch mobile))
              (right (right-branch mobile)))
             (and (= (branch-torque left)
                     (branch-torque right))
                 (mobile-balanced? (branch-structure left))
                 (mobile-balanced? (branch-structure right))))))

#|
(newline)
(display (mobile-balanced? m1))
(newline)
(display (mobile-balanced? m2))             
(newline)
(display (mobile-balanced? mob))
|#

; Mapping over trees

#|
(define (scale-tree tree factor)
    (cond ((null? tree) (list))
          ((not (pair? tree)) (* tree factor))
          (else (cons (scale-tree (car tree) factor) 
                      (scale-tree (cdr tree) factor)))))
|#
(define (scale-tree tree factor)
    (map
        (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree sub-tree factor)
                (* sub-tree factor)))
        tree))

#|        
        
(define t (list 1 2 (list 3 4 (list 7 8)) 5 6))
(newline)
(display (scale-tree t 1000))

|#

; Exercises

;2.30

#|

(define (square-tree tree)
    (cond ((null? tree) (list))
          ((not (pair? tree)) (* tree tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))
                      

(define (square-tree tree)
    (map
        (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (* sub-tree sub-tree)))
        tree))
        
        
(define t (list 1 2 (list 3 4 (list 7 8)) 5 6))        
(newline)
(display (square-tree t))
|#

;2.31

(define (tree-map proc tree)
    (map
        (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree)))
        tree))
        
(define (square-tree tree)
    (tree-map (lambda (x) (* x x)) tree))
    
#|    
    
(define t (list 1 2 (list 3 4 (list 7 8)) 5 6))        
(newline)
(display (square-tree t))

|#

;2.32

(define (subsets s)
    (if (null? s)
        (list (list))
        (let ((rest (subsets (cdr s))))
             (append rest (map 
                              (lambda (l) (cons (car s) l)) 
                              rest)))))
#|                        
(define t (list))
(newline)
(display (subsets t))
(define t (list 1 2))
(newline)
(display (subsets t))
|#

; 2.2.3 Sequences as Conventional Interfaces


#|
(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree))
           (if (odd? tree)
               (square tree)
               0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))
          
          
(define (even-fibs n)
    (define (next k)
        (if (> k n)
            (list)
            (let ((f (fib k)))
                (if (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))))))
    (next 0))
    
|#    

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))))
    
; Sequence operations

(define (filter predicate sequence)
    (cond ((null? sequence) (list))
          ((predicate (car sequence))
           (cons (car sequence) 
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
          
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) 
            (accumulate op initial (cdr sequence)))))
            
(define (enumerate-interval low high)
    (if (> low high)
        (list)
        (cons low (enumerate-interval (+ low 1) high))))
        
(define (enumerate-tree tree)
    (cond ((null? tree) (list))
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))
                        
(define (sum-odd-squares tree)
    (accumulate + 
                0 
                (map square
                     (filter odd?
                            (enumerate-tree tree)))))
                    
(define (even-fibs n)
    (accumulate cons 
                (list)
                (filter even?
                        (map fib 
                            (enumerate-interval 0 n)))))
                            
(define (list-fib-squares n)    
    (accumulate cons 
                (list) 
                (map (lambda (x) (square (fib x)))
                    (enumerate-interval 0 n))))
                    
(define (product-of-squares-of-odd-elements sequence)
    (accumulate *
                1
                (map square
                     (filter odd?
                             sequence))))
                             
; Exercises

;2.33
#|
(define (map proc sequence)
    (accumulate (lambda (x y) 
                        (cons (proc x) y)) 
                (list) sequence))
|#                
(define (append seq1 seq2)
    (accumulate cons seq2 seq1))
    
(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
    
;2.34

(define (horner-eval x coeffs)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ this-coeff (* x higher-terms)))
                0
                coeffs))
                
;2.35

#|
(define (count-leaves tree)
    (accumulate (lambda (x y) 
                    (if (pair? x)
                        y
                        (+ 1 y)))
                0
                (enumerate-tree tree)))
|#

(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (x) 1) 
                     (enumerate-tree tree))))
                     
;2.36

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        (list)
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

             
#|              
(define s (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))
(newline)
(display (accumulate-n * 1 s))

(newline)
(display (accumulate-n + 0 s))
|#

;2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

#|
(define v (list 1 2 3))
(define w (list 4 5 6))
(newline)
(display (dot-product v w))
|#

(define (matrix-*-vector m v)
  (map (lambda (mv) 
         (dot-product mv v)) 
       m))
#|
(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 3 2 1))

(newline)
(display (matrix-*-vector m v))

(define m1 (list (list 0 4 -2) (list -4 -3 0)))
(define m2 (list (list 0 1) (list 1 -1) (list 2 3)))
|#

(define (transpose m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
      (map
         (lambda (mv)
            (map
                (lambda (nv)
                  (dot-product mv nv))
                cols))
         m)))

;2.38

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (accumulate op initial sequence))
    
; (newline)
; (display (fold-right / 1 (list 1 2 3)))
; (newline)
; (display (fold-left / 1 (list 1 2 3)))
; (newline)
; (display (fold-right list (list) (list 1 2 3)))
; (newline)
; (display (fold-left list (list) (list 1 2 3)))

;2.39

(define (fold-right-reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) (list) sequence)) 
    
(define (fold-left-reverse sequence)
    (fold-left (lambda (x y) (append (list y) x)) (list) sequence))
    
; Nested mappings


; >> PRIME TEST
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
                                    
(define (expmod base exp m)
        (remainder (fast-expt base exp) m))
                           
(define (fermat-test n)
    (define (try-it a)
            (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
    
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
          
(define (prime? n)
    (fast-prime? n 100))
          
; << PRIME TEST       


(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
                (unique-pairs n))))
                            
(define (flatmap proc seq)
    (accumulate append (list) (map proc seq)))
    
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
    
(define (make-pair-sum pair)
    (+ (car pair) (cadr pair)))
    
(define (remove x sequence)
    (filter (lambda (i) (not (= i x))) sequence))
    
(define (permutations set)
    (if (null? set)
        (list (list))
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x set))))
                 set)))
                 
; Exercises

;2.40

(define (unique-pairs n)    
    (flatmap (lambda (i)
                (map (lambda (j)
                       (list j i))
                     (enumerate-interval 1 (- i 1))))    
    (enumerate-interval 1 n)))
    
;2.41

(define (triple-sum triple)
    (+ (car triple)
       (cadr triple)
       (caddr triple)))
       
(define (unique-triples n)
    (flatmap (lambda (k)
                (map (lambda (p)
                        (append p (list k)))
                     (unique-pairs (- k 1))))
             (enumerate-interval 1 n)))
             
(define (filter-triples-by-sum n sum)
    (filter (lambda (x) 
                (= (triple-sum x) sum))
            (unique-triples n)))

;2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
              (adjoin-position new-row k rest-of-queens))
            (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board (list))

(define (adjoin-position row col prev-col)
  (cons row prev-col))

(define (safe? k positions)
  
(define (safe-iter rest i)
(cond ((null? rest) #t)
      ((or (= (car rest) (car positions))
           (= (car rest) (+ (car positions) i))
           (= (car rest) (- (car positions) i)))
       #f)
      (else (safe-iter (cdr rest) (+ i 1)))))

(if (< k 2)
#t
(safe-iter (cdr positions) 1)))


;2.43

;TODO

; Exercises 2.44 to 2.52 are implemented in painting.rkt
; Open and execute that code in drracket

; 2.3 SYMBOLIC DATA

(define exp (list 'car (list 'quote '(a b c))))
;(newline)
;(display x)


(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Excersises

; 2.53

(newline)
(display (list 'a 'b 'c))
; (a b c)

(newline)
(display (list (list 'george)))
; ((george))

(newline)
(display (cdr '((x1 x2) (y1 y2))))
; ((y1 y2))

(newline)
(display (cadr '((x1 x2) (y1 y2))))
; (y1 y2) 

(newline)
(display (pair? (car '(a short list))))
; #f 

(newline)
(display (memq 'red '((red shoes) (blue socks))))
; #f

(newline)
(display (memq 'red '(red shoes blue socks)))
; (red shoes blue socks)

; 2.54

(define (equal? list1 list2)
  (cond  ((and (null? list1) 
               (null? list2)) 
          #t)
         ((or (null? list1) 
              (null? list2)) 
          #f)
         ((and (pair? (car list1)) 
               (pair? (car list2)))
              (if (equal? (car list1) (car list2)) 
                  (equal? (cdr list1) (cdr list1))
                  #f))
         ((and (not (pair? (car list1))) 
               (not (pair? (car list2))))
              (if (eq? (car list1) (car list2)) 
                  (equal? (cdr list1) (cdr list2)) 
                  #f))
         (else #f)
        )
  )

; 2.55

; 'a... = (quote a...)
; '(quote a...) = (quote a...)
; (car (quote a...)) = quote = '


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (augend exp) var)
                              (deriv (addend exp) var)))
        ((product? exp) 
         (make-sum 
           (make-product (multiplier exp) 
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp) 
                         (deriv (multiplier exp) var))
           ))
        ((exponentiation? exp)
         (make-product 
           (exponent exp)
           (make-product 
             (make-exponentiation (base exp) 
                                  (make-sum (exponent exp) -1))
             (deriv (base exp) var))))
        (else 
          (error "unknown expression type: DERIV" exp))))

(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? x)
  (symbol? x))

(define (same-variable? var1 var2)
  (and (variable? var1) (variable? var2) (eq? var1 var2)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2) 
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (sum? x) 
  (and (pair? x) (eq? (car x) '+)))

(define (addend x) (cadr x))

(define (augend x) (caddr x))

(define (product? x) 
  (and (pair? x) (eq? (car x) '*))) 

(define (multiplier x) (cadr x))

(define (multiplicand x) (caddr x))


(define expr1 '(+ x 3))
(newline)
(display (deriv expr1 'x))

(define expr2 '(* x y))
(newline)
(display (deriv expr2 'x))

(define expr3 '(* (* x y) (+ x 3)))
(newline)
(display (deriv expr3 'x))


; Excercises

; 2.56

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))

(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))

(define (make-exponentiation bs ex)
  (cond ((=number? ex 0) 1)
        ((=number? ex 1) bs)
        ((and (number? bs) (number? ex)) (fast-expt bs ex))
        (else (list '^ bs ex))))

#|
(define e '(^ 2 5))
(newline)
(display (exponentiation? e))
(newline)
(display (exponent e))
(newline)
(display (base e))

(newline)
(display (make-exponentiation 2 5))

(newline)
(display (make-exponentiation 2 0))

(newline)
(display (make-exponentiation 2 1))

(newline)
(display (make-exponentiation 'a 3))
|#

(define ex2 '(^ (+ x 2) b))
(newline)
(display (deriv ex2 'x))
