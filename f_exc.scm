(define (f n)
        (f-iter 0 (- n 1) (- n 2) (- n 3) 1 n))
        
(define (f-iter res f-1 f-2 f-3 c n)
		(cond ((< n 3) n)
			  ((= c n) res)
			  (else (f-iter (+ f-1 (* 2 f-2) (* 3 f-3)) res (* 2 f-1) (* 3 f-2) (+ c 1) n))))
