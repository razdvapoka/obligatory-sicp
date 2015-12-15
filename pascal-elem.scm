(define (pascal-elem row pos) 
	(cond ((or (< pos 1) (> pos row)) 0)
	      ((= row 1) 1)
	      (else (+ (pascal-elem (- row 1) pos) (pascal-elem (- row 1) (- pos 1))))))
