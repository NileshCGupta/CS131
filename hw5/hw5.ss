(define (null-ld? obj)
	(if (pair? obj)
		(if (eq? (car obj) (cdr obj)) 
			#t 
			#f
		)
	#f
	)
)

(define (listdiff? obj)
	(if (pair? obj)
		(if (null-ld? obj)
			#t
			(if (pair? (car obj))
				(listdiff? (cons (cdr (car obj)) (cdr obj)))
			 	#f
			)		
		)
	#f
	)
)

(define (cons-ld obj listdiff)
	(if (listdiff? listdiff)
		(cons (cons obj (car listdiff))	(cdr listdiff))	
		#f
	)	
)

(define (car-ld listdiff)
	(if (listdiff? listdiff)
		(if (not (eq? (car listdiff) (cdr listdiff)))
			(car (car listdiff))
			error
		)
	error
	)
)

(define (cdr-ld listdiff)
	(if (listdiff? listdiff)
		(cons (cdr (car listdiff)) (cdr listdiff))
		error
	)
)

(define (listdiff . args)
	(cons (append args '0) '0)
)

(define (length-ld listdiff)
	(if (listdiff? listdiff)
		(if (eq? (car listdiff) (cdr listdiff))
			0
			(if (pair? (car listdiff))
				(+ 1 (length-ld (cons (cdr (car listdiff)) (cdr listdiff))))
				error
			)
		)
		error
	)
)

(define (append-ld . args)
	(list->listdiff (makelist args))
)

(define (makelist listoflds)
	(if (eq? listoflds '())
		'()
		(append (listdiff->list (car listoflds)) (makelist (cdr listoflds)))
	)
)

(define (assq-ld obj alistdiff)
	(if (pair? (car (car alistdiff)))
		(if (eq? (car alistdiff) '())
			#f
			(if (eq? (car (car (car alistdiff))) obj)
				(car (car alistdiff))
				(assq-ld obj (cons (cdr (car alistdiff)) (cdr alistdiff)))
			)
		)
		#f
	)
)

(define (list->listdiff alist)
	(cons (append alist '0) '0)
)

(define (listdiff->list listdiff)
	(if (null-ld? listdiff)
		'()
		(cons 
			(car (car listdiff))
			(listdiff->list (cons (cdr (car listdiff)) (cdr listdiff)))
		)
	)
)

(define (expr-returning listdiff)
	(append 
		'(cons) 
		(append 
			(list (helpf listdiff)) 
			(list (cons 'quote (list '())))
		)
	)
)

(define (helpf listdiff)
	(cons 'quote (listdiff->list listdiff))
)

