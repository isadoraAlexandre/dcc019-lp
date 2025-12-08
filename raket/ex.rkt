#lang racket
(define tres 3)

(define (id x ) x)

;dobro
(define (dobro x) (* 2 x))
;chamda (dobro 3)

;soma
(define (soma x y) (+ x y))

;tupla '(cons 1 . 2)
;(car (cons 1 . 2))
;(cdr (cons 1 . 2))

; primeiro elem da tupla
(define (first t) (car t))

(define (fat n)
	(if (= n 0) 1 (* fat (- n 1))))

(define (fat n)
	(if (eq? n 0) 1 (* fat (- n 1))))
	
(define (fat n)
	(if (zero? n) 1 (*n (fat (- n 1)))))
	
(define (fat1 n)
	(match n
		[0 1]
		[_ (*n (fat (- n 1)))]))

(define (fib n)
	(cond
		[(zero? n) 1]
		[(= n 1) 1]
		[else (+ (fib (- n 1)) (fib (- n 2)))]))
		
; concat
(define (concat l1 l2)
	(match l1
		['() l2]
		[(cons x xs) (cons x (concat xs l2))]))

;reverse
(define (reverse l1 l2)
	(match l1
		['() '()]
		[(cons x xs) (concat (reverse xs) (list x))]
	)
)

(define (zip l1 l2)
	(match l1
		['() '()]
		[(cons x xs)
			(match l2
				['() '()]
				[(cons y ys) (cons (cons x y)(zip xs ys))]
			)
		]
	)
)

(define (inc x) (+ x 1))
(define (app f x) (f x))

(define (map f l)
	[match l
		{'() '()}
		{(cons x xs) (cons (f x) (map f xs))}
	]
)

(define (filter p l)
	[match l 
		['() '()]
		[(cons x xs) 
			(cond 
				[(p x) (cons x (filter p xs))]
				[else (filter p xs)]
			)
		]
	]
)
				
;(filter (lambda (x) (> x 5)) '(1,2,3,4)


