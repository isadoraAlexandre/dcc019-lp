#lang racket
(define (inc x) (+ x 1))

(define (apply f x)
  (f x))

; funçõe de orden supeior aparametro ou retoro outra função

; funções anonimas
(define (add-n n)
  (lambda (m) (+ m n)))

;função superior map inc (lista 1 2 3)) avaliar para
(define (map f l)
  (if (empty? l) l
      (cons (f (first l)) (map f (rest l)))))

; abstracao procedimento ==> funções
; e dados => dsefiniçãos de tipos dee dados
;interface x implenetação
; opeacaçõ de criar cansultar e ocultação de ifnormação

(define (plus x y)
  (if (zero? x) y
      (suc (plus (pred x) y))))

;tipo de dados abstrato

#|(define zero '())

(define (is-zero? n)
  (null? n))

(define (suc n)
  (cons #t n))

(define (pred n)
  (rest n))|#

(define zero 0)

(define (is-zero? n)
  (zero? n))

(define (suc n)
  (+ n 1))

(define (pred n)
  (- n 1))

;enviroment variaveis -> valores
;empty-env
;extend-env var value env
;apply-env env var

#|(define empty-env '(empty-env))
(define (extend-env var value env)
  (list 'extend-end var value env))
(define (apply-env env var)
  (if (equal? 'empty-env (car env)) #f
      (if (equal? var (cadr env)) (caddr env)
          (apply-env (cadddr env) var))))|#

; imlementação procedural
;env = var -> value

(define (empty-env var) #f)
(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))
(define (apply-env env var)
  (env var))
  
(define env (extend-env 'd 6
                        (extend-env 'a 1
                                    empty-env)))

(define hex-zero '())

(define (is-hex-zero? n)
  (null? n))

(define (hex-suc n)
  (cond
    [(null? n) '(1)]
    [(< (car n) 15)
     (cons (+ (car n) 1) (cdr n))]
    [else
     (cons 0 (hex-suc (cdr n)))]))

(define (hex-pred n)
  (cond
    [(null? n) '()] ; zero continua zero
    [(> (car n) 0)
     (normalize (cons (- (car n) 1) (cdr n)))]
    [else
     (normalize (cons 15 (hex-pred (cdr n))))]))

(define (normalize n)
  (cond
    [(null? n) '()]
    [(and (null? (cdr n)) (= (car n) 0)) '()]
    [else (cons (car n) (normalize (cdr n)))]))

; =========================================================================

; exercicio 2.1 e 2.2 classroom

[(add-ast e1 e2) (+ (value-of e1 Δ) (value-of e2 Δ))]
[(mult-ast e1 e2) (* (value-of e1 Δ) (value-of e2 Δ))]
[(div-ast e1 e2) (/ (value-of e1 Δ) (value-of e2 Δ))]

(if-ast (zero?-ast (int-ast 3)) 
    (mult-ast (int-ast 2) (int-ast 0))
        (add-ast (mult-ast (int-ast 2) (int-ast 3))
                 (div-ast (int-ast 16) (int-ast 4))))


; exercicio 4.16 livro
(let times4 (lit 0) 
	(begin 
		(set times4 (proc x 
			(if (zero? x)
				(lit 0)
				(dif (call times4 (dif (var x) (lit 1)) (lit -4))))))
		(call times4 (lit 3))		
	)
)
















