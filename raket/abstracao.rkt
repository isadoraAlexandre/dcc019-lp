#lang racket

; Abstração
  ; procedimentos ===> funções
  ; dados ===========> definição de tipos de dados

; Interface x Implementação
  ; Operação para criar
  ; Operação para consultar / manipular
  ; Ocultação de informação

; Exemplo : tipos para números naturais
  ; zero -----------> construtor do valor zero
  ; (is-zero? n) ---> observer
  ; (suc n) --------> sucessor de n
  ; (pred n) -------> predecessor de n

  ; (print n) ------> impressão de um número na tela

; Somente com as operações da interface implementar (plus n m)
(define (plus n m)
  ;TODO
  void ; remove
    )

; Exemplo de código
#;(print
  (plus zero (suc zero)))



































; Definição do tipo de dados natural
  ; Primeira representação: unária com lista de 1's
(define zero '())
(define is-zero? empty?)
(define (suc n)
  (cons 1 n))
(define (pred n)
  (if (is-zero? n)
      zero
      (cdr n)))
(define (print n)
  (if (is-zero? n)
      0
      (foldl + 0 n)))
     
  ; Segunda representação
#|(define zero 0)
(define is-zero? zero?)
(define (suc n)
  (add1 n))
(define (pred n)
  (if (is-zero? n)
      0
      (sub1 n)))
(define (print n)
  n)
|#
; Solução da definição da função plus
#;(define (plus n m)
  (if (is-zero? n) m
      (suc (plus (pred n) m))))
