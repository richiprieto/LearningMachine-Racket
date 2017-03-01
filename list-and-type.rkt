;Construya la función (tipo-atributo <ejemplo-AT>) que devuelva
;una lista en la que se especifique de qué tipo es cada atributo de un ejemplo
;(nominal o numérico).
;Taller 1, exercise 1 with global variable
#lang racket
(define (tipoAtributo lst)
  (define tipo '())
  (for ([i lst])
    (cond
      ((number? (car i))
       (set! tipo (append tipo '(numerico))))
      (else (set! tipo (append tipo '(nominal))))      
    )
  )
  tipo
)
