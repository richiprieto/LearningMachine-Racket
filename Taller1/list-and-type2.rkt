;Construya la función (tipo-atributo <ejemplo-AT>) que devuelva
;una lista en la que se especifique de qué tipo es cada atributo de un ejemplo
;(nominal o numérico).
;Entrada: (tipo-atributo '((soleado)(10)(lluvioso)(15)))
;Salida: '(nonimal numérico nonimal numérico)
;local variable using let

#lang racket
(define (tipoAtributo lst)
  (let ([tipo empty])
    (for ([i lst])
      (cond
        ((number? (car i))
         (set! tipo (append tipo '(numerico))))
        (else (set! tipo (append tipo '(nominal))))
      )
    )
    tipo
  )
)
