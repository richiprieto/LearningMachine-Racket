#lang racket

;Función para traducir un atributo a un valor numérico

(define (traducir meta-atributo valor)
  (let ([traduccion null])
    (cond [(number? valor)
         (set! traduccion valor)
         ]
        [else
         (for ([i (length (car (cdr meta-atributo)))])
           (cond [(equal? valor (list-ref (car (cdr meta-atributo)) i))
                  (set! traduccion i)
                  ]
                 )
           )
         ]
        )
  traduccion
    )
  )
;;Ejemplo de implementación de la función traducir
;;(traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)

;Funcion que permite calcular el producto punto entre dos vectores

(define (dot-product l r)
  (for/sum ([x l] [y r]) (* x y))
  )

;;Ejemplo de implementación de la función dot-product
;;(dot-product '(1.15 -1.96 -1.54) '(5 2.5 1))


