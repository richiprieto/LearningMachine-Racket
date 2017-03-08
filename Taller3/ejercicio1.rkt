#|Construya las funciones (match-LUU concepto-UU ejemplo-sin-
clase) e (interprete-LUU concepto-UU ejemplo-sin-clase), que realicen lo
siguiente:
-La función (match-LUU concepto-UU ejemplo-sin-clase) debe seguir los
siguientes pasos:
- Tomar un ejemplo ejemplo-sin-clase y traducirlo en un vector-LUU.
- Agregarle el valor de 1 al final del vector-LUU: (set! Vector-LUU
(append vector-LUU 1)). Esto se hace debido a que, el valor
esperado de los ejemplos positivo será de 1 .
- Realizar el producto punto entre el concepto-LUU y el vector-LUU.
- Si es que la operación anterior devuelve un valor mayor a 0, la
función debe devolver el valor #t; caso contrario, debe devolver el
valor #f.
-En la carpeta “ejerciciosC3.zip” se encuentra disponible el código
para traducir un atributo y para calcular el producto punto.
Entrada: (match-LUU (list(car ejemplos)'(1.15 -1.96 -1.54)) '(5 2.5))
Salida: #f
Entrada: (match-LUU (list(car ejemplos)'(1.15 -1.96 -1.54)) '(6 1.75))
Salida: #t
La función (interprete-LUU concepto-UU ejemplo-sin-clase) debe asignar
una clase a un ejemplo-sin-clase, basándose en el resultado de la función
match-LUU, implementada anteriormente. Si es que match-LUU devuelve
#t, se debe colocar al final del ejemplo sin clase, la clase +; caso contrario,
se debe colocar la clase -.
Entrada: (interprete-LUU (list(car ejemplos)'(1.15 -1.96 -1.54)) '(5 2.5))
Salida: '(5 2.5 -)
Entrada: (interprete-LUU (list(car ejemplos)'(1.15 -1.96 -1.54)) '(6 1.75))
Salida: '(6 1.75 +)

|#
#lang racket

(define ejemplos null)
(define output null)

(define (dot-product l r)
  (for/sum ([x l] [y r]) (* x y))
  )

(define (leer-ejemplos archivo)
  (set! ejemplos (with-input-from-file archivo read)
        )
  )
;;Función para leer los datos de un ejemplo
(leer-ejemplos "ejemplosB3P.scm")


(define (match-LUU [concepto-UU empty][ejemplo-sin-clase empty])
  (let ([result empty])
    (set! ejemplo-sin-clase(append ejemplo-sin-clase '(1)))
    (set! result (dot-product (car(cdr concepto-UU)) ejemplo-sin-clase))
      (cond
        [(positive? result) (set! output #t)]
        [else (set! output #f)]
        )
    output
    )
  )

(define (interprete-LUU concepto-UU ejemplo-sin-clase)
  (define clase (match-LUU concepto-UU ejemplo-sin-clase))
  (cond
    [(equal? clase '#t)
     (set! ejemplo-sin-clase (append ejemplo-sin-clase '(+)))
     ][else
       (set! ejemplo-sin-clase (append ejemplo-sin-clase '(-)))
       ]
    )
  ejemplo-sin-clase
  )