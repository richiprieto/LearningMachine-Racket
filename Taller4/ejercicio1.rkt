#|
Construya las funciones (distancia-EU ejemplo-con-clase ejemplo-
sin-clase) e (interprete-Distancia-EU ejemplos ejemplo-sin-clase) que realicen lo
siguiente:

-La función (distancia-EU ejemplo-con-clase ejemplo-sin-clase) debe
calcular la distancia euclidiana entre un ejemplo-con-clase y un ejemplo-
sin-clase. Recuerde que la fórmula para calcular la distancia euclidiana es
la siguiente:
        [ a             ]
d = sqrt| ∑(pi - xi )^2 |
        [ i=1           ]

a = numero de atributos de los ejemplos
pi= valor del atributo i del ejemplo-con-clase
xi= valor del atributo i del ejemplo-sin-clase

Tomar en consideración que, si se evalúa un atributo nominal, el valor de la
distancia euclidiana, será de 0 cuando el atributo nominal del ejemplo-sin-clase
coincida literalmente con el atributo nominal del ejemplo-con-clase; caso
contrario, será de 1. Por el momento, no considerar para el cálculo la clase del
ejemplo-con-clase.

Entrada: (distancia-EU '(soleado 10 20 si) '(soleado 25 75 si +))
Salida: 57.0087712549569

Entrada: (distancia-EU '(soleado 10 20 si) ' (lluvioso 20 21 no -))
Salida: 10.14889156509222

-La función (interprete-Distancia-EU ejemplos ejemplo-sin-clase) debe
asignar una clase a un ejemplo sin clase, basándose en el concepto de
distancia euclidiana. Para esto debe utilizar la lista de ejemplos de
entrenamiento disponible en el AVAC (ejemplosB4.scm) y seguir los
siguientes pasos:
  - Calcular la distancia euclidiana entre el ejemplo-sin-clase y el
promedio de cada clase de entrenamiento. 
  - Asignar al ejemplo-sin-clase, la clase del ejemplo de entrenamiento
con el cual obtenga la menor distancia euclidiana.

Entrada: (interprete-Distancia-EU ejemplos '(soleado 15 21 no))
Salida: '(soleado 15 21 no -)

Entrada: (interprete-Distancia-EU ejemplos '(soleado 30 50 si))
Salida: '(soleado 30 50 si +)
|#
#lang racket

(define ejemplos null)

(define (leer-ejemplos archivo)
  (set! ejemplos (with-input-from-file archivo read)
        )
  )
;;Función para leer los datos de un ejemplo
(leer-ejemplos "ejemplosB4.scm")

(define(distancia-EU ejemplo-con-clase ejemplo-sin-clase)
  (let ([dist empty]
        [output 0]
        )
    (for ([i ejemplo-con-clase][j ejemplo-sin-clase])
      (cond
        [(number? i)(set! dist (expt(- i j) 2))(set! output(+ dist output))]
        [else
         (cond
           [(equal? i j)(set! output (+ 0 output))]
           [else(set! output (+ 1 output))]
           )
         ]
        )
      )
    (set! output (sqrt output))
    output
    )
  )

(define (interprete-Distancia-EU ejemplos ejemplo-sin-clase)
  (let (
        [promedio-pos 0]
        [num-pos 0]
        [promedio-neg 0]
        [num-neg 0]
        )
    (for ([k (cdr ejemplos)])
      (cond
        [(equal? (last k) '+)(set! promedio-pos (+ promedio-pos (distancia-EU k ejemplo-sin-clase)))(set! num-pos(add1 num-pos))]
        [(equal? (last k) '-)(set! promedio-neg (+ promedio-neg (distancia-EU k ejemplo-sin-clase)))(set! num-neg(add1 num-neg))]
        )
      )
    (set! promedio-pos (/ promedio-pos num-pos))
    (set! promedio-neg (/ promedio-neg num-neg))
    (cond
      [(< promedio-pos promedio-neg)(set! ejemplo-sin-clase (append ejemplo-sin-clase '(+)))]
      [else (set! ejemplo-sin-clase (append ejemplo-sin-clase '(-)))]
      )
    ejemplo-sin-clase
    )
  )