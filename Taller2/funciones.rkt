;Construya las funciones (match-CL C1 C2) e (interprete-CL concepto ejemplo-sin-clase) que realicen lo siguiente:

;La función match-CL debe comparar dos conceptos lógicos y devolver #t
;si se cumplen las siguientes condiciones:
; C1 y C2 tienen la misma longitud.
; C2 tiene atributos numéricos en la misma posición que C1.
; Los valores de atributos de C2 coinciden con los valores de atributos
;de C1 en sus respectivas posiciones. Tomar en cuenta que, si los
;atributos son nominales deberán coincidir textualmente, mientras
;que si los conceptos son numéricos deberán estar dentro del rango
;indicado por C1.
; Caso contrario devolver #f.
;Entrada: (match-CL '((soleado)(*)(10 50)(si)) '(soleado 30 40 si))
;Salida: #t
;Entrada: (match-CL '((soleado)(*)(10 50)(si)) '(lluvioso 30 40 si))
;Salida: #f
;Entrada: (match-CL '((soleado)(*)(10 50)(si)) '(soleado 30 60 si))
;Salida: #f
;Tomar en consideración que cuando un atributo tenga el valor *, significará que dicho
;atributo acepta cualquier valor. Un atributo de este tipo es considerado como el atributo
;más general posible.
;La función interprete-CL debe asignar una clase a un ejemplo sin clase,
;basándose en el concepto de entrenamiento. Se debe usar la función
;match-CL para comparar el concepto de entrenamiento con el ejemplo sin
;clase. Si es que que match-CL devuelve #t, se debe colocar al final del
;ejemplo sin clase, la clase +; caso contrario, se debe colocar la clase -.
;Entrada: (interprete-CL '((soleado)(*)(10 50)(si)) '(soleado 30 40 si))
;Salida: '(soleado 30 40 si +)
;Entrada: (interprete-CL '((soleado)(*)(10 50)(si)) '(lluvioso 30 40 si))
;Salida: '(lluvioso 30 40 si -)
;Entrada: (interprete-CL '((soleado)(*)(10 50)(si)) '(soleado 30 60 si))
;Salida: '(soleado 30 40 si -)
;Tener en consideración que, para este ejercicio, todos los ejemplos de
;entrenamiento son considerados como positivos.
#lang racket
(define (match-CL [c1 empty][c2 empty])
  (define output #t)
  (cond
    ((equal? (length c1) (length c2))
     (for ([i c1] [j c2])
       #:break (equal? output #f)
       (cond
         [(and (number? (car i)) (number? j))
          (cond
               [(not(>= (list-ref c2 1) (car(list-ref c1 2))))
                (set! output '#f)
                ]
               [(not(<= (list-ref c2 2) (car(cdr(list-ref c1 2)))))
                (set! output '#f)
                ]
           )
          ]
         [(equal?(symbol?(car i))(symbol? j))
          (cond
            [(not(equal? (car i) j))
             (set! output '#f)
             ]
            )
          ]
         [(equal? '* (car i))]
         [else (set! output '#f)]
         )
        )
     )(else (set! output '#f))
    )
    output
  )
(define (interprete-CL ejemplo-con-clase ejemplo-sin-clase)
  (define clase (match-CL ejemplo-con-clase ejemplo-sin-clase))
  (cond
    [(equal? clase '#t)
     (set! ejemplo-sin-clase (append ejemplo-sin-clase '(+)))
     ][else
       (set! ejemplo-sin-clase (append ejemplo-sin-clase '(-)))
       ]
    )
  (display ejemplo-sin-clase)
  )
