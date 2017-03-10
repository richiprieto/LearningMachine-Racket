#|
Basándose en la Figura 1, implementar el algoritmo PCP. Para las
pruebas, utilizar el conjunto de ejemplos “ejemplosB3P.scm”, disponible en el
AVAC. Además, graficar el conjunto de ejemplos positivos, ejemplos negativos y
la hipótesis final LTU, en el software de su preferencia.

Conjunto de ejemplos

Peso   Altura   Clase
5      2        +
5      2.5      -
6      1.75     +
6.25   3        -
Nota: Para realizar este ejercicio utilizar la función (PRM H ISET ATTS) , que se
encuentra dentro del archivo “ejerciciosC3.zip”. Además, para generar un número
aleatorio entre 1 y -1, puede utilizar la siguiente línea de código:
(* 1 (- (* 2 (random)) 1))

Finalmente, tenga en consideración que el conjunto H debe tener un tamaño o
número de elementos, comprendidos entre 1 y -1, igual a:

(add1 (length ATTS))

En donde, el último elemento será el umbral o bias de cada hipótesis.
|#

#lang racket
;;Nota: la siguiente función sirve para leer datos de un ejemplo y asignar los mismos a una variable para su posterior
;;trabajo
(define ejemplos null)
(define (leer-ejemplos archivo)
  (set! ejemplos (with-input-from-file archivo read)
        )
  )

;******************************************************Traducir***********************************************************

;Función que permite pasar de un concepto LUU a un vector LUU pesos

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


;Codigo PRM***********
(define (PRM concepto-UU ejemplos atributos)
  (let ([pesos empty]
        [umbral 0]
        [ganancia 0.04]
        [instancias empty]
        [salida 0]
        [vectorTraducido empty])
    ;;(set! concepto-UU (append concepto-UU '((+))))
    ;;(set! pesos (last (nuevo-conceptoUU concepto-UU 1)))
    (set! pesos concepto-UU)
    ;;Se especifica el umbral inicial
    (set! umbral (* -1 (last pesos)))
    ;;Se asignan como instancias los ejemplos de entrenamiento
    (for ([j (length (cdr ejemplos))])
      (set! vectorTraducido empty)
      (for ([i (length (list-ref (cdr ejemplos) j))])
          (set! vectorTraducido (append vectorTraducido (list (traducir (list-ref (car ejemplos) i) (list-ref (list-ref (cdr ejemplos) j) i)))))
          )
      (set! instancias (append instancias (list vectorTraducido)))
      )
    (for ([i (length instancias)])
      ;;Para cada instancia se evalua la salida que se obtiene con los pesos del algoritmo
      (set! salida 0)
      (for ([j (- (length pesos) 1)])
        (set! salida (+ salida (* (list-ref pesos j) (list-ref (list-ref instancias i) j))))
        )
      (cond [(equal? (last (list-ref instancias i)) 0)
             (cond [(< (- salida umbral) 0)
                    ;;Reajuste de umbral
                    (set! umbral (+ umbral (* ganancia 1)))
                    (set! pesos (list-set pesos (- (length pesos) 1) (* umbral -1)))
                    ;;Reajuste de pesos
                    (for ([j (- (length pesos) 1)])
                      (set! pesos (list-set pesos j (+ (list-ref pesos j) (* (list-ref (list-ref instancias i) j) ganancia))))
                      )
                    ]
                   )
             ]
            [else
             (cond [(>= (- salida umbral) 0)
                    ;;Reajuste de umbral
                    (set! umbral (- umbral (* ganancia 1)))
                    (set! pesos (list-set pesos (- (length pesos) 1) (* umbral -1)))
                    ;;Reajuste de pesos
                    (for ([j (- (length pesos) 1)])
                      (set! pesos (list-set pesos j (- (list-ref pesos j) (* (list-ref (list-ref instancias i) j) ganancia))))
                      )
                    ]
                   )
             ]
            )
      )
    ;(display "Pesos del PRM\n")
    ;(display pesos)
    pesos
    )
  )

;Fin PRM**********************************

;;funcion para probar el ejercicio 30
(require plot)

(define graph-pos empty)
(define graph-neg empty)
(define varPRM empty)
(leer-ejemplos "ejemplosB3P.scm")
(set! varPRM(PRM '(1 -2 -1.5) ejemplos 2))
(for ([i (cdr ejemplos)])
  (cond
       [(equal? (list-tail i 2) '(+)) (set! graph-pos(append graph-pos (take i 2)))]
       [else (set! graph-neg(append graph-neg (take i 2)))]
       )  
  )

(plot
 (list
  (points (map vector (list(second graph-pos)(fourth graph-pos))(list(first graph-pos)(third graph-pos))) #:color 'red)
  (points (map vector (list(second graph-neg)(fourth graph-neg))(list(first graph-neg)(third graph-neg))) #:x-min '0 #:y-min '0 #:x-max '4 #:y-max '8 #:color 'blue)
  (function (λ (x) (/ (* (+ (* (second varPRM) x) (third varPRM)) -1) (first varPRM))) #:color 0 #:style 'dot #:label "PRM")
  )
 )


;********************Algoritmo PCP************************
(define (PCP ejemplospcp atributospcp)
  (let ([Hrandom empty]
        [count 50]
        [noerror #t]
        [valueControl empty]
        [auxmult1 empty]
        [auxmult2 empty]
        [auxBreak #f]
        )
    (for ([i (length (car ejemplospcp))])
      (set! Hrandom (append Hrandom (list(* 1 (- (* 2 (random)) 1)))))
      )
    ;**************Hacer mas eficiente el algoritmo colocando un while  o break*********
    ;************de esa forma detener repeticiones innecesarias****
    (for ([i count])
      (for([j (cdr ejemplospcp)])        
        (set! auxmult1(* (first j) (first Hrandom)))
        (set! auxmult2(* (second j) (second Hrandom)))
        (set! valueControl (+ auxmult1 (+ auxmult2 (third Hrandom)))) 
        
        (cond
          [(equal? (last j) '+)
           (cond
             [(positive? valueControl) (set! noerror #t)]
             [else(set! noerror #f)]
             )]
          [(equal? (last j) '-)
           (cond
             [(negative? valueControl) (set! noerror #t)]
             [else(set! noerror #f)]
             )]         
          )
        (cond
          [(equal? noerror #f) (set! Hrandom(PRM Hrandom ejemplos 2))]
          )
        ) 
      )
    
    (plot
     (list
      (points (map vector (list(second graph-pos)(fourth graph-pos))(list(first graph-pos)(third graph-pos))) #:color 'red)
      (points (map vector (list(second graph-neg)(fourth graph-neg))(list(first graph-neg)(third graph-neg))) #:x-min '0 #:y-min '0 #:x-max '4 #:y-max '8 #:color 'blue)
      (function (λ (x) (/ (* (+ (* (second Hrandom) x) (third Hrandom)) -1) (first Hrandom))) #:color 0 #:style 'dot #:label "PCP")
      )
     )
    ;(display Hrandom)
    )
  )
(PCP ejemplos 2)

;(display (cdr ejemplos))
