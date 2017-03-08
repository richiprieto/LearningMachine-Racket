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


;***************************************************************************************************************************
;******************************************************Código PRM***********************************************************
;***************************************************************************************************************************

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
    pesos
    )
  )

;***************************************************************************************************************************
;******************************************************Fin PRM***********************************************************
;***************************************************************************************************************************


;;funcion para probar el ejercicio 30
(leer-ejemplos "ejemplosB2P.scm")
(PRM '(1 -2 -1.5) ejemplos 2)