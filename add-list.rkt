;Need ejemplosB2.scm
;Construya la función anadir-ejemplo que tenga como parámetros
;una lista de ejemplos y un nuevo ejemplo. Esta función debe añadir el ejemplo
;nuevo a la lista de ejemplos.
;Entrada: (anadir-ejemplos "ejemplosB2.scm" '((soleado 15 10 si -)))
;Salida: '(((perspectiva (nublado soleado lluvioso))
;         (temperatura (15 30))
;         (humedad (30 85))
;         (viento (si no))
;         (clase (+ -)))
;         (soleado 25 79 si +)
;         (soleado 25 75 si +)
;         (lluvioso 20 21 no -)
;         (lluvioso 35 20 no -)
;         (soleado 20 79 si +)
;         (soleado 16 75 si +)
;         (soleado 28 40 si +)
;         (lluvioso 25 13 no -)
;         (nublado 17 31 si +)
;         (soleado 23 31 si +)
;         (lluvioso 12 90 no -)
;         (soleado 17 79 si +)
;         (soleado 29 35 si +)
;         (soleado 27 57 si +)
;         (lluvioso 9 90 no -)
;         (lluvioso 13 25 no -)
;         (soleado 15 10 si -))
#lang racket
(define (anadirEjemplos [direccion empty][agregar empty])
  (define listaOriginal (call-with-input-file direccion
                            read))
  (set! listaOriginal (append listaOriginal agregar))
  listaOriginal
  )
