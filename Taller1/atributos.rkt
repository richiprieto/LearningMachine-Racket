;Construya la función atributo que admita 2 parámetros: el primero
;será el nombre del atributo pedido y el segundo la lista de ejemplos. La función
;debe leer la lista de ejemplos y devolver una nueva lista con todos los valores
;que posee el atributo ingresado. Por ejemplo, tomando como lista de ejemplos
;el archivo “ejemplosB2.scm”, una llamada a ejercicio con el parámetro
;perspectiva como atributo devolvería lo siguiente:
;Entrada: (atributo 'perspectiva "ejemplos.scm")
;Salida: ‘(nublado soleado lluvioso)
#lang racket
(define (atributo [nombreAtrib empty][lista empty])
  (define listaExtraer (call-with-input-file lista
                            read))
  
  (for ([i (car listaExtraer)])
    (cond
        ((equal? (car i) nombreAtrib )
         (display (car(cdr i)))
         (display "\n"))
    )
  )
)
