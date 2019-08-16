#lang racket

(provide (all-defined-out))

;;PROPUESTA 1 de grafo
(define pesoCaminos '((51 52 9)
                      (61 51 10)
                      (52 23 8)
                      (61 23 9)
                      (23 61 9)
                      (61 21 4)
                      (21 61 4)
                      (23 22 8)
                      (22 21 5)
                      (21 22 5)
                      (21 12 3)
                      (42 22 3)
                      (12 11 2)
                      (41 42 2)
                      (41 11 6)
                      (11 41 6)
                      (41 71 4)
                      (11 31 2)
                      (31 11 2)
                      (71 32 5)
                      (31 13 1)
                      (31 32 2)
                      (32 31 2)
                      (32 72 4)
                      (72 32 4)
                      (13 14 3)
                      (14 72 6)
                      (72 14 6)
                      (62 14 7)
                      (72 62 10)
                      ))

;;Grafo principal
(define grafo '( (51 ((52 9)))
                 (52 ((23 8)))
                 (23 ((61 9) (22 8)))
                 (61 ((23 9) (51 10) (21 4)))
                 (21 ((61 4) (22 5) (12 3)))
                 (22 ((21 5)))
                 (12 ((11 2)))
                 (11 ((41 6) (31 2)))
                 (41 ((11 6) (42 2) (71 4)))
                 (42 ((22 3)))
                 (71 ((32 5)))
                 (32 ((31 2) (72 4)))
                 (31 ((32 2) (11 2) (13 1)))
                 (13 ((14 3)))
                 (14 ((72 6)))
                 (62 ((14 7)))
                 (72 ((62 10) (14 6) (32 4)))
  ))

;;Grado precargado de prueba
(define gg '( (i (a b))
              (a (i c d))
              (b (i c d))
              (c (a x b))
              (d (a b f))
              (x (c))
              (f (d))
              ))

;;Indica si una ruta sin peso ha llegado al fin deseado
(define (solucion? fin ruta)
  (equal? fin (car ruta)))

;;Indica si una ruta con peso ha llegado al fin deseado
(define (solucion2? fin ruta)
  (equal? fin (caar ruta)))

;;Indica los vecinos de un nodo
(define (vecinos ele grafo)
       (vecinos-Aux (assoc ele grafo) ele grafo))

(define (vecinos-Aux resultado ele grafo)
  (cond ( (equal? resultado #f)
          #f)
        ( else
          (cadr resultado))))

;;Me indica si un nodo es parte de una ruta.
(define (miembro? ele lista)
  (cond ( (null? lista)
          #f)
        ( (equal? ele (car lista))
          #t)
        (else
         (miembro? ele (cdr lista)))))

;;Me indica si un nodo es parte de una ruta con peso, SIN considerar el peso.
;;Ejemplo: (miembro? (car '((72 0))) '((14 7) (62 10) (72 6))) #t
(define (miembro2? ele lista)
  (cond ( (null? lista)
          #f)
        ( (equal? (car ele) (caar lista))
          #t)
        (else
         (miembro2? ele (cdr lista)))))

;;Extiende una ruta hacia los nodos vecinos del grafo de prueba sin mostrar el peso de cada movimiento desde el inicio
;;Ejemplo (extender2 '(i) gg)
(define (extender2 ruta grafo)
  (extender2-Aux ruta '() grafo (vecinos (car ruta) grafo))
  )

(define (extender2-Aux ruta rutaGenerada grafo vecinos)
  (cond ((null? vecinos) rutaGenerada)
        (else
         (cond ((miembro? (car vecinos) ruta)
                (extender2-Aux ruta rutaGenerada grafo (cdr vecinos)))
               (else 
                     (extender2-Aux ruta (append (list (cons (car vecinos) ruta)) rutaGenerada) grafo (cdr vecinos))
  )))))


;;Extiende una ruta hacia los nodos vecinos sin mostrar el peso de cada movimiento desde el inicio
;;Ejemplo (extender3 '(51) grafo)
(define (extender3 ruta grafo)
  (extender3-Aux ruta '() grafo (vecinos (car ruta) grafo))
  )

(define (extender3-Aux ruta rutaGenerada grafo vecinos)
  (cond ((null? vecinos) rutaGenerada)
        (else
         (cond ((miembro? (caar vecinos) ruta)
                (extender3-Aux ruta rutaGenerada grafo (cdr vecinos)))
               (else 
                     (extender3-Aux ruta (append (list (cons (caar vecinos) ruta)) rutaGenerada) grafo (cdr vecinos))
  )))))


;;Extiende una ruta hacia los nodos vecinos mostrando el peso de cada movimiento desde el inicio
;;Ejemplo (extender4 '((51 0)) grafo)
(define (extender4 ruta grafo)
  (extender4-Aux ruta '() grafo (vecinos (caar ruta) grafo))
  )

(define (extender4-Aux ruta rutaGenerada grafo vecinos)
  (cond ((null? vecinos) rutaGenerada)
               (else
                (cond ((miembro2? (car vecinos) ruta)
                       (extender4-Aux ruta rutaGenerada grafo (cdr vecinos)))
                      (else 
                       (extender4-Aux ruta (append (list (cons (car vecinos)  ruta)) rutaGenerada) grafo (cdr vecinos))
                       )))))
        
;; Revierte los elementos de un conjunto de rutas
(define (reverseTotal rutasAux rutas)
  (cond ( (null? rutas)
          rutasAux)
        (else
         (reverseTotal 
                      (append (list (reverse (car rutas))) rutasAux)
                      (cdr rutas)
         ))))

;; buscar todas las rutas para el grafo de prueba gg
(define (buscaCaminosP ini fin grafo)
  (buscaCaminosAux (list (list ini)) fin grafo '()))

(define (buscaCaminosPAux rutas fin grafo total)
  (cond ( (null? rutas)
          (reverseTotal '() total))
        ( (solucion? fin (car rutas))
          (buscaCaminosPAux (cdr rutas)
                           fin
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaCaminosPAux (append (extender2 (car rutas) grafo)
                                   (cdr rutas))
                           fin
                           grafo
                           total))))


;;RETORNA LAS RUTAS DE UN PUNTO A OTRO SIN PESO
(define (buscaCaminos2 ini fin grafo)
  (buscaCaminosAux2 (list (list ini)) fin grafo '()))

(define (buscaCaminosAux2 rutas fin grafo total)
  (cond ( (null? rutas)
          (reverseTotal '() total))
        ( (solucion? fin (car rutas))
          (buscaCaminosAux2 (cdr rutas)
                           fin
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaCaminosAux2 (append (extender3 (car rutas) grafo)
                                   (cdr rutas))
                           fin
                           grafo
                           total))))

;;RETORNA LAS RUTAS ACOMPAÑADOS DE SUS PESOS POR TRASLADO
(define (buscaCaminos3 ini fin grafo)
  (buscaCaminosAux3 (list (list (list ini '0))) fin grafo '()))

(define (buscaCaminosAux3 rutas fin grafo total)
  (cond ( (null? rutas)
          (reverseTotal '() total))
        ( (solucion2? fin (car rutas))
          (buscaCaminosAux3 (cdr rutas)
                           fin
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaCaminosAux3 (append (extender4 (car rutas) grafo)
                                   (cdr rutas))
                           fin
                           grafo
                           total))))

;;RETORNA LISTA CON DISTANCIA TOTAL DE CADA RUTA
;;ejemplo: (distanciasTotalesRutas '(((72 0) (32 4) (31 2) (13 1) (14 3)) ((72 0) (14 6)) ((72 0) (62 10) (14 7))))
(define (distanciasTotalesRutas rutas)
  (distanciaTotalesRutasAux rutas '()))

(define (distanciaTotalesRutasAux rutas listaTotales)
  (cond ( (null? rutas)
          listaTotales)
        ( else
          (distanciaTotalesRutasAux (cdr rutas) (append listaTotales (list(distanciaTotalRuta 0 (car rutas))))))))

;;RETORNA DISTANCIA TOTAL DE RUTA
;;ejemplo (distanciaTotalRuta 0 '((72 0) (32 4) (31 2) (13 1) (14 3)))
(define (distanciaTotalRuta num ruta)
  (cond ( (null? ruta)
          num)
        (else
         (distanciaTotalRuta (+ num (cadar ruta)) (cdr ruta)))))

;;RETORNA EL ID DE LA MENOR LISTA
(define (menorLista lista)
  (menorListaAux lista (car lista) 0))

(define (menorListaAux lista num cont)
  (cond ( (null? lista)
          cont)
        (else
         (cond ( (<= num (car lista))
                 (menorListaAux (cdr lista) num cont))
               (else
                (menorListaAux (cdr lista) (car lista) (+ cont 1)))))))

;;RETORNA RUTA MAS CORTA
(define (buscaCaminoCorto ini fin grafo)
  (buscaCaminoCortoAux (buscaCaminos3 ini fin grafo) (buscaCaminos2 ini fin grafo))) 

(define (buscaCaminoCortoAux rutas rutasSinPeso)
  (buscaCaminoCortoAux2 (menorLista (distanciasTotalesRutas rutas)) rutas rutasSinPeso))

(define (buscaCaminoCortoAux2 num rutas rutasSinPeso)
  (cond ( (zero? num)
          (cons (car rutasSinPeso) (list (distanciaTotalRuta 0 (car rutas)))))
        ( else
          (buscaCaminoCortoAux2 (- num 1) (cdr rutas) (cdr rutasSinPeso)))))

;;RETORNA TODAS LAS RUTAS DE UN PUNTO A OTRO CON SU PESO TOTAL
(define (buscaCaminos ini fin grafo)
  (ordenarRutas (buscaCaminosAux (buscaCaminos2 ini fin grafo) (distanciasTotalesRutas (buscaCaminos3 ini fin grafo)) '()) '())
  )

(define (buscaCaminosAux rutas pesosPorRuta listaFinal)
  (cond ( (null? rutas)
          listaFinal)
        (else
         (buscaCaminosAux (cdr rutas) (cdr pesosPorRuta)  (append listaFinal (list (cons (car rutas) (list (car pesosPorRuta)))))))))

;;InsertaElementoOrdenado
(define (insertarElementoOrdenado ele lista)
  (cond ( (null? lista)
          (list ele))
        ( (> (cadr ele) (cadar lista))
          (cons (car lista)
                (insertarElementoOrdenado ele (cdr lista))))
        ( else
          (cons ele lista))))

;;Genera una lista con las rutas ordenadas
(define (ordenarRutas rutas rutasOrdenadas)
  (cond ( (null? rutas)
          rutasOrdenadas)
        ( else
          (ordenarRutas (cdr rutas) (insertarElementoOrdenado (car rutas) rutasOrdenadas)))))