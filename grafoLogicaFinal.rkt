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
(define grafo '( (51 ())
                 (52 ((23 8) (51 9)))
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

;;GrafoDinamico
(define grafoDinamico '()
  )

;; Agregar ciudades para grafoDinamico
;; E: #id ciudad, grafo mixto
;; S: grafo mixto con la ciudad añadida
;; R: # ciudad vacío, literales.
;;> (agregarCiudad 3 '())
;;'((3 ()))
(define (agregarCiudad id grafo)
  (cond ( (null? grafo) 
          (list (append (list id) (list grafo)))) ;;si el grafo está vacío, añada la ciudad
        ( else
           (append grafo (list (list id '()))));; sino, agregue la ciudad al grafo      
 ))

;; Agregar caminos entre ciudades
;;agregarCiudad
;;Propósito: Agrega una ciudad (ID) al grafo mixto.
;;Parámetros de entrada:
;;id: identificador de ciudad
;;grafo: grafo mixto al que agregar ciudad
;;Salida: grafo mixto con ciudad agregada.

(define (agregarCamino idIni idFin peso grafo)
  (agregarCaminoAux idIni idFin peso grafo '()))

(define (agregarCaminoAux idIni idFin peso grafo grafoFinal)
  (cond ( (null? grafo) ;;Condición base: Cuando ya se terminó de recorrer el grafo, finalice
          grafoFinal) ;;retorne el nuevo grafo generado 'grafoFinal'.
        ( else
          (cond ( (equal? idIni (caar grafo)) ;;valida si está recorriendo la ciudad de origen en el grafo
                  (agregarCaminoAux idIni idFin peso (cdr grafo) ;;recorre la función nuevamente ahora sin el primer elemnto en el grafo
                                    (append grafoFinal ;;grafoFinal ahora es la unión del grafo en formación +
                                            (list (append (list(caar grafo)) ;;la ciudad de origen +
                                                          (list (modificarConexionesCiudad (list idFin peso) (cadar grafo)))))))) ;;conexiones de la ciudad incluyendo el nuevo camino
                (else
                 (agregarCaminoAux idIni idFin peso (cdr grafo) ;;recorre la función nuevamente ahora sin el primer elemnto en el grafo
                                   (append grafoFinal ;;grafoFinal ahora es la unión del grafo en formación +
                                           (list(car grafo))) ;;el primer elemento en el grafo en desintegración.
                 )
                )
            )
          )
   )
)


;; Agrega un nodo a la lista de conexiones de una ciudad
;; E: id: nodo compuesto por ciudad y peso.
;; S: lista de nodos conexiones
;; R: nodos vacíos
(define (modificarConexionesCiudad id lista)
  (modificarConexionesCiudadAux id lista))

(define (modificarConexionesCiudadAux id lista)
  (cond ( (null? lista);;si la lista esta vacía
          (list (append lista id))) ;;agrege el nodo a la lista
        ( else ;;sino
           (append lista (list id)));; una la actual lista de conexiones con el nodo   
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

;;Propósito: Indica si una ruta sin peso ha llegado al fin deseado.
;;Parámetros de entrada:
;;fin: id de la ciudad de destino.
;;ruta: ruta - lista de IDs de ciudades.
;;Salida: boolean, donde true (#t) comprueba que la ruta finaliza con el elemento fin, y (#f) que la ruta termina con cualquier otro elemento.
;;Indica si una ruta sin peso ha llegado al fin deseado
(define (solucion? fin ruta)
  (equal? fin (car ruta)))

;;Propósito: Indica si una ruta sin peso ha llegado al fin deseado.
;;Parámetros de entrada:
;;fin: id de la ciudad de destino.
;;ruta: ruta - lista de IDs de ciudades.
;;Salida: boolean, donde true (#t) comprueba que la ruta finaliza con el elemento fin, y (#f) que la ruta termina con cualquier otro elemento.
;;Indica si una ruta con peso ha llegado al fin deseado
(define (solucion2? fin ruta)
  (equal? fin (caar ruta)))

;;Indica los vecinos de un nodo
;;Propósito: Indica los vecinos de un nodo.
;;Parámetros de entrada:
;;ele: id de la ciudad de destino.
;;grafo: ruta - lista de IDs de ciudades.
;;Salida: boolean, donde true (#t) comprueba que la ruta finaliza con el elemento fin, y (#f) que la ruta termina con cualquier otro elemento.
(define (vecinos ele grafo)
       (vecinos-Aux (assoc ele grafo) ele grafo))

(define (vecinos-Aux resultado ele grafo)
  (cond ( (equal? resultado #f)
          #f)
        ( else
          (cond ( (null? (cdr resultado))
                  (cdr resultado))
                (else
                 (cadr resultado))))))

;;miembro?
;;Propósito: Indica si un nodo es parte de una ruta.
;;Parámetros de entrada:
;;ele: id de la ciudad de destino.
;;lista: lista con ids de las ciudades que componen la ruta.
;;Salida: boolean, donde true (#t) comprueba que el elemento es parte de la lista.
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
;;Propósito: Me indica si un nodo es parte de una ruta con peso, SIN considerar el peso.
;;Parámetros de entrada:
;;ele: id de la ciudad de destino.
;;lista: lista con ids de las ciudades que componen la ruta.
;;Salida: boolean, donde true (#t) comprueba que el elemento es parte de la lista.
(define (miembro2? ele lista)
  (cond ( (null? lista)
          #f)
        ( (equal? (car ele) (caar lista))
          #t)
        (else
         (miembro2? ele (cdr lista)))))

;;Extiende una ruta hacia los nodos vecinos del grafo de prueba sin mostrar el peso de cada movimiento desde el inicio
;;Ejemplo (extender2 '(i) gg)
;;Propósito: extiende una ruta hacia los nodos vecinos del grafo de prueba sin mostrar el peso de cada movimiento desde el inicio.
;;Parámetros de entrada:
;;ruta: ruta de ids de ciudades.
;;grafo: grafo mixto compuesto por las ciudades y sus conexiones.
;;Salida: ruta(s) extendida hacia los nodos vecinos o bien una lista vacia en caso que la ruta no se pueda extender.
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
;;Propósito: Revierte los elementos de un conjunto de rutas.
;;Parámetros de entrada:
;;rutaAux: lista en el que se guardara las rutas invertidas.
;;ruta: rutas a invertir.
;;Salida: rutas invertidas.
(define (reverseTotal rutasAux rutas)
  (cond ( (null? rutas)
          rutasAux)
        (else
         (reverseTotal 
                      (append (list (reverse (car rutas))) rutasAux)
                      (cdr rutas)
         ))))

;; buscar todas las rutas para el grafo de prueba gg
;;Propósito: busca todas las rutas de un punto a otro para el grafo de prueba gg.
;;Parámetros de entrada:
;;ini: ID de ciudad de origen.
;;fin: ID de ciudad de destino.
;;grafo: grafo mixto con ciudades y conexiones.
;;Salida: lista de listas con rutas de un punto a otro.

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
;;Propósito: Busca las rutas de un punto a otro sin peso.
;;Parámetros de entrada:
;;ini: ID de ciudad de origen.
;;fin: ID de ciudad de destino.
;;grafo: grafo mixto con ciudades y conexiones.
;;Salida: lista de listas con rutas de un punto a otro.

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
;;Propósito: Busca las rutas de un punto a otro con su peso de traslado.;;
;;Parámetros de entrada:
;;ini: ID de ciudad de origen.
;;fin: ID de ciudad de destino.
;;grafo: grafo mixto con ciudades y conexiones.
;;Salida: lista de listas con rutas de un punto a otro con su peso de traslado.
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
;;Propósito: determina la posicion en una lista de rutas, donde se encuentra el elemento que representa (la ruta) con distancia más corta.
;;Parámetros de entrada:
;;lista: lista con distancias totales de rutas.
;;Salida: posicion (numero, de 0 indicando posición inicial, hasta n-1, con 'n' como el numero de elementos en la lista).
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
;;Propósito: retorna la ruta (lista con ids de ciudades) más corta entre dos puntos seleccionados en un grafo mixto, junto con el peso total de dicha ruta.
;;Parámetros de entrada:
;;ini: id de ciudad que representa el origen de la ruta.
;;fin: id de ciudad de destino.
;;grafo: grafo mixto que contiene id ciudadesy sus conexiones entre sí.
;;Salida: lista con ids de ciudades indicando la ruta más corta por la que puede pasar, junto con el peso total de la ruta.
(define (buscaCaminoCorto ini fin grafo)
  (buscaCaminoCortoAux (buscaCaminos3 ini fin grafo) (buscaCaminos2 ini fin grafo))) 

(define (buscaCaminoCortoAux rutas rutasSinPeso)
  (cond ( (null? rutas)
          rutas)
        ( else
          (buscaCaminoCortoAux2 (menorLista (distanciasTotalesRutas rutas)) rutas rutasSinPeso))))

(define (buscaCaminoCortoAux2 num rutas rutasSinPeso)
  (cond ( (zero? num)
          (cons (car rutasSinPeso) (list (distanciaTotalRuta 0 (car rutas)))))
        ( else
          (buscaCaminoCortoAux2 (- num 1) (cdr rutas) (cdr rutasSinPeso)))))

;;RETORNA TODAS LAS RUTAS DE UN PUNTO A OTRO CON SU PESO TOTAL
;;Propósito: retorna las rutas entre dos puntos seleccionados en un grafo mixto, junto con el peso total de cada ruta contiguo a esta.
;;Parámetros de entrada:
;;ini: id de ciudad que representa el origen de la ruta.
;;fin: id de ciudad de destino.
;;grafo: grafo mixto que contiene id ciudadesy sus conexiones entre sí.
;;Salida: lista rutas por las que se puede llegar desde ini a fin, junto con el peso total de cada ruta.
(define (buscaCaminos ini fin grafo)
  (ordenarRutas (buscaCaminosAux (buscaCaminos2 ini fin grafo) (distanciasTotalesRutas (buscaCaminos3 ini fin grafo)) '()) '())
  )

(define (buscaCaminosAux rutas pesosPorRuta listaFinal)
  (cond ( (null? rutas)
          listaFinal)
        (else
         (buscaCaminosAux (cdr rutas) (cdr pesosPorRuta)  (append listaFinal (list (cons (car rutas) (list (car pesosPorRuta)))))))))

;;InsertaElementoOrdenado
;;Propósito: insertar de manera creciente ids de rutas (con peso) en una lista, de acuerdo al id de la ciudad y no el peso.
;;Parámetros de entrada:
;;ele: elemento a ingresar.
;;lista: lista de elementos (generalmente rutas).
;;Salida: lista con elementos ordenados.
(define (insertarElementoOrdenado ele lista)
  (cond ( (null? lista)
          (list ele))
        ( (> (cadr ele) (cadar lista))
          (cons (car lista)
                (insertarElementoOrdenado ele (cdr lista))))
        ( else
          (cons ele lista))))

;;Genera una lista con las rutas ordenadas
;;Propósito: tomar un conjunto de rutas acompañadas por sus pesos (generadas por buscaCaminos) y ordenarlas de manera creciente, según el peso total.
;;Parámetros de entrada:
;;rutas: rutas sin peso acompañadas del peso total por ruta.
;;rutasOrdenadas: lista en la que se van a ir insertando rutas ordenadas por peso.
;;Salida: lista con rutas ordenadas por peso de ruta.
(define (ordenarRutas rutas rutasOrdenadas)
  (cond ( (null? rutas)
          rutasOrdenadas)
        ( else
          (ordenarRutas (cdr rutas) (insertarElementoOrdenado (car rutas) rutasOrdenadas)))))