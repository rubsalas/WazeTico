#lang racket
(require racket/gui)
(require racket/draw/arrow)
(require "grafoLogica.rkt")





;-------------------------------------------Variables-------------------------------------------;


;GrafoDinamico
(define node-map '() )

;Lista de Coordenadas
(define coords-list '() )


;Ciudad actual por agregar
(define actual-new-city -1)

;Posicion en x de la ciudad por agregar
(define actual-new-city-x-pos 0)

;Posicion en y de la ciudad por agregar
(define actual-new-city-y-pos 0)


;Ciudad inicial donde sale el camino nuevo
(define actual-initial-city -1)

;Ciudad final donde llega el camino nuevo
(define actual-final-city -1)

;Peso del nuevo camino
(define actual-weight -1)


;Destino inicial de la ruta
(define initial-destiny "")
 
;Destino final de la ruta
(define final-destiny "")


;Lista de Nodos con coordenadas
(define nodes-list '(
        (11 442 279) (12 343 309) (13 510 339) (14 638 378)
        (21 393 219) (22 432 104) (23 265 40)
        (31 589 279) (32 687 249)
        (41 521 169) (42 540 90)
        (51 88 189) (52 137 70)
        (61 274 189) (62 736 428)
        (71 678 159) 
     )
  )


;Variable de estado de seleccion de rutas
(define path-selection -1)

;Estado de seleccion de rutas
(define search-state "")


;Lista con el camino mas corto y su peso
(define shortest-path '() )

;Lista con todos los caminos y sus pesos
(define all-paths '() )

;Cantidad de caminos por ruta actual
(define path-num 0)


;Lista con botones actuales
(define actual-buttons '() )


;Pens
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define one-way-pen (make-object pen% "GAINSBORO" 4 'solid))
(define arrow-pen (make-object pen% "GAINSBORO" 1 'solid))
(define two-way-pen (make-object pen% "DIM GRAY" 4 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))
(define red-pen (make-object pen% "RED" 4 'solid))
(define weight-pen (make-object pen% "BLACK" 1 'solid))
(define white-pen (make-object pen% "SNOW" 1 'solid))


;Brushes
(define indian-red-brush (make-object brush% "INDIAN RED" 'solid)) ;0
(define chocolate-brush (make-object brush% "CHOCOLATE" 'solid)) ;1
(define salmon-brush (make-object brush% "SALMON" 'solid)) ;2
(define green-brush (make-object brush% "GREEN" 'solid)) ;3
(define yellow-green-brush (make-object brush% "YELLOW GREEN" 'solid)) ;4
(define aquamarine-brush (make-object brush% "AQUAMARINE" 'solid)) ;5
(define medium-turquoise-brush (make-object brush% "MEDIUM TURQUOISE" 'solid)) ;6
(define lightblue-brush (make-object brush% "LIGHTBLUE" 'solid)) ;7
(define medium-slate-blue-brush (make-object brush% "MEDIUM SLATE BLUE" 'solid)) ;8
(define plum-brush (make-object brush% "PLUM" 'solid)) ;9
(define white-brush (make-object brush% "WHITE" 'solid))
(define arrow-brush (make-object brush% "GAINSBORO" 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define black-hilite-brush (make-object brush% "BLACK" 'hilite))



;-------------------------------------------Variables-------------------------------------------;






;-------------------------------------------Funciones-------------------------------------------;


;;Funciones iniciales

;Funcion para encontrar la coordenada deseada de un nodo
;Parametros:
;   nodo: numero de ciudad
;   pos: coordenada ("x" o "y")
(define (get-coord nodo pos)
  (get-coord-aux nodo pos coords-list)
    )

;Funcion auxiliar para encontrar la coordenada deseada de un nodo
;Parametros:
;   nodo: numero de ciudad
;   pos: coordenada ("x" o "y")
;   list: nodes-list
(define (get-coord-aux nodo pos list)
  (cond( (null? list)
         -1)
       ( (equal? nodo (caar list))
         (cond( (equal? pos "x")
                (cadar list)
               )
              ( (equal? pos "y")
                (caddar list)
               )
              (else
               -1)
           )
        )
       (else
        (get-coord-aux nodo pos (cdr list))
        )
   )
)


;-------------------------------------------Funciones-------------------------------------------;





;-------------------------------------Referencias en Canvas-------------------------------------;


(define (draw-grid)
  (set-references-aux 0 50 800 500)
  )

(define (set-references-aux n in-between until-x until-y)
  (cond ( (and #t (<= until-x n) (<= until-y n) )
          #f
         )
        (else
         (send dc set-pen weight-pen)
         (send dc set-brush black-hilite-brush)
         (send dc draw-line n 0 n until-y)
         
         (send dc draw-line 0 n until-x n)

         
         (set-references-aux (+ n in-between) in-between until-x until-y)
         (cond ( (equal? 0 (modulo n 150))
                 ;Dibuja el texto
                 (send dc draw-text (number->string n) 0 n)
                 (send dc draw-text (number->string n) n 0)
                )
               )
         )
    
    )

  )

;-------------------------------------Referencias en Canvas-------------------------------------;





;----------------------------------------Graficar Nodos----------------------------------------;

#|
;Funcion para dibujar todos los nodos al iniciar la aplicacion
;Llama a una funcion auxiliar
(define (draw-all-nodes)
  (draw-all-nodes-aux nodes-list dc)
  )


;Funcion auxiliar para dibujar todos los nodos al iniciar la aplicacion
;Parametros:
;   list: nodes-list
;   dc: Drawing Context
(define (draw-all-nodes-aux list dc)
  (cond ( (null? (cdr list) )
              (draw-node dc (caar list) (cadar list) (caddar list) )
           )
        (else
             (draw-node dc (caar list) (cadar list) (caddar list) )
             (draw-all-nodes-aux (cdr list) dc)
         )
  )
)


;Funcion para dibujar un nodo
;Parametros:
;   dc: Drawing Context
;   node: numero de ciudad
;   x: coordenada en x
;   y: coordenada en y
(define (draw-node dc node x y)

  (cond ( (equal? (quotient node 10) 1)
          (send dc set-brush green-brush)
         )
        ( (equal? (quotient node 10) 2)
          (send dc set-brush yellow-green-brush)
         )
        ( (equal? (quotient node 10) 3)
          (send dc set-brush salmon-brush)
         )
        ( (equal? (quotient node 10) 4)
          (send dc set-brush aquamarine-brush)
         )
        ( (equal? (quotient node 10) 5)
          (send dc set-brush lightblue-brush)
         )
        ( (equal? (quotient node 10) 6)
          (send dc set-brush chocolate-brush)
         )
        ( (equal? (quotient node 10) 7)
          (send dc set-brush medium-turquoise-brush)
         )
        (else
         (send dc set-brush black-brush)
         )
    )

  ;Dibuja el nodo (circulo)
  (send dc set-pen black-pen)
  (send dc draw-ellipse x y 30 30)

  ;Muestra el respectivo numero dentro del nodo
  (send dc set-pen white-pen)
  (send dc draw-text (number->string node) (+ x 5) (+ y 5) )
 
)
|#



;----------------------------------------Graficar Nodos----------------------------------------;





;---------------------------------------Graficar Lineas---------------------------------------;

#|
;Funcion para dibujar todas las lineas (inicio de aplicacion)
(define (draw-all-lines)
  (draw-all-lines-aux grafo)
  )

;Funcion auxiliar para dibujar todas las lineas (inicio de aplicacion)
;Parametros:
;   graph: graph
(define (draw-all-lines-aux graph)
  (cond ( (null? (cdr graph) )
          (draw-lines-by-node dc (caar graph) (cadar graph))
         )
        (else         
         (draw-lines-by-node dc (caar graph) (cadar graph))
         (draw-all-lines-aux (cdr graph) ) ;Elimina el primer nodo del grafo
         )
    )
  )


;Funcion para dibujar lineas de un nodo especifico
;Parametros:
;   dc: Drawing Context
;   node: nodo inicial
;   connections: lista de nodos que finalizan la linea
(define (draw-lines-by-node dc node connections)
  (cond ( (null? (cdr connections) )
          (draw-line dc node (caar connections) (check-ways (caar connections) node) )
         )
        (else
         (draw-line dc node (caar connections) (check-ways (caar connections) node) )
         (draw-lines-by-node dc node (cdr connections) )
         )
    )
  )


;Funcion para dibujar una linea
;Parametros:
;   ini: nodo inicial
;   fin: nodo final
;   way: cantidad de vias (1 o 2)
(define (draw-line dc ini fin way)
  (cond ( (equal? way 1)
          ;Dibuja solo las flechas de los caminos de una sola via
          (draw-arrows (+ 15 (get-coord ini "x")) (+ 15 (get-coord ini "y"))
              (+ 15 (get-coord fin "x")) (+ 15 (get-coord fin "y")) )
          ;Cambia al pen para una via
          (send dc set-pen one-way-pen)
         )
        ( (equal? way 2)
          ;Cambia al pen para dos vias
          (send dc set-pen two-way-pen)
         )
    )
  ;Manda a dibujar la linea
  (send dc draw-line
        (+ 15 (get-coord ini "x")) (+ 15 (get-coord ini "y"))
        (+ 15 (get-coord fin "x")) (+ 15 (get-coord fin "y")) )
        ; + 10 ya que el nodo tiene radio de 20 -> asi la linea queda en el medio
  )


;Funcion para verificar si el camino es one-way o two-way
;Parametros:
;   node: nodo por verificar
;   fin: nodo por encontrar camino
(define (check-ways node fin)
  (check-ways-aux node fin grafo)
  )

;Funcion auxiliar para verificar si el camino es one-way o two-way
;   node: nodo por verificar
;   fin: nodo por encontrar camino
;   graph: graph
(define (check-ways-aux node fin graph)
  (cond ( (null? graph)
          1
         )
        ( (equal? node (caar graph))
          (cond ( (path? fin (cadar graph) )
                  2
                 )
                (else
                 (check-ways-aux node fin (cdr graph) )
                 )
                )
         )
        (else
         (check-ways-aux node fin (cdr graph) )
         )
  )
  )

;Funcion para verificar si existe un path a la inversa del ya encontrado
;Parametros:
;   fin: nodo final que pasa a ser inicial
;   connections: lista de nodos que finalizarian la linea
(define (path? fin connections)
  (cond ( (null? connections)
          #f
         )
        ( (equal? fin (caar connections) )
          #t
          )
        (else
         (path? fin (cdr connections) )
         )
    )
 )


;Funcion para dibujar una flecha cerca de la linea
;Parametros:
;   x1:
;   y1:
;   x2:
;   y2:
(define (draw-arrows x1 y1 x2 y2)
  (send dc set-pen arrow-pen)
  (send dc set-brush arrow-brush)

         (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 6 7)
         
  )
|#

;---------------------------------------Graficar Lineas---------------------------------------;





;----------------------------------------Graficar Pesos----------------------------------------;


;Funcion para graficar el peso del camino entre dos nodos
(define (draw-all-weights)
  (draw-all-weights-aux pesoCaminos)
  )

;Funcion auxiliar para graficar el peso del camino entre dos nodos
(define (draw-all-weights-aux peso-caminos)
  (cond ( (null? peso-caminos)
          #t
         )
        (else
         ;Envia a dibujar el peso actual
         (draw-weight
          (+ 15 (get-coord (caar peso-caminos) "x"))
          (+ 15 (get-coord (caar peso-caminos) "y"))
          (+ 15 (get-coord (cadar peso-caminos) "x"))
          (+ 15 (get-coord (cadar peso-caminos) "y"))
          (caddar peso-caminos) )
         ;Llama a la funcion nuevamente recursivamente para recorrer toda la lista
         (draw-all-weights-aux (cdr peso-caminos) )
         )
    )
  )


;Funcion para mostrar el peso de la linea cerca de esta
(define (draw-weight x1 y1 x2 y2 w)
  
  (cond
        ( (>= 70 (abs (- x1 x2))  )
          ;Sumar solo en x
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 10)
                ( + (/ (+ y1 y2) 2) 0)
                )
         )
        ( (>= 70 (abs (- y1 y2))  )
          ;Sumar solo en y
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 0)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        (else
         ;Suma en ambos
         ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 6)
                ( + (/ (+ y1 y2) 2) 6)
                )
         )
    )

  )


;----------------------------------------Graficar Pesos----------------------------------------;





;----------------------------------------Agregar Ciudad----------------------------------------;


;Funcion para agregar una nueva ciudad al mapa
(define (add-city)

  ;Se ingresa el texto del field a la varible
  (set! actual-new-city (send add-city-text-field get-value))
  (set! actual-new-city-x-pos (send add-city-x-text-field get-value))
  (set! actual-new-city-y-pos (send add-city-y-text-field get-value))
  

  (cond ( (equal? #f (check-city actual-new-city))
          ;Si se encuentra que la ciudad no puede ser agregada
          ;Se manda un mensaje de error dentro de la funcion llamada
          #f
         )
        (else

         (cond ( (equal? #f (check-city-coords actual-new-city-x-pos actual-new-city-y-pos))
                 ;Si se encuentra que la ciudad no puede ser agregada
                 ;Se manda un mensaje de error dentro de la funcion llamada
                 #f
                )
               (else
                ;Se agrega el nuevo nodo a grafoDinamico
                (set! node-map (agregarCiudad (string->number actual-new-city) node-map))
                ;Se agrega el nodo y sus posiciones a coords-list
                (set! coords-list (set-city-position (string->number actual-new-city)
                                   (string->number actual-new-city-x-pos)
                                   (string->number actual-new-city-y-pos)))
                
                (send instructions-text-field set-value (string-append "AGREGADO:\nNode: " actual-new-city
                                                                       "Pos x: " actual-new-city-x-pos
                                                                       "Pos y: " actual-new-city-y-pos
                                                                       ))
                ;Para graficar todos los nodos en la lista
                (draw-all-nodes)

                ;Verifica la cantidad de ciudades actuales en grafo para activar botones y funciones
                (check-city-quantity)
                )
           )
         )
    )
  )


;Funcion para verificar si la ciudad puede ser agregada al mapa
(define (check-city city)
  (check-city-aux city node-map)
  )

;Funcion auxiliar para verificar si la ciudad puede ser agregada al mapa
(define (check-city-aux city graph)
  ;Verifica que la ciudad ingresada sea una ciudad valida
  (cond ( (equal? #t (check-city-node city) )
          ;Verifica que la ciudad ingresada sea una ciudad que no exista ya
          (cond ( (equal? #t (check-city-existence city graph) )
                  #t
                  )
                (else
                 (send instructions-text-field set-value
                       "La ciudad deseada ya se encuentra en al mapa.\nIngrese una ciudad no existente." )
                 #f
                 )
             )
          )
        (else
         (send instructions-text-field set-value
               "La ciudad deseada no puede ser agregada al mapa.\nIngrese una ciudad entre 0 y 99." )
         #f
         )
    )
  )
  


;Funcion para verificar que la ciudad tenga el formato deseado (0 a 99)
(define (check-city-node city)
  (check-city-node-aux city 99)
  )

;Funcion auxiliar para verificar que la ciudad tenga el formato deseado (0 a 99)
(define (check-city-node-aux city n)
  (cond ( (equal? n -1)
          #f
         )
        ( (equal? city (number->string n))
          #t
         )
        (else
         (check-city-node-aux city (- n 1))
         )
    )
  )

;Funcion para verificar que el nodo existe en grafoDinamico
(define (check-city-existence city graph)
  (cond ( (null? graph)
          #t
         )
        ( (equal? (string->number city) (caar graph) )
          #t
         )
        (else
         (check-city-existence city (cdr graph) )
         )
    )
  )

;Funcion para verificar que las coordenadas sea numeros dentro del canvas
(define (check-city-coords x y)
  (check-city-coords-aux x y 773 483)
  )


;Funcion auxiliar para verificar que las coordenadas sea numeros dentro del canvas
(define (check-city-coords-aux x y dx dy)
  (cond ( (and #t (check-coord x dx) (check-coord y dy) )
          #t
         )
        (else
         (send instructions-text-field set-value
                       "->Las coordenadas ingresadas no pertenecen dentro\n    del mapa.
->Ingrese una coordenada en 'x' de 17 hasta 773
    y una coordenada en 'y' de 17 hasta 483." )
                 #f
                 )
         )
  )


;Verifica una sola coordenada por separado
(define (check-coord z dz)
  (cond ( (equal? 16 dz)
          #f
         )
        ( (equal? z (number->string dz))
         #t
         )
        (else
         (check-coord z (- dz 1))
         )
   )
  )

;Funcion para agregar el nodo con sus respectivas coordenadas a coords-list
;Ya se encuentran verificados los parametros
(define (set-city-position city x y)
  (append coords-list (list (list city x y)) )
  )

;Funcion para verificar la cantidad de nodos agregados
;Dependiendo de esto dependen otras funcionalidades del programa
(define (check-city-quantity)
  (cond ( (<= 6 (length node-map))
          #t
         )
        ( (equal? 5 (length node-map))
          ;Habilita el boton de busqueda y los radio-buttons
          (send initial-text-field enable #t)
          (send final-text-field enable #t)
          (send search-button enable #t)
          (send rbuttons enable #t)
          (send new-search-button enable #t)
         )
        ( (equal? 2 (length node-map))
          (send add-road-button enable #t)
         )
    )
  )

;----------------------------------------Agregar Ciudad----------------------------------------;





;----------------------------------------Agregar Camino----------------------------------------;


;Funcion para agregar un camino entre dos ciudades en el mapa
(define (add-road)

  ;Se ingresa el texto del field a la varible
  (set! actual-initial-city (send add-road-initial-text-field get-value))
  (set! actual-final-city (send add-road-final-text-field get-value))
  (set! actual-weight (send add-road-weight-text-field get-value))

  (cond ( (equal? #f (check-road actual-initial-city actual-final-city))
          ;Si se encuentra que los destinos no se encuentran en el mapa
          ;Se manda un mensaje de error dentro de la funcion llamada
          #f
         )
        (else
         (cond ( (equal? #f (check-weight actual-weight))
                 ;Si se encuentra que el peso no es adecuado al formato
                 ;Se manda un mensaje de error dentro de la funcion llamada
                 #f
                 )
               (else
                (cond ( (equal? #f (check-road-existence actual-initial-city actual-final-city actual-weight))
                        ;Si se encuentra que el camino se repite
                        ;Se mantiene el camino y se sobreescribe el peso
                        ;Se manda un mensaje de confirmacion dentro de la funcion llamada
                        #f
                        )
                      (else
                       #t
                       ;Verificar que el camino no se repita
                       ;O que si se repite solo se cambie el peso
                       (set! node-map (agregarCamino
                              (string->number actual-initial-city)
                              (string->number actual-final-city)
                              (string->number actual-weight) node-map))
                       ;Graficacion de caminos
                       (draw-all-lines)
                       ;Se grafican de nuevos los nodos para manener las lineas por debajo
                       (draw-all-nodes)
                       )
                    )
                )
             )
         )
    )
  )

;Funcion que verifica la validez del camino por agregar
(define (check-road i-city f-city)
  (cond ( (equal? #f (check-road-aux i-city node-map) )
        ;Cond inicial
          (send instructions-text-field set-value
              "La ciudad inicial seleccionada no existe.\nIngrese una nueva ciudad." )
          #f )
        ( (equal? #f (check-road-aux f-city node-map) )
        ;Cond final
          (send instructions-text-field set-value
              "La ciudad final seleccionada no existe.\nIngrese una nueva ciudad." )
          #f )
        (else
         #t )
     )
  )

;Funcion auxiliar que verifica la validez del camino por agregar
(define (check-road-aux city graph)
  (cond ( (null? graph)
        #f
        )
        ( (equal? city (number->string (caar graph)))
          #t
         )
        (else
         (check-road-aux city (cdr graph) )
         )
      )
  )


;Funcion para verificar que el peso del camino se apegue al formato de estos (1-20)
(define (check-weight weight)
  (cond ( (equal? #f (check-weight-aux weight 20) )
          ;Si el peso esta mal
          (send instructions-text-field set-value
              "La distancia deseada no se encuentra dentro del rango\nde 0 y 20.\nIngrese una nueva distancia." )
          #f )
        (else
         ;Si el peso es adecuado para el mapa
         #t )
     )
  )


;Funcion auxiliar para verificar que el peso del camino se apegue al formato de estos (1-20)
(define (check-weight-aux weight max)
  (cond ( (equal? 0 max)
          #f
         )
        ( (equal? weight (number->string max))
         #t
         )
        (else
         (check-weight-aux weight (- max 1) )
         )
    )
  )


;Funcion para verificar la existencia del camino deseado
;Si este existe, se mantiene el camino y se sobreescribe el peso.
(define (check-road-existence i-city f-city weight)
  (check-road-existence-aux i-city f-city weight node-map)
  )

;Funcion auxiliar para verificar la existencia del camino deseado
(define (check-road-existence-aux i-city f-city weight graph)

  #t
  
  )



;----------------------------------------Agregar Camino----------------------------------------;





;---------------------------------------Graficar Ciudades---------------------------------------;


;Funcion para dibujar todos los nodos al iniciar la aplicacion
;Llama a una funcion auxiliar
(define (draw-all-nodes)
  (draw-all-nodes-aux coords-list dc)
  )

;Funcion auxiliar para dibujar todos los nodos al iniciar la aplicacion
;Parametros:
;   list: nodes-list
;   dc: Drawing Context
(define (draw-all-nodes-aux list dc)
  (cond ( (null? (cdr list) )
              (draw-node dc (caar list) (cadar list) (caddar list) )
           )
        (else
             (draw-node dc (caar list) (cadar list) (caddar list) )
             (draw-all-nodes-aux (cdr list) dc)
         )
  )
)


;Funcion para dibujar un nodo
;Parametros:
;   dc: Drawing Context
;   node: numero de ciudad
;   x: coordenada en x
;   y: coordenada en y
(define (draw-node dc node x y)

  (cond ( (equal? (quotient node 10) 0)
          (send dc set-brush indian-red-brush)
         )
        ( (equal? (quotient node 10) 1)
          (send dc set-brush chocolate-brush)
         )
        ( (equal? (quotient node 10) 2)
          (send dc set-brush salmon-brush)
         )
        ( (equal? (quotient node 10) 3)
          (send dc set-brush green-brush)
         )
        ( (equal? (quotient node 10) 4)
          (send dc set-brush yellow-green-brush)
         )
        ( (equal? (quotient node 10) 5)
          (send dc set-brush aquamarine-brush)
         )
        ( (equal? (quotient node 10) 6)
          (send dc set-brush medium-turquoise-brush)
         )
        ( (equal? (quotient node 10) 7)
          (send dc set-brush lightblue-brush)
         )
        ( (equal? (quotient node 10) 8)
          (send dc set-brush medium-slate-blue-brush)
         )
        ( (equal? (quotient node 10) 9)
          (send dc set-brush plum-brush)
         )
        (else
         (send dc set-brush white-brush)
         )
    )

  ;Dibuja el nodo (circulo)
  (send dc set-pen black-pen)
  (send dc draw-ellipse (- x 15) (- y 15) 30 30)

  ;Muestra el respectivo numero dentro del nodo
  (send dc set-pen white-pen)

  (cond ( (>= 9 node)
          (send dc draw-text (string-append "0" (number->string node)) (- x 10) (- y 10) )
         )
        (else
         (send dc draw-text (number->string node) (- x 10) (- y 10) )
         )
        )
  
  
 
)


;---------------------------------------Graficar Ciudades---------------------------------------;
 












;---------------------------------------Graficar Caminos----------------------------------------;


;Funcion para dibujar todas las lineas (inicio de aplicacion)
(define (draw-all-lines)
  (draw-all-lines-aux node-map);CAMBIO
  )

;Funcion auxiliar para dibujar todas las lineas (inicio de aplicacion)
;Parametros:
;   graph: graph
(define (draw-all-lines-aux graph)
  (cond ( (null? (cdr graph) )
          (draw-lines-by-node dc (caar graph) (cadar graph))
         )
        (else         
         (draw-lines-by-node dc (caar graph) (cadar graph))
         (draw-all-lines-aux (cdr graph) ) ;Elimina el primer nodo del grafo
         )
    )
  )


;Funcion para dibujar lineas de un nodo especifico
;Parametros:
;   dc: Drawing Context
;   node: nodo inicial
;   connections: lista de nodos que finalizan la linea
(define (draw-lines-by-node dc node connections)
  (cond ( (null? connections )
          #t
         )
        (else
         (draw-line dc node (caar connections) (check-ways (caar connections) node) )
         (draw-lines-by-node dc node (cdr connections) )
         )
    )
  )


;Funcion para dibujar una linea
;Parametros:
;   ini: nodo inicial
;   fin: nodo final
;   way: cantidad de vias (1 o 2)
(define (draw-line dc ini fin way)
  (cond ( (equal? way 1)
          ;Dibuja solo las flechas de los caminos de una sola via
          (draw-arrows (+ 15 (get-coord ini "x")) (+ 15 (get-coord ini "y"))
              (+ 15 (get-coord fin "x")) (+ 15 (get-coord fin "y")) )
          ;Cambia al pen para una via
          (send dc set-pen one-way-pen)
         )
        ( (equal? way 2)
          ;Cambia al pen para dos vias
          (send dc set-pen two-way-pen)
         )
    )
  ;Manda a dibujar la linea
  (send dc draw-line
        (+ 15 (get-coord ini "x")) (+ 15 (get-coord ini "y"))
        (+ 15 (get-coord fin "x")) (+ 15 (get-coord fin "y")) )
        ; + 10 ya que el nodo tiene radio de 20 -> asi la linea queda en el medio
  )


;Funcion para verificar si el camino es one-way o two-way
;Parametros:
;   node: nodo por verificar
;   fin: nodo por encontrar camino
(define (check-ways node fin)
  (check-ways-aux node fin node-map);CAMBIO
  )

;Funcion auxiliar para verificar si el camino es one-way o two-way
;   node: nodo por verificar
;   fin: nodo por encontrar camino
;   graph: graph
(define (check-ways-aux node fin graph)
  (cond ( (null? graph)
          1
         )
        ( (equal? node (caar graph))
          (cond ( (path? fin (cadar graph) )
                  2
                 )
                (else
                 (check-ways-aux node fin (cdr graph) )
                 )
                )
         )
        (else
         (check-ways-aux node fin (cdr graph) )
         )
  )
  )

;Funcion para verificar si existe un path a la inversa del ya encontrado
;Parametros:
;   fin: nodo final que pasa a ser inicial
;   connections: lista de nodos que finalizarian la linea
(define (path? fin connections)
  (cond ( (null? connections)
          #f
         )
        ( (equal? fin (caar connections) )
          #t
          )
        (else
         (path? fin (cdr connections) )
         )
    )
 )


;Funcion para dibujar una flecha cerca de la linea
;Parametros:
;   x1:
;   y1:
;   x2:
;   y2:
(define (draw-arrows x1 y1 x2 y2)
  (send dc set-pen arrow-pen)
  (send dc set-brush arrow-brush)

         (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 6 7)
         
  )



;---------------------------------------Graficar Caminos----------------------------------------;




























































;---------------------------------------Busqueda de Rutas---------------------------------------;



;Funcion para iniciar la busqueda de los caminos
(define (begin-search)
  (cond ( (equal? #t (check-fields)) ;Si los campos de texto estan correctos
          ;Deshabilita el boton de busqueda y los radio-buttons
          (send search-button enable #f)
          (send rbuttons enable #f)
          ;Se informa en el text-box sobre la busqueda
          (search-info)
          ;Se inicia la busqueda de los caminos dependiendo del estado de seleccion del usuario
          (search-by-state)
         )
    )
 )


;Funcion para verificar los fields antes de buscar las rutas
(define (check-fields)
  (cond ( (equal? #f (check-destiny-field initial-destiny coords-list) )
        ;Si el destino inicial no esta en los nodos
          (send instructions-text-field set-value
              "El destino inicial seleccionado no existe.\nIngrese un nuevo destino." )
          #f )
        ( (equal? #f (check-destiny-field final-destiny coords-list) )
        ;Si el destino final no esta en los nodos
          (send instructions-text-field set-value
              "El destino final seleccionado no existe.\nIngrese un nuevo destino." )
          #f )
        (else
         #t )
  )
)

;Funcion para verificar las escogencias de los text-fields de los destinos
;Parametros:
;   text: string escrito en text-box
;   list: nodes-list
(define (check-destiny-field text list)
  (cond ( (null? list)
          #f )
        ( (equal? text ( number->string (caar list)) )
          #t )
        (else
         (check-destiny-field text (cdr list))
         )
    )
  )


;Funcion para cambiar el texto de instructions-text-field al presionar "Search"
(define (search-info)
  ;Se guarda en un string la seleccion de los radio buttons
(cond ( (equal? 0 path-selection)
        (set! search-state "Ruta más Corta" )
       )
      ( (equal? 1 path-selection)
        (set! search-state "Todas las Rutas" )
       )
      )

  ;Se cambia el texto del text-field
  (send instructions-text-field set-value (string-append
                                           "Tipo de Busqueda: " search-state
                                           "\n\nDestino Inicial: " initial-destiny
                                           "\nDestino Final: " final-destiny )
        )
  )


;Funcion para empezar la busqueda de caminos dependiendo de la seleccion del usuario
(define (search-by-state)
  (cond ( (equal? 0 path-selection)
          ;Camino mas corto
          (shortest-path-search (string->number initial-destiny) (string->number final-destiny) )
         )
        ( (equal? 1 path-selection)
          ;Todos los caminos
          (all-paths-search (string->number initial-destiny) (string->number final-destiny) )
         )
        (else
         (send instructions-text-field set-value "Tipo de Busqueda no seleccionado.")
         )
    )
  )


;Funcion para graficar el CAMINO MAS CORTO
(define (shortest-path-search ini fin)
  ;Se llama a la funcion BuscaCaminos en el archivo de logica
  ;Se guarda la lista con el camino y el peso
  (set! shortest-path (buscaCaminoCorto ini fin node-map) )
  
  ; Wait a second to let the window get ready
  (sleep/yield 0.1)
  ;Dibuja el grid
  (draw-grid)
  ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
  (draw-all-lines)
  ;Llama a dibujar el camino mas corto
  (draw-path shortest-path)
  ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
  (draw-all-nodes)
  ;Se dibujan nuevamente los pesos
  #|(draw-all-weights)|# ;;MIENTRAS

  ;Ingresa la informacion del camino al text-field
  (send information-text-field set-value (string-append "Información de Rutas:\n"
                                                        "Ruta más corta:\nPeso:"
                                                        (number->string (cadr shortest-path))
                                                        ))
 )



;Funcion para graficar TODOS LOS CAMINOS
(define (all-paths-search ini fin)
  ;Se llama a la funcion BuscaCaminos en el archivo de logica
  ;Se guarda la lista con los caminos y los pesos
  (set! all-paths (buscaCaminos ini fin node-map) )

  ;Ingresa las informaciones de los caminos al text-field
  (set-path-info all-paths)

  ;Crea los botones necesarios
  (set-path-buttons (length all-paths) 1)
  ;Se guarda una lista con los botones actuales
  (set! actual-buttons (send vpanel-buttons get-children) )
  
  ; Wait a second to let the window get ready
  (sleep/yield 0.1)

  ;Dibuja el grid
  (draw-grid)
  ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
  (draw-all-lines)
  ;Llama a dibujar el camino mas corto
  (draw-path (car all-paths) )
  ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
  (draw-all-nodes)
  
  ;Se dibujan nuevamente los pesos
  #|(draw-all-weights)|# ;;MIENTRAS
  
 )


;Funcion para graficar un camino
;Parametros:
;   list: lista con el camino y el peso
(define (draw-path list)
  (draw-path-aux (car list))
  )

;Funcion auxilir para graficar un camino 
;Parametros:
;   path-list: lista solo con el camino
(define (draw-path-aux path-list)
  (cond ( (null? (cddr path-list) )
          ;Grafica una linea entre los ultimos dos nodos
          (build-path dc (car path-list) (cadr path-list) )
         )
        (else
          ;Grafica una linea entre dos nodos
          (build-path dc (car path-list) (cadr path-list) )
          (draw-path-aux (cdr path-list) ) ;Elimina el primer nodo de la lista
         )
    )
  )


;Funcion para dibujar una linea
;Parametros:
;   ini: nodo inicial
;   fin: nodo final
;   way: cantidad de vias (1 o 2)
(define (build-path dc ini fin)
  ;Se escoje un pen rojo para trazar el camino
  (send dc set-pen red-pen)
  ;Manda a dibujar la linea
  (send dc draw-line
        (+ 15 (get-coord ini "x")) (+ 15 (get-coord ini "y"))
        (+ 15 (get-coord fin "x")) (+ 15 (get-coord fin "y")) )
        ; + 10 ya que el nodo tiene radio de 20 -> asi la linea queda en el medio

  )


;Funcion para ingresar la informacion de los caminos
(define (set-path-info paths-list)
  ;Redefine el information-text-field
  (send information-text-field set-value (string-append "Información de Rutas:\n"
                                                        (set-path-info-aux paths-list 1) ) )
  
  )

;Funcion para ingresar la informacion de los caminos
(define (set-path-info-aux paths-list n)
  
  (cond ( (null? paths-list )
          (set! path-num n)
          " "
         )
        (else

         (cond ( (equal? (length paths-list) 1)
                 (string-append "-> Ruta " (number->string n) ":\n     Peso: " (number->string (cadar paths-list)) 
                        (set-path-info-aux (cdr paths-list) (+ 1 n) ) )
                )
               (else
                (string-append "-> Ruta " (number->string n) ":\n     Peso: " (number->string (cadar paths-list)) "\n" 
                        (set-path-info-aux (cdr paths-list) (+ 1 n) ) )
                )
           )
 
         )
        )
  
  )


;Funcion para agregar los botones de las rutas
(define (set-path-buttons total n)
  (cond ( (equal? (+ total 1) n)
          #t)
        (else
         
         (cond ( (> 20 n)
                 (set-path-buttons-aux total n vpanel-buttons)
                 )
               ;Si se sobrepasa de 21 botones
               (else
                (set-path-buttons-aux total n vpanel-buttons2)
                )
               )
         
         (set-path-buttons total (+ n 1))
         
         )
        )
  )


;Funcion auxiliar para agregar los botones de las rutas
(define (set-path-buttons-aux total n panel)
  (cond ( (< (length actual-buttons) n )
          ;Boton Ruta N
          (new button% [parent vpanel-buttons] [label (string-append "Ruta " (number->string n) )]
               
               [callback (lambda (button event)
                           ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
                           (draw-all-lines)
                           
                           (build-specific-path n all-paths)

                           ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
                           (draw-all-nodes)
                           ;Se dibujan nuevamente los pesos
                           (draw-all-weights)
                           
                         )])
         )
        (else

         (send (get-button n actual-buttons) show #t)

         )
        )
  )


;Funcion para obtener el boton necesario n
(define (get-button n list)
  (cond ( (equal? 1 n)
          (car list)
         )
        (else
         (get-button (- n 1) (cdr list))
         )
   )
  )


;Funcion para eliminar los botones luego de ser utilizados
(define (delete-actual-buttons list)
  (cond ( (null? list)
          #t
          )
        (else
         (send (car list) show #f)

         (delete-actual-buttons (cdr list) )
         
         )
    )
  )

;Funcion para graficar una ruta especifica
;Usada en los botones dinamicos
(define (build-specific-path n paths)
  (cond ( (<= n (length paths))   
          (cond ( (equal? n 1)
                  (draw-path (car paths))
                 )
                (else
                 (build-specific-path (- n 1) (cdr paths) )
                 )
            )
         )
        (else
         (send instructions-text-field set-value "Ruta inexistente")
         )

        )
  )


;Funcion que sera llamada al presionar "Nueva Busqueda"
;"Reiniciara" la aplicacion
(define (new-search)
  ;Re-draws
  (draw-grid)
  (draw-all-lines)
  (draw-all-nodes)
  #|(draw-all-weights)|# ;Por mientras
  ;Redefinicion de instruction-text-field
  (send instructions-text-field set-value "Bienvenidos a WazeTico!\n
Favor ingrese un nuevo destino inicial y un nuevo destino final.\n")
  ;Borra las selecciones de los destinos en text-fields
  (send initial-text-field set-value "")
  (send final-text-field set-value "")
  ;Redefinicion de information-text-field
  (send information-text-field set-value "Información de Rutas:" )
  ;Elimina los botones de las rutas
  (delete-actual-buttons actual-buttons)
  ;Habilita el boton de busqueda y los radio-buttons
  (send search-button enable #t)
  (send rbuttons enable #t)
  )


;---------------------------------------Busqueda de Rutas---------------------------------------;





;--------------------------------------------Widgets--------------------------------------------;





;========================================Pantalla Inicial========================================;


;Frame inicial
(define initial-frame (new frame% [label "WazeTico"]
                   [width 400]
                   [height 200]
                   [alignment '(center center)]))


;Panel vertical
;Incluye title y hpanel-initial
(define vpanel-initial (new vertical-panel% [parent initial-frame]
                            [alignment '(center center)]))


; Make a static text message in the frame
(define title (new message% [parent vpanel-initial]
                          [label "WazeTico"]))


;Panel horizontal
(define hpanel-initial (new horizontal-panel% [parent vpanel-initial]
                            [alignment '(center center)]))


;Boton Iniciar
(define initial-initialize-buton (new button% [parent hpanel-initial]
             [label "Iniciar"]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         ;Cierra la ventana inicial
                         (send initial-frame show #f)
                         ;Abre la ventana principal
                         (send main-frame show #t)

                         ; Wait a second to let the window get ready
                         (sleep/yield 1)
                         (draw-grid)

                         
                         #|
                         ;Initial draws
                         (draw-all-lines)
                         (draw-all-nodes)
                         (draw-all-weights)
                         |#
                         
                         )]))


;========================================Pantalla Inicial========================================;





;=======================================Pantalla Principal=======================================;


; Frame principal
(define main-frame (new frame% [label "WazeTico"]
                   [width 1050]
                   [height 660]
                   [alignment '(left top)]))


;Panel principal (horizontal)
(define main-panel (new horizontal-panel% [parent main-frame] ))
 

;Panel vertical
;Incluye cavas(mapa) y panel horizontal secundario
(define vpanel (new vertical-panel% [parent main-panel]))


;Canvas donde se muestra el grafo
(define map-canvas (new canvas% [parent vpanel]
                       [style '(border)]
                       [label "MAP"] 
                       [vert-margin 10]	 
                       [horiz-margin 10]
                       [min-height 500]
                       [min-width 790]
                       ))


;Drawing Context de map-canvas
(define dc (send map-canvas get-dc))


;Panel horizontal secundario
;Incluye text-field (instrucciones) y vpanel2 (botones)
(define hpanel2 (new horizontal-panel% [parent vpanel]
                     [alignment '(center center)]))


;Text-field de instrucciones
(define instructions-text-field ( new text-field% [parent hpanel2]
                                    [label #f]
                                    [init-value
                                     "                                  ¡Bienvenido a WazeTico!\n
-> Cree su propio mapa con ciudades personalizadas.
-> Agregue caminos y distancias entre ellos.
-> Presione \"Agregar Ciudad\" para iniciar." ]
                                    [vert-margin 10]	 
                                    [horiz-margin 10]
                                    [min-width 380]
                                    [min-height 120]))




;Panel vertical II
;Agregar Ciudades y Caminos
(define vpanel2 (new vertical-panel% [parent hpanel2]
                     [alignment '(center center)]))

;Boton Search
(define add-city-button (new button% [parent vpanel2]
             [label "Agregar Ciudad"]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send add-city-frame show #t)
                         )]))

;Boton Agregar Camino
(define add-road-button (new button% [parent vpanel2]
             [label "Agregar Camino"]
             [enabled #f]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send add-road-frame show #t)
                         )]))


;Panel vertical IV
;Destinos
(define vpanel4 (new vertical-panel% [parent hpanel2]
                     [alignment '(center center)]))


;Text-field de destino inicial
(define initial-text-field ( new text-field% [parent vpanel4]
                                 [enabled #f]       
                                 [label #f]
                                 [min-width 10]
                                 ))

;Label de destino Inicial
(define intial-label (new message% [parent vpanel4]
                          [label "Destino Inicial"]))


;Text-field de destino final
(define final-text-field ( new text-field% [parent vpanel4]
                               [enabled #f]       
                               [label #f]
                               [min-width 10]
                               ))

;Label de destino Final
(define final-label (new message% [parent vpanel4]
                          [label "Destino Final"]))


;Panel vertical V
;Busqueda
(define vpanel5 (new vertical-panel% [parent hpanel2]
                     [alignment '(center center)]
                     ))


;Radio Button Selection
(define rbuttons (new radio-box% [label ""]
     [enabled #f]                 
     [choices '("Ruta más Corta" "Todas las Rutas")]
     [parent vpanel5]
))
     

;Boton Search
(define search-button (new button% [parent vpanel5]
             [label "Buscar"]
             [enabled #f]
             [callback (lambda (button event)
                         ;Ingresa el texto de los fields a sus respectivas variables
                         (set! initial-destiny (send initial-text-field get-value))
                         (set! final-destiny (send  final-text-field get-value))
                         (set! path-selection (send rbuttons get-selection) )
                         ;Comienza el proceso de busqueda
                         (begin-search)
                         
                         )]))


;Boton New Path
(define new-search-button (new button% [parent vpanel5]
             [label "Cambiar Destinos"]
             [enabled #f]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (new-search)
                         )]))


;Text-field de informacion
(define information-text-field ( new text-field% [parent main-panel]
                                    [label #f]
                                    [init-value "Información de Rutas:"]
                                    [vert-margin 10]	 
                                    [horiz-margin 10]
                                    [min-width 160]
                                    [min-height 640]))


;Panel vertical de botones principal
;
(define vpanel-buttons (new vertical-panel% [parent main-panel]
                            [horiz-margin 8]
                     [alignment '(center center)]
                     ))

;Panel vertical de botones secundario
;
(define vpanel-buttons2 (new vertical-panel% [parent main-panel]
                             	;[enabled #f]
                             [horiz-margin 3]
                     [alignment '(center center)]
                     ))


;=======================================Pantalla Principal=======================================;





;====================================Window de Agregar Ciudad====================================;


;Frame de Agregar Ciudad
(define add-city-frame (new frame% [label "Agregar Ciudad"]
                   [width 140]
                   [height 180]
                   [alignment '(center center)]))

;Panel vertical city
;Agregar Ciudad
(define vpanel-city (new vertical-panel% [parent add-city-frame]
                     [alignment '(center center)]))


;Text-field de agregar ciudad
(define add-city-text-field ( new text-field% [parent vpanel-city]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de agregar ciudad
(define add-city-label (new message% [parent vpanel-city]
                          [label "Nuevo Nodo"]))


;Text-field de agregar ciudad
(define add-city-x-text-field ( new text-field% [parent vpanel-city]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de agregar ciudad
(define add-city-x-label (new message% [parent vpanel-city]
                          [label "Posición en x"]))

;Text-field de agregar ciudad
(define add-city-y-text-field ( new text-field% [parent vpanel-city]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de agregar ciudad
(define add-city-y-label (new message% [parent vpanel-city]
                          [label "Posición en y"]))





;Boton Search
(define add-city-window-button (new button% [parent vpanel-city]
             [label "Agregar Ciudad"]
             [callback (lambda (button event)
                         (add-city)
                         )]))


;====================================Window de Agregar Ciudad====================================;





;====================================Window de Agregar Camino====================================;


;Frame de Agregar Camino
(define add-road-frame (new frame% [label "Agregar Camino"]
                   [width 140]
                   [height 180]
                   [alignment '(center center)]))


;Panel vertical III
;Agregar Camino
(define vpanel3 (new vertical-panel% [parent add-road-frame]
                     [alignment '(center center)]))


;Text-field de agregar ciudad
(define add-road-initial-text-field (new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de agregar ciudad
(define add-road-initial-label (new message% [parent vpanel3]
                          [label "Ciudad Inicial"]))

;Text-field de destino Final
(define add-road-final-text-field ( new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de destino Final
(define add-road-final-label (new message% [parent vpanel3]
                          [label "Ciudad Final"]))

;Text-field de peso
(define add-road-weight-text-field (new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de peso
(define add-road-weight-label (new message% [parent vpanel3]
                          [label "Distancia"]))

;Boton Agregar Camino
(define add-road-window-button (new button% [parent vpanel3]
             [label "Agregar Camino"]
             [callback (lambda (button event)
                         (add-road)
                         )]))


;====================================Window de Agregar Camino====================================;





;--------------------------------------------Widgets--------------------------------------------;





;--------------------------------------Inicio de Aplicacion--------------------------------------;


; Se muestra el frame inicial
(send initial-frame show #t)


;--------------------------------------Inicio de Aplicacion--------------------------------------;

