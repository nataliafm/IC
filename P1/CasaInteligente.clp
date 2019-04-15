; apaga y enciende las luces usando el conocimiento del profesor
(defglobal ?*aux* = 0)

(deftemplate Habitacion (field Nombre) (field Puertas (type STRING)) (field Pasos (type STRING)) (field Ventanas (type STRING)) (field valores (type STRING)) )
(deftemplate Puerta (field Nombre) (field H1) (field H2) )
(deftemplate Ventana (field Nombre) (field Habitacion) )
(deftemplate PasoSinPuerta (field Nombre) (field H1) (field H2) )
(deftemplate posible_pasar (field H1) (field H2) )
(deftemplate necesario_pasar (field H1) (field H2) )
(deftemplate habitacion_interior (field Habitacion) )
(deftemplate luminosidad_max (field H1) (field l) )
(deftemplate luminosidad_necesaria (field H1) (field l))

(deftemplate valor_registrado (field t) (field tipo) (field H1) (field v) )

; a la hora de hacer la simulación, se hace referencia a las habitaciones por su nombre (R1, R2, ...)
(deffacts Habitaciones
  (Habitacion (Nombre R1) (Puertas "P1 P2 P3") (Pasos "null") (Ventanas "V1") (valores "M1, P1"))
  (Habitacion (Nombre R2) (Puertas "P2") (Pasos "null") (Ventanas "null") (valores "M2, P2"))
  (Habitacion (Nombre R3) (Puertas "P4") (Pasos "null") (Ventanas "null") (valores "M3, P3"))
  (Habitacion (Nombre R4) (Puertas "P3") (Pasos "null") (Ventanas "null") (valores "M4, P4"))
  (Habitacion (Nombre R5) (Puertas "P4 P5") (Pasos "null") (Ventanas "V6") (valores "M5, P5"))
  (Habitacion (Nombre R6) (Puertas "P1") (Pasos "A1 A3") (Ventanas "V2 V2 V4") (valores "M6, P6"))
  (Habitacion (Nombre R7) (Puertas "null")(Pasos "A1 A2") (Ventanas "V5") (valores "M7, P7"))
  (Habitacion (Nombre R8) (Puertas "P6 P5") (Pasos "A2 A3") (Ventanas "null") (valores "M8, P8"))
  (Habitacion (Nombre Exterior) (Puertas "P6") (Pasos "null") (Ventanas "null") (valores "null"))
)

(deffacts Puertas
  (Puerta (Nombre P1) (H1 R1) (H2 R6))
  (Puerta (Nombre P2) (H1 R1) (H2 R2))
  (Puerta (Nombre P3) (H1 R1) (H2 R4))
  (Puerta (Nombre P4) (H1 R3) (H2 R5))
  (Puerta (Nombre P5) (H1 R5) (H2 R8))
)

(deffacts Ventanas
  (Ventana (Nombre V1) (Habitacion R1))
  (Ventana (Nombre V2) (Habitacion R6))
  (Ventana (Nombre V3) (Habitacion R6))
  (Ventana (Nombre V4) (Habitacion R6))
  (Ventana (Nombre V5) (Habitacion R7))
  (Ventana (Nombre V6) (Habitacion R5))
)

(deffacts Pasos
  (PasoSinPuerta (Nombre A1) (H1 R7) (H2 R6))
  (PasoSinPuerta (Nombre A2) (H1 R7) (H2 R8))
  (PasoSinPuerta (Nombre A3) (H1 R8) (H2 R6))
)

(deffacts UltimosRegistros
  (ultimo_registro movimiento R1 0)
  (ultimo_registro movimiento R2 0)
  (ultimo_registro movimiento R3 0)
  (ultimo_registro movimiento R4 0)
  (ultimo_registro movimiento R5 0)
  (ultimo_registro movimiento R6 0)
  (ultimo_registro movimiento R7 0)
  (ultimo_registro movimiento R8 0)

  (ultimo_registro estadoluz R1 0)
  (ultimo_registro estadoluz R2 0)
  (ultimo_registro estadoluz R3 0)
  (ultimo_registro estadoluz R4 0)
  (ultimo_registro estadoluz R5 0)
  (ultimo_registro estadoluz R6 0)
  (ultimo_registro estadoluz R7 0)
  (ultimo_registro estadoluz R8 0)
)

(deffacts LuminosidadesMax
  (luminosidad_max (H1 R1) (l 300))
  (luminosidad_max (H1 R2) (l 400))
  (luminosidad_max (H1 R3) (l 300))
  (luminosidad_max (H1 R4) (l 300))
  (luminosidad_max (H1 R5) (l 300))
  (luminosidad_max (H1 R6) (l 600))
  (luminosidad_max (H1 R7) (l 400))
  (luminosidad_max (H1 R8) (l 400))
)

(deffacts LuminosidadesNecesarias
  (luminosidad_necesaria (H1 R1) (l 150))
  (luminosidad_necesaria (H1 R2) (l 200))
  (luminosidad_necesaria (H1 R3) (l 150))
  (luminosidad_necesaria (H1 R4) (l 150))
  (luminosidad_necesaria (H1 R5) (l 150))
  (luminosidad_necesaria (H1 R6) (l 300))
  (luminosidad_necesaria (H1 R7) (l 200))
  (luminosidad_necesaria (H1 R8) (l 200))
)

(deffacts UltimasActivaciones
  (ultima_activacion_movimiento R1 0)
  (ultima_activacion_movimiento R2 0)
  (ultima_activacion_movimiento R3 0)
  (ultima_activacion_movimiento R4 0)
  (ultima_activacion_movimiento R5 0)
  (ultima_activacion_movimiento R6 0)
  (ultima_activacion_movimiento R7 0)
  (ultima_activacion_movimiento R8 0)
)

(deffacts UltimasDesactivaciones
  (ultima_desactivacion_movimiento R1 0)
  (ultima_desactivacion_movimiento R2 0)
  (ultima_desactivacion_movimiento R3 0)
  (ultima_desactivacion_movimiento R4 0)
  (ultima_desactivacion_movimiento R5 0)
  (ultima_desactivacion_movimiento R6 0)
  (ultima_desactivacion_movimiento R7 0)
  (ultima_desactivacion_movimiento R8 0)
)

; dos habitaciones están conectadas por una puerta o un paso sin puerta
(defrule PasarDirectamente
  (or
  (Puerta (Nombre ?) (H1 ?H1) (H2 ?H2))
  (PasoSinPuerta (Nombre ?) (H1 ?H1) (H2 ?H2))
  )
  =>
  (assert (posible_pasar (H1 ?H1) (H2 ?H2)))
)

; si una habitación solo tiene una puerta o un paso sin puerta, es necesario pasar por la habitación con la que tiene conexión
(defrule SoloUnaPuerta
  ; se comprueba que existe la habitación
  (Habitacion (Nombre ?H1) (Puertas ?P) (Pasos ?H))
  ; la habitación no tiene pasos y solo tiene una puerta
  ; o
  ; la habitación no tiene puertas y solo tiene un paso
  (or
    (and (test (eq ?H "null")) (test (= (str-length ?P) 2) ))
    (and (test (eq ?P "null")) (test (= (str-length ?H) 2) ))
  )
  =>
  (assert (necesario_pasar (H1 ?H1) (H2 ?H1)))
)

(defrule EsInterior
  ; se comprueba que existe la habitación
  (Habitacion (Nombre ?N) (Ventanas ?V))
  ; la habitación no tiene ventanas
  (test (eq ?V "null"))
  =>
  (assert (habitacion_interior (Habitacion ?N))) ; es una habitación interior
)

(defrule RegistrarValorMovimientoPositivo
    ; el manejo inteligente de luces está activado para ?habitacion
    (Manejo_inteligente_luces ?habitacion)
    ; hay movimiento en la habitación
    ?a <- (valor movimiento ?habitacion on)
    ?b <- (ultimo_registro movimiento ?habitacion ?ti)
    ?c <- (ultima_activacion_movimiento ?habitacion ?tii)
    =>
    (bind ?t (totalsegundos ?*hora* ?*minutos* ?*segundos*))
    (bind ?h ?habitacion)
    (retract ?a)
    (retract ?b)
    (retract ?c)
    ; se sustituyen el último registro de movimiento y la última activación de movimiento para ?habitación
    ; y se registra el nuevo valor
    (assert (ultima_activacion_movimiento ?h ?t))
    (assert (valor_registrado (t ?t) (tipo movimiento) (H1 ?h) (v on)))
    (assert (ultimo_registro movimiento ?h ?t))

)

(defrule RegistrarValorMovimientoNegativo
    ; el manejo inteligente de luces está activado para ?habitacion
    (Manejo_inteligente_luces ?habitacion)
    ; no hay movimiento en la habitación
    ?a <- (valor movimiento ?habitacion off)
    ?b <- (ultimo_registro movimiento ?habitacion ?ti)
    ?c <- (ultima_desactivacion_movimiento ?habitacion ?tii)
    =>
    (bind ?t (totalsegundos ?*hora* ?*minutos* ?*segundos*))
    (bind ?h ?habitacion)
    (retract ?a)
    (retract ?b)
    (retract ?c)
    ; se sustituyen el último registro de movimiento y la última desactivación de movimiento para ?habitación
    ; y se registra el nuevo valor
    (assert (ultima_desactivacion_movimiento ?h ?t))
    (assert (valor_registrado (t ?t) (tipo movimiento) (H1 ?h) (v off)))
    (assert (ultimo_registro movimiento ?h ?t))
)

(defrule RegistrarValorLuzPositivo
    ; el manejo inteligente de luces está activado para ?habitacion
    (Manejo_inteligente_luces ?habitacion)
    ; se enciende la luz en ?habitacion
    ?a <- (valor estadoluz ?habitacion on)
    ?b <- (ultimo_registro estadoluz ?habitacion ?ti)
    =>
    (bind ?t (totalsegundos ?*hora* ?*minutos* ?*segundos*))
    (bind ?h ?habitacion)
    (retract ?a)
    (retract ?b)
    ; se sustituye el último registro de estadoluz y se registra el nuevo valor
    (assert (valor_registrado (t ?t) (tipo estadoluz) (H1 ?h) (v on)))
    (assert (ultimo_registro estadoluz ?h ?t))
)

(defrule RegistrarValorLuzNegativo
    ; el manejo inteligente de luces está activado para ?habitacion
    (Manejo_inteligente_luces ?habitacion)
    ; se apaga la luz en ?habitacion
    ?a <- (valor estadoluz ?habitacion off)
    ?b <- (ultimo_registro estadoluz ?habitacion ?ti)
    =>
    (bind ?t (totalsegundos ?*hora* ?*minutos* ?*segundos*))
    (bind ?h ?habitacion)
    (retract ?a)
    (retract ?b)
    ; se sustituye el último registro de estadoluz y se registra el nuevo valor
    (assert (valor_registrado (t ?t) (tipo estadoluz) (H1 ?h) (v off)))
    (assert (ultimo_registro estadoluz ?h ?t))
)

(defrule Informe
  (informe ?h)
  =>
  (printout t "INFORME" crlf)
  ;recorre todos los valores registrados para la habitación ?h e imprime la hora, el valor y el tipo
  (do-for-all-facts ((?va valor_registrado))
  (eq ?va:H1 ?h)
  (printout t ?va:t " " ?va:v " " ?va:tipo crlf))
  (printout t "FIN INFORME" crlf)
)

(defrule Encender
  ; el manejo inteligente de luces está activado para ?habitacion
  (Manejo_inteligente_luces ?habitacion)
  ; hay movimiento en ?habitacion
  (ultimo_registro movimiento ?habitacion ?t)
  (valor_registrado (t ?t) (tipo movimiento) (H1 ?habitacion) (v on))
  ; la luminosidad en la habitación es menor de la necesaria para poder ver
  (luminosidad_necesaria (H1 ?habitacion) (l ?l))
  (luminosidad ?habitacion ?li)
  (test (< ?li ?l))
  =>
  (assert (accion pulsador_luz ?habitacion encender)) ; enciende la luz
)

; cuando detecta que no hay movimiento en ?habitación, genera otro dato para el sensor 10 segundos después
(defrule datosensor_movimiento_off
  (declare (salience 10000))
  (Manejo_inteligente_luces ?habitacion)
  (datosensor  ?h ?m ?s  movimiento ?habitacion off)
  (hora_actual ?h1)
  (minutos_actual ?m1)
  (segundos_actual ?s1)
  (test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
  (test (= ?*aux* 0))
  =>
  (if (eq ?habitacion R2) ;si la habitación es el baño, en vez de esperarse 10 segundos cuando no detecta movimiento, apaga la luz directamente
    then
    (bind ?*aux* 10)
    else
    (bind ?*aux* 1)
    (assert (datosensor  (hora_suma ?h ?m ?s 10) (minuto_suma ?h ?m ?s 10) (segundo_suma ?h ?m ?s 10) movimiento ?habitacion  off ))
  )
)

; señala que han pasado 10 segundos desde que se detectó que la habitación está vacía
(defrule aux
  (datosensor  ?h ?m ?s  movimiento ?habitacion off)
  (test (= ?*aux* 1))
  =>
  (bind ?*aux* 10)
)

(defrule ApagarPorVacia
  ; el manejo inteligente de luces está activado para ?habitacion
  (Manejo_inteligente_luces ?habitacion)
  ; la luz en ?habitacion está encendida
  (valor_registrado (t ?t) (tipo estadoluz) (H1 ?habitacion) (v on))
  (ultimo_registro estadoluz ?habitacion ?t)
  ; el último registro de movimiento en ?habitación dice que no hay movimiento
  (ultimo_registro movimiento ?habitacion ?tiii)
  (valor_registrado (t ?tiii) (tipo movimiento) (H1 ?habitacion) (v off))
  ; ?habitacion lleva 10 segundos sin movimiento
  (test (>= ?*aux* 10))
  =>
  (bind ?*aux* 0)
  (assert (accion pulsador_luz ?habitacion apagar)) ; apaga la luz
)

(defrule ApagarPorLuz
  ; el manejo inteligente de luces está activado para ?habitacion
  (Manejo_inteligente_luces ?habitacion)
  ; la luz en ?habitacion está encendida
  (valor_registrado (t ?t) (tipo estadoluz) (H1 ?habitacion) (v on))
  (ultimo_registro estadoluz ?habitacion ?t)
  ; la luminosidad en la habitación es mayor de la máxima necesaria
  (luminosidad_max (H1 ?habitacion) (l ?l))
  (luminosidad ?habitacion ?li)
  (test (> ?li ?l))
  =>
  (assert (accion pulsador_luz ?habitacion apagar)) ; apaga la luz
)
