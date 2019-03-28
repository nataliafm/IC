(deftemplate Habitacion (field Nombre) (field Puertas (type STRING)) (field Pasos (type STRING)) (field Ventanas (type STRING)) (field Sensores (type STRING)) )
(deftemplate Puerta (field Nombre) (field H1) (field H2) )
(deftemplate Ventana (field Nombre) (field Habitacion) )
(deftemplate PasoSinPuerta (field Nombre) (field H1) (field H2) )
(deftemplate posible_pasar (field H1) (field H2) )
(deftemplate necesario_pasar (field H1) (field H2) )
(deftemplate habitacion_interior (field Habitacion) )

(deftemplate Sensor (field Tipo))

(deftemplate valor_registrado (field t) (field tipo) (field H1) (field v) )
(deftemplate ultimo_registro (field tipo) (field H1) (field t) )
(deftemplate ultima_activacion_movimiento (field H1) (field t) )
(deftemplate ultima_desactivacion_movimiento (field H1) (field t))

(deffacts Habitaciones
  (Habitacion (Nombre R1) (Puertas "P1 P2 P3") (Pasos "null") (Ventanas "V1") (Sensores "M1, P1"))
  (Habitacion (Nombre R2) (Puertas "P2") (Pasos "null") (Ventanas "null") (Sensores "M2, P2"))
  (Habitacion (Nombre R3) (Puertas "P4") (Pasos "null") (Ventanas "null") (Sensores "M3, P3"))
  (Habitacion (Nombre R4) (Puertas "P3") (Pasos "null") (Ventanas "null") (Sensores "M4, P4"))
  (Habitacion (Nombre R5) (Puertas "P4 P5") (Pasos "null") (Ventanas "V6") (Sensores "M5, P5"))
  (Habitacion (Nombre R6) (Puertas "P1") (Pasos "A1 A3") (Ventanas "V2 V2 V4") (Sensores "M6, P6"))
  (Habitacion (Nombre R7) (Puertas "null")(Pasos "A1 A2") (Ventanas "V5") (Sensores "M7, P7"))
  (Habitacion (Nombre R8) (Puertas "P6 P5") (Pasos "A2 A3") (Ventanas "null") (Sensores "M8, P8"))
  (Habitacion (Nombre Exterior) (Puertas "P6") (Pasos "null") (Ventanas "null") (Sensores "null"))
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

(deffacts Movimientos
  (Sensor (Tipo "movimiento") (Nombre M1) (H1 R1) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M2) (H1 R2) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M3) (H1 R3) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M4) (H1 R4) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M5) (H1 R5) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M6) (H1 R6) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M7) (H1 R7) (ON/OFF "off") (Lux 0))
  (Sensor (Tipo "movimiento") (Nombre M8) (H1 R8) (ON/OFF "off") (Lux 0))
)

(deffacts PulsadoresLuz
  (Sensor (Tipo "luz") (Nombre P1) (H1 R1) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P2) (H1 R2) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P3) (H1 R3) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P4) (H1 R4) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P5) (H1 R5) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P6) (H1 R6) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P7) (H1 R7) (ON/OFF "off"))
  (Sensor (Tipo "luz") (Nombre P8) (H1 R8) (ON/OFF "off"))
)
