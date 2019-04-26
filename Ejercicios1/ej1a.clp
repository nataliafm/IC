;Ejercicio 1
; a) Bajo demanda

(deffacts Factos
  (Eueueu 3)
  (Eueueu 4)
  (Eueueu 5)
  (Eueueu 6)
  (Eueueu 7)
)

(deffacts ElFacto
  (NumeroHechos Eueueu 0)
)

(defrule contar
  ?a <- (ContarHechos ?X)
  (exists (NumeroHechos ?X ?))
  ?b <- (NumeroHechos ?X ?)
  =>
  (retract ?b)
  (bind ?num (length$ (find-all-facts ((?v ?X)) (neq ?X null)) ))
  (assert (NumeroHechos ?X ?num))
  (retract ?a)
)
