;Ejercicio 1
; a) Bajo demanda

(deffacts Factos
  (Eueueu 3)
  (Eueueu 4)
  (Eueueu 5)
  (Eueueu 6)
  (Eueueu 7)
)

(defrule contar
  ?a <- (ContarHechos ?X)
  =>
  (bind ?num (length$ (find-all-facts ((?v ?X)) (neq ?X null)) ))
  (assert (NumeroHechos ?X ?num))
  (retract ?a)
)

(defrule borrarDuplicados
  ?a <- (NumeroHechos ?X ?num1)
  ?b <- (NumeroHechos ?X ?num2)
  =>
  (if (= ?num1 ?num2)
    then
    (retract ?b)
    else
    (bind ?num_hechos (length$ (find-all-facts ((?v ?X)) (neq ?X null)) ))
    (if (= ?num_hechos ?num1)
      then
      (retract ?b)
      else
      (retract ?a)
    )
  )
)
