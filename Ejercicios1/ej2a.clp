;Ejercicio 2
; a) Templates

(deftemplate T (slot S (type NUMBER)))

(deffacts TTT
  (T (S 1))
  (T (S 2))
  (T (S 3))
)

(defrule minSdeT
  (Ejecutar ?)
  =>
  (bind ?min 4)
  (do-for-all-facts ((?T T)) (< ?T:S ?min) (bind ?min ?T:S))
  (printout t "Valor minimo: " crlf)
  (printout t ?min crlf)
  (assert (Min ?min))
)
