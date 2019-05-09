;Ejercicio 2
; b) Vector ordenado
(defglobal ?*min* = 1000)
(deffacts Facts
  (T 5 6 7 8 4)
  (T 5 3 4 7 9)
  (T 1 2 3 4 5)
)

(defrule minXiT
  (T ?a ?b ?c ?d ?e)
  =>
  (if (< ?a ?*min*) then (bind ?*min* ?a))
  (if (< ?b ?*min*) then (bind ?*min* ?b))
  (if (< ?c ?*min*) then (bind ?*min* ?c))
  (if (< ?d ?*min*) then (bind ?*min* ?d))
  (if (< ?e ?*min*) then (bind ?*min* ?e))
)
