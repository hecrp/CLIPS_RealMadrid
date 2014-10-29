;;; Clase básica de jugador
(defclass Jugador
(is-a USER)
(role concrete)
(slot nombre
  (type STRING)
  (create-accessor read-write))
(slot dorsal
  (type STRING)
  (create-accessor read-write))
(slot lesionado
  (type INTEGER) (default 0)
  (create-accessor read-write))
(slot partidos_acumulados
  (type INTEGER) (default 0)
  (create-accessor read-write))
  
  )
  
;;; Jugadores
;;; Porteros
       
(CASILLAS of Jugador
(nombre Iker Casillas)
(dorsal 1)
(lesionado 0)
(partidos_acumulados 0)
)

(defclass KNAVAS
(is-a Jugador)
(role concrete)
)

;;; Defensas

(defclass VARANE
(is-a Jugador)
(role concrete)
)

(defclass PEPE
(is-a Jugador)
(role concrete)
)

(defclass RAMOS
(is-a Jugador)
(role concrete)
)

(defclass COENTRAO
(is-a Jugador)
(role concrete)
)

(defclass MARCELO
(is-a Jugador)
(role concrete)
)

(defclass CARVAJAL
(is-a Jugador)
(role concrete)
)

(defclass ARBELOA
(is-a Jugador)
(role concrete)
)

(defclass NACHO
(is-a Jugador)
(role concrete)
)

(defclass LLORENTE
(is-a Jugador)
(role concrete)
)

  
;;; Respuestas de las preguntas / hechos a tener en cuenta

	
;;; Obtiene una respuesta de entre un conjunto de respuestas posibles

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
   )
   ?answer)

;;; Hace una pregunta a la que hay que responder si o no

(deffunction si-o-no-p (?question)
   (bind ?response (ask-question ?question si no s n))
   (if (or (eq ?response si) (eq ?response s)) ;;; En el caso de que escriba si o s
       then TRUE 
       else FALSE))
       
;;; Hace una pregunta a la que hay que responder A, B o C

(deffunction a-b-c (?question)
   (bind ?response (ask-question ?question a b c))
   (if (eq ?response a) ;;; En el caso de que escriba a
       then a
       else if (eq ?response b) ;;; En el caso de que escriba b
       then b 
       else c ))
       

;;; Preguntamos si hay lesionados
(defrule lesionados
 =>
  (if  (si-o-no-p "Hay jugadores lesionados? (s/n) ")
    then 
     (assert (lesionados 1))
 ) 
)

;; Template clase rival

(deftemplate nivel
  (slot niv)
)

;; Nivel del rival

(deffacts clase_rival
 (nivel (niv c))
)

;; Pregunta por la clase del rival

  (defrule determinarrival
?r <-(nivel (niv c))
=>
 (if  (eq a-b-c "Qué nivel tiene el rival?" a)
    then 
     (modify ?r (niv a))
     else if (eq a-b-c "Qué nivel tiene el rival?" b) ;;; En el caso de que escriba b
       then (modify ?r (niv b))
       else (modify ?r (niv c)) ))
