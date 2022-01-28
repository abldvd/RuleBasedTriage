;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Funciones
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question si no s n))
   (if (or (eq ?response si) (eq ?response s))
       then si 
       else no))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Inicializacion
(defrule saludo ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Sistema de triage de emergencia")
  (printout t crlf crlf))

(defrule respuesta ""
  (declare (salience 10))
  (prioridad ?item)
  =>
  (printout t crlf crlf)
  (printout t "Grado de prioridad sugerido:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Preguntas
(defrule puede-caminar ""
  (not (camina ?))
  (not (prioridad ?))
  =>
  (assert (camina (yes-or-no-p "Puede caminar el paciente (si/no)? "))))
  
(defrule esta-gravemente-herido ""
  (not (herida-grave ?))
  (camina no)
  (not (prioridad ?))
  =>
  (assert (herida-grave (yes-or-no-p "¿Está mortalmente herido? (si/no)? "))))
  
(defrule tiene-dificultades-respirar ""
  (not (respira-bien ?))
  (camina no)
  (herida-grave no)
  (not (prioridad ?))
  =>
  (assert (respira-bien (yes-or-no-p "¿El paciente respira con normalidad? (si/no)? "))))

(defrule tiene-herida-sangrante ""
  (not (sangra ?))
  (respira-bien si)
  (camina no)
  (herida-grave no)
  (not (prioridad ?))
  =>
  (assert (sangra (yes-or-no-p "¿El paciente sangra? (si/no)? "))))

(defrule parar-sangrado ""
  (sangra si)
  (respira-bien si)
  (camina no)
  (herida-grave no)
  (not (prioridad ?))
  =>
  (bind ?response (ask-question "Intente parar la hemorragia... ¿Sigue sangrando? (si/no) " si no s n))
  (if (or (eq ?response si) (eq ?response s))
       then (assert (prioridad "El paciente necesita traslado y prioridad inmediata")) 
       else (assert (sangra no)))
 )
 
(defrule comprobar-pulso ""
   (not (pulso ?))
   (sangra no)
   (respira-bien si)
   (camina no)
   (herida-grave no)
   (not (prioridad ?))
   =>
   (assert (pulso (ask-question "¿Cómo es el pulso del paciente? (presente/ausente) " presente ausente)))
)

(defrule estado-mental ""
   (not (responde ?))
   (pulso presente)
   (sangra no)
   (respira-bien si)
   (camina no)
   (herida-grave no)
   (not (prioridad ?))
   =>
   (assert (responde (yes-or-no-p "¿El paciente esta responsivo y entiende ordenes simples? (si/no)? "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Conclusiones

(defrule si-puede-caminar ""
  (camina si)
  (not (prioridad ?))
  =>
  (assert (prioridad "Nivel 3: El paciente tiene prioridad baja, que camine a la zona de espera")))

(defrule gravemente-herido ""
  (gravemente-herido si)
  (not (prioridad ?))
  =>
  (assert (prioridad "Nivel X: El paciente se da por fallecido")))

(defrule no-respira-bien ""
  (respira-bien no)
  (not (prioridad ?))
  =>
  (printout t crlf)
  (printout t "Limpie y mantenga abierta las vías de respiración")
  (assert (prioridad "Nivel 1: El paciente necesita traslado y prioridad inmediata")))
  
(defrule pulso-ausente ""
  (pulso ausente)
  (not (prioridad ?))
   =>
  (assert (prioridad "Nivel 1: El paciente necesita traslado y prioridad inmediata")))

(defrule consciente-sin-peligro ""
  (responde si)
  (not (prioridad ?))
   =>
  (assert (prioridad "Nivel 2: El paciente esta estable, puede posponerse su traslado y tratamiento")))


(defrule no-responsivo ""
  (responde no)
  (not (prioridad ?))
  =>
  (assert (prioridad "Nivel 1: El paciente necesita traslado y prioridad inmediata")))


  