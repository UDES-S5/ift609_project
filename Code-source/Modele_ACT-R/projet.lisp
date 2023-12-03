(clear-all)

;;; Fonction pour placer les valises dans le coffre

(defun place-valises(n-times &optional (draw-valises nil))

   (setf moyenne 0)
   (dotimes (i n-times)
      (setf compteur 1)
      (setf not-win t)
      (setf res nil)
      (setf state nil)
      (setf *additional-valises-count* (act-r-random 4)) ;; generer un nombre aleatoire entre 0 et 3 pour le nombre de valises additionnelles
      (format t "Nombre de valises additionnelles: ~a~%" *additional-valises-count*)
      (setf *valises* (create-valises *additional-valises-count*))
      (while not-win ; appeler le modèle tant qu'il n'a pas win
         (loop for valise in *valises*
            do (setf (slot-value valise 'couche) 1))
         (let ((choix-model (show-model-valises *valises* res state))); Montre les valises au modèle et enregistre la key pressée par le model
            (when (string-equal "4" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (cadddr *valises*) 'couche) 2))) ; Met la quatrième valise en couche 2
			(when (string-equal "3" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (caddr *valises*) 'couche) 2)
               (setf state "weight-problem"))) ; Met la troisième valise en couche 2
                ;; mettre state à weight-problem si key 1
            (when (string-equal "2" choix-model) (progn
               (setf compteur (+ compteur 1))
               (setf (slot-value (cadr *valises*) 'couche) 2)
               (setf state "weight-problem-2"))) ; Met la deuxième valise en couche 2
            (when (string-equal "1" choix-model) (progn
               (setf compteur (+ compteur 1))
               (setf (slot-value (car *valises*) 'couche) 2))) ; Met la première valise en couche 2
            (setf res "win") ;; De base on win
            (setf poids-tot-couche-1 0)
            (setf poids-tot-couche-2 0)
            (loop for valise in *valises* ; boucle sur les valises choisi par le modèle
               do (if (= (slot-value valise 'couche) 1)
                     ;; Si la valise est couche 1
                     (setf poids-tot-couche-1 (+ poids-tot-couche-1 (slot-value valise 'poids))) ; Adition du poids de la valise
                     ;; Si la valise est couche 2
                     (setf poids-tot-couche-2 (+ poids-tot-couche-2 (slot-value valise 'poids))))) ; Adition du poids de la valise
            ;(setf choix-model "0")
            (if (> poids-tot-couche-2 poids-tot-couche-1)
               (setf res "lose") ; Si les valises en couche 2 sont plus lourdes -> lose
               (progn (setf not-win nil)
                      (unless (string-equal choix-model "0")(progn 
                        (setf state "final")
                        (show-model-result res state))))))
            (when draw-valises
            (loop for valise in *valises* do (print-valise valise))

            (format t "Niveau 1:~%")
			(setf nb 0)
            (loop for valise in *valises*
                do (when (= (slot-value valise 'couche) 1)
					(if (not (= (slot-value valise 'categorie) 1))
						(progn (draw-valise valise) (format t "~%"))
						(setf nb (+ nb 1)))))
			(cond
				((>= nb 4) (draw2little)(draw2little))
				((= nb 3) (draw2little)(draw1little))				
				((= nb 2) (draw2little))
				((= nb 1) (draw1little)))
			(setf nb 0)
            (format t "~%Niveau 2:~%")
            (loop for valise in *valises*
                do (when (= (slot-value valise 'couche) 2)
					(if (not (= (slot-value valise 'categorie) 1))
						(progn (draw-valise valise) (format t "~%"))
						(setf nb (+ nb 1)))))
			(cond
				((>= nb 4) (draw2little)(draw2little))
				((= nb 3) (draw2little)(draw1little))				
				((= nb 2) (draw2little))
				((= nb 1) (draw1little)))))

   (setf moyenne (+ moyenne compteur)))
   (/ (/ moyenne n-times) 3.0))

;; Todo: add the additional valises count to handle dynamically the number of valises on the mdodel side
(defun show-model-valises(valises &optional res state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
      (mod-focus-fct `(c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) 
                           p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids)
                           result , res
                           state , state
                           second-l, nil
						   reste 4))
      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) 
                                 p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids)
                                 result , res
                                 state , state
                                 second-l, nil
								 reste 4))))))
   
   (run-full-time 10) 
   *model-action*)

(defun show-model-result(res state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
      (mod-focus-fct `(result ,res
                           state ,state))
      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `((isa arrange-state result ,res
                                 state ,state))))))
                                 (run-full-time 10))

(defun run-blocks (blocks block-size)     
   (dotimes (i blocks)
      (setf retour (place-valises block-size)))
      retour)

(defun show-learning (n &optional (graph t))
   (let ((points))
      (dotimes (i n)
         (push (run-blocks 1 100) points)) ;; ici pour des blocs de 100
      (setf points (rev points))
      (when graph
         (draw-graph points))))


(defun rev(l)
           (cond
             ((null l) '())
             (T (append (rev (cdr l)) (list (car l)))))) 

(defun draw-graph (points)
  (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
    (allow-event-manager w)
    (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")
    (dotimes (i 11)
      (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (* (- 1 (* i .1)) 3)) :window "Data")
      (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data"))
    
    (let ((x 50))
      (mapcar (lambda (a b) (add-line-to-exp-window (list x (floor (- 410 (* a 400))))
                                                  (list (incf x 25) (floor (- 410 (* b 400))))
                                                    :color 'blue :window "Data"))
        (butlast points) (cdr points)))
    (allow-event-manager w)))


(defvar *model-action* nil) ;; La variable que le model devra remplir (liste de valise)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (if (eq win (current-device))
      (setf *model-action* (string key))
    (unless *human-action*
      (setf *human-action* (string key)))))

;;; Classe valise
(defclass valise()
   (poids
   categorie
   couche
   x
   y))

;;; permet d'afficher les infos d'une valise
(defgeneric print-valise (valise))
(defmethod print-valise ((la-valise valise))
   (format t "La valise pese: ~d, est de categorie ~d, mesure ~dx~d et est positionnee a la couche ~d~%" (slot-value la-valise 'poids) (slot-value la-valise 'categorie) (slot-value la-valise 'x) (slot-value la-valise 'y) (slot-value la-valise 'couche)))

;;; permet de dessiner une valise
(defgeneric draw-valise (valise))
(defmethod draw-valise ((la-valise valise))
   (format t " ")
   (dotimes (i (slot-value la-valise 'x))
      (format t "__")
   )
   (format t "~%")
   (dotimes (i (slot-value la-valise 'y))
      (format t "|")
      
      (dotimes (i (slot-value la-valise 'x))
         (format t "__")
      )
      (format t "|~%")
   )
)
(defun draw2little()
   (format t " ______  ______~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
)

(defun draw1little()
   (format t " ______ ~%")
   (format t "|______|~%")
   (format t "|______|~%")
   (format t "|______|~%")
)

;;; Fonction pour obtenir la taille restante du coffre apres avoir placé des valises (valise-list)
(defun calculate-remaining-space (valise-list)
  ;; todo: rendre plus générique en fonction de la taille du coffre (tenir compte de la hauteur q4)
  (let ((total-space 72) ;; 6*6*2
        (used-space 0))
      ;; Boucler sur les valises deja ajoutees et calculer l'espace utilise
      (loop for valise in valise-list
            do (setq used-space (+ used-space (* (slot-value valise 'x) (slot-value valise 'y))))
      )
      ;; Retourner l'espace restant
      (- total-space used-space)))

;;; Fonction pour créer des valises additionnelles (selon le nombre genere aleatoirement entre 0 et 3)
(defun create-additional-valises (valise-list number-additional-valises)
  (let ((new-valises nil))
      ;; Boucler sur le nombre de valises additionnelles et en ajouter le maximum possible
      (loop repeat number-additional-valises
            do (let ((new-valise (make-instance 'valise)))
                  ;; Assigner un poids et une catégorie aléatoirement
                  (setf (slot-value new-valise 'poids) (1+ (act-r-random 5)))
                  (setf (slot-value new-valise 'categorie) (1+ (act-r-random 3)))
                  (setf (slot-value new-valise 'couche) 1)
                  ;; Dimension selon la catégorie
                  (case (slot-value new-valise 'categorie)
                  (1 (progn (setf (slot-value new-valise 'x) 3) (setf (slot-value new-valise 'y) 3)))
                  (2 (progn (setf (slot-value new-valise 'x) 6) (setf (slot-value new-valise 'y) 2)))
                  (3 (progn (setf (slot-value new-valise 'x) 6) (setf (slot-value new-valise 'y) 3)))
                  )
                  ;; Valider si la valise peut être ajoutée sinon on l'ignore
                  (when (>= (calculate-remaining-space (append valise-list new-valises))
                           (* (slot-value new-valise 'x) (slot-value new-valise 'y)))
                  (push new-valise new-valises))
               )
         )
    (append valise-list new-valises)
   )
)

;; Fonction pour creer les valises, on cree 3 valises de base et on ajoute des valises additionnelles (selon le nombre genere aleatoirement entre 0 et 3)
(defun create-valises(count-to-add)
   ;; Création de l'instance des valises
   (defparameter *valise-1* (make-instance 'valise))
   (defparameter *valise-2* (make-instance 'valise))
   (defparameter *valise-3* (make-instance 'valise))

   (defvar valise-list(list *valise-1* *valise-2* *valise-3*)) ; ajout des valises dans une liste

   (loop for valise in valise-list ; boucle sur les valises
      do (progn
            (setf (slot-value valise 'poids) (1+ (act-r-random 5))) ;; poids aléatoire
            (setf (slot-value valise 'categorie) (1+ (act-r-random 3))) ;; categorie aléatoire
            (setf (slot-value valise 'couche) 1) ;; couche
            ;; Dimension selon la catégorie
            (case (slot-value valise 'categorie)
               (1 (progn (setf (slot-value valise 'x) 3) (setf (slot-value valise 'y) 3)))
               (2 (progn (setf (slot-value valise 'x) 6) (setf (slot-value valise 'y) 2)))
               (3 (progn (setf (slot-value valise 'x) 6) (setf (slot-value valise 'y) 3)))
            )
         )
   )

   (format t "Taille de la liste initiale: ~a~%" (length valise-list))
   (format t "Espace restant avant ajout: ~a~%" (calculate-remaining-space valise-list))

   ;; Ajout les valises additionnelles
   (setf v (create-additional-valises valise-list count-to-add))

   
   (format t "Taille de la liste apres ajout: ~a~%" (length v))
   (format t "Espace restant apres ajout: ~a~%" (calculate-remaining-space v))

   ;; Trier la liste selon le poids (decroissant) des valises (limite 3 du modele)
   (setf sorted-v (sort v #'(lambda (v1 v2) (> (slot-value v1 'poids) (slot-value v2 'poids)))))
   sorted-v); return valise-list




;;; Modèle ACT-R : 


(define-model baggage-organization

(sgp :esc nil :ans 0.1 :bll 0.5  :ncnar nil :pas nil :show-focus t :trace-detail low)

(install-device (open-exp-window "" :visible nil))

;; La variable second-l indique quelle valise se trouve sur le 2ème niveau (ex. second-l = 1, valise 1 est sur le deuxième niveau)
(chunk-type arrange-state c1 c2 c3 p1 p2 p3 second-l result state reste)
(chunk-type set4 v1 v2 v3 v4 result-set4)
(chunk-type set3 v1 v2 v3 result-set3)
(chunk-type set2 v1 v2  result-set2)

(chunk-type learned-info c1 c2 c3 p1 p2 p3 second-l)
(declare-buffer-usage goal arrange-state :all)

(define-chunks 

    (begin-model isa chunk)
    (remembering isa chunk) 
    (finish isa chunk) 
    (retrieving isa chunk) 
    (retrieving_2layers isa chunk) 
    (retrieving_2layers_2 isa chunk)
    (retrieving_2layers_3 isa chunk) 
    (comparing_weight isa chunk) 
    (comparing2 isa chunk)    

)


(add-dm
   (a ISA set4 v1 1 v2 1 v3 1 v4 1 result-set4 1111 )
   (b ISA set3 v1 1 v2 1 v3 1 result-set3 111 )
   (c ISA set3 v1 2 v2 2 v3 2 result-set3 222 )
   (d ISA set3 v1 1 v2 1 v3 2 result-set3 112 )
   (e ISA set3 v1 1 v2 2 v3 1 result-set3 112 )
   (f ISA set3 v1 2 v2 1 v3 1 result-set3 112 )
   (g ISA set3 v1 3 v2 1 v3 1 result-set3 113 )
   (h ISA set3 v1 1 v2 3 v3 1 result-set3 113 )
   (i ISA set3 v1 1 v2 1 v3 3 result-set3 113 )
   (j ISA set2 v1 1 v2 1 result-set2 11)
   (k ISA set2 v1 2 v2 2 result-set2 22)
   (l ISA set2 v1 3 v2 3 result-set2 33)
   (m ISA set2 v1 1 v2 2  result-set2 12)
   (n ISA set2 v1 2 v2 1  result-set2 12)
   (o ISA set2 v1 1 v2 3 result-set2 13)
   (p ISA set2 v1 3 v2 1 result-set2 13)
   (q ISA set2 v1 2 v2 3  result-set2 23)
   (r ISA set2 v1 3 v2 2 result-set2 23)
)
(p start
   =goal>
        isa arrange-state
        state nil
        c1  =a
        c2  =b
        c3  =c
        p1  =j
        p2  =k
        p3  =l
   ==>
   +retrieval> 
        isa learned-info
        c1  =a
        c2  =b
        c3  =c
        p1  =j
        p2  =k
        p3  =l
      - second-l nil
   =goal>
        state remembering
)
(p remember-organization
    =goal>
       isa arrange-state
       state remembering
    =retrieval>
       isa learned-info
       second-l =second-l
    ==>
    =goal>
       second-l =second-l
   +manual>
      cmd press-key
      key =second-l
)
(p doesnt-remember-organization
    =goal>
       isa arrange-state
       state remembering
    ?retrieval>
       buffer  failure
    ==>
     =goal>
        state begin-model
)
(p begin
   =goal>
      c1 =a
      c2 =b
      c3 =c
      state begin-model
   ==>
   +retrieval> 
      isa set4
      v1 =a
      v2 =b
      v3 =c
   =goal>
      state retrieving
)
(p success_3bags
   =retrieval>
    - buffer failure
   =goal> 
      state retrieving
	  reste 0
   ==>
   =goal>
      second-l "0"
      state "final"
      result "win"        
)
(p fail-3bags-1     
   ?retrieval>
      buffer  failure
   =goal>
      isa arrange-state 
      state retrieving
      c1  =a
      c2  =b
   ==>
   +retrieval>
      v4   =a
      v5   =b
   =goal>
      state retrieving_2layers
)
(p car-trunk
	=retrieval>
      result-first2 =p
   =goal>
      isa arrange-state
      state retrieving_2layers
	?manual>
      state free
   ==>
   =goal>
      second-l  "3"
      state comparing_weight
   +manual>
      cmd press-key
      key "3"
)
(p fail-3bag-2
   =goal>
      result "lose"
      state "weight-problem"
      c2  =a
      c3  =b
   ==>
   +retrieval>
      v4   =a
      v5   =b
   =goal>
      state retrieving_2Layers_2    
)
(p car-trunk-2
   =retrieval>
      result-first2 =val
   =goal>
      state retrieving_2Layers_2  
   ?manual>
      state free
   ==>
   =goal>
      second-l "1"
      state comparing2
   +manual>
      cmd press-key
      key "1"
)
(p fail-3bag-3
   ?retrieval>
      buffer  failure
   =goal>
      isa arrange-state 
      state "weight-problem-2"
      c1  =a
      c3  =b
   ==>
   +retrieval>
      v4   =a
      v5   =b
   =goal>
      state retrieving_2layers_3   
)
(p car-trunk-3
   =retrieval>
      result-first2 =val
   =goal>
      state retrieving_2layers_3
   ==>
   =goal>
      second-l "2"
      state "final"
      result "win"
   +manual>
      cmd press-key
      key "2"
)
(p memorize
    =goal>
        state "final"
        result "win"
        c1 =a
        c2 =b
        c3 =c
        p1 =l
        p2 =d
        p3 =e
        second-l =g
    ?imaginal>
        state free    
    ==>
    =goal>
        state finish
    +imaginal>
        c1 =a
        c2 =b
        c3 =c
        p1 =l
        p2 =d
        p3 =e
        second-l  =g
)
(p show-organization
   =goal>
      state finish
      result "win"
   ?manual>
      state free
    ==>
   +manual>
      cmd press-key
      key "0"
)
(p clear-new-imaginal-chunk
    ?imaginal>
        state free
        buffer full
    ==>
    -imaginal>
)

)