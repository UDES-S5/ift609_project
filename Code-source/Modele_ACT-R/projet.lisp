(clear-all)

;;; Fonction pour placer les valises dans le coffre

(defun place-valises(n-times &optional (draw-valises nil))
   (setq sets '(1111 111 222 112 113 11 22 33 12 13 23 1 2 3)) ;Arrangements possibles sur un niveau
   (setf moyenne 0)
   (dotimes (i n-times)
      (setf compteur 1)
      (setf not-win t)
      (setf res nil)
      (setf state nil)
	  (setf level 1)
      (setf *additional-valises-count* (act-r-random 4)) ;; generer un nombre aleatoire entre 0 et 3 pour le nombre de valises additionnelles
      (format t "Nombre de valises additionnelles: ~a~%" *additional-valises-count*)
      (setf *valises* (create-valises *additional-valises-count*))
      (while not-win ; appeler le modèle tant qu'il n'a pas win
         ;(loop for valise in *valises*
          ;  do (setf (slot-value valise 'couche) 1))
		 (setq couche1 '())
		 (setq couche2 '())
		 
         (loop for valise in *valises* do (print-valise valise))

         (loop for valise in *valises*
			do (if(= (slot-value valise 'couche) 1) (push (slot-value valise 'categorie) couche1)
			(push (slot-value valise 'categorie) couche2)))

		 (setq couche1 (sort couche1 #'<))
		 (setq couche2 (sort couche2 #'<))
		 
		 (setq concatenated (parse-integer (format nil "~{~a~}" couche1)))
		 
		 (format t "Concat: ~a~%" concatenated)
		 (format t "Couche 1: ~a~%" couche1)
		 (format t "Couche 2: ~a~%" couche2)

		 (if (member concatenated sets)
			(format t "The item is in the list.~%")
			(format t "The item is not in the list.~%"))
			
         (let ((choix-model (show-model-valises *valises* res state))); Montre les valises au modèle et enregistre la key pressée par le model
            (when (string-equal "L" choix-model) (progn
               (if (= level 1)
					(progn 
						(setf level 2)
						(setf state "third-luggage-2"))
					(setf level 1)))) ;Switches the level
			(when (string-equal "6" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (nth 5 *valises*) 'couche) level) ; Met la sixième valise sur la couche level
			   (if (= level 1)
				  (setf state "switch-level")
				  (setf state "finish")))) 			   
			(when (string-equal "5" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (nth 4 *valises*) 'couche) level) ; Met la cinquième valise sur la couche level
			   (if (= level 1)
				  (setf state "sixth-luggage")
				  (setf state "sixth-luggage-2"))))
			(when (string-equal "4" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (cadddr *valises*) 'couche) level) ; Met la quatirème valise sur la couche level
			   (if (= level 1)
				  (setf state "fifth-luggage")
				  (setf state "fifth-luggage-2"))))
			(when (string-equal "3" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (caddr *valises*) 'couche) level) ; Met la troisième valise sur la couche level
               (if (= level 1) 
				  (setf state "fourth-luggage")
				  (setf state "fourth-luggage-2"))))  
                ;; mettre state à weight-problem si key 1
            (when (string-equal "2" choix-model) (progn
               (setf compteur (+ compteur 1))
               (setf (slot-value (cadr *valises*) 'couche) level) ; Met la deuxième valise sur la couche level
               (if (= level 1)
				  (setf state "third-luggage")
				  (setf state "third-luggage-2"))))  
            (when (string-equal "1" choix-model) (progn
               (setf compteur (+ compteur 1))
               (setf (slot-value (car *valises*) 'couche) level); Met la première valise sur la couche level
			   (if (= level 1)
				  (setf state "second-luggage")
				  (setf state "second-luggage-2")))) 
            (when (string-equal "F" choix-model) (progn
				(format t "Here")
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
							(show-model-result res state))))))))

            (when draw-valises
            ;; Affichage du message avec les caractéristiques des valises
            (loop for valise in *valises* do (print-valise valise))

            ;; Affichage du schéma des valises
            (format t "Niveau 1:~%")
			   (setf nb 0) ; Nombre de petites valises
            (loop for valise in *valises*
                do (when (= (slot-value valise 'couche) 1)
					(if (not (= (slot-value valise 'categorie) 1))
						(progn (draw-valise valise) (format t "~%"))
						(setf nb (+ nb 1)))))
            ; Affichage des petites valises
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
			   ; Affichage des petites valises
            (cond
				   ((>= nb 4) (draw2little)(draw2little))
				   ((= nb 3) (draw2little)(draw1little))				
				   ((= nb 2) (draw2little))
				   ((= nb 1) (draw1little)))))

   (setf moyenne (+ moyenne compteur)))
   (/ (/ moyenne n-times) 3.0))



;; (defun show-model-valises(valises &optional res state)

;;    (let* 
;;       ( (cc1 (slot-value (nth 0 valises) 'categorie)) 
;;         (cc2 (slot-value (nth 1 valises) 'categorie))
;;         (cc3 (slot-value (nth 2 valises) 'categorie))

;;         (cc4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'categorie) nil))
;;         (cc5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'categorie) nil))
;;         (cc6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'categorie) nil))

;;         (pp1 (slot-value (nth 0 valises) 'poids))
;;         (pp2 (slot-value (nth 1 valises) 'poids))
;;         (pp3 (slot-value (nth 2 valises) 'poids))

;;         (pp4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'poids) nil))
;;         (pp5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'poids) nil))
;;         (pp6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'poids) nil))

;;         (nl1 (slot-value (nth 0 valises) 'couche))
;;         (nl2 (slot-value (nth 1 valises) 'couche))
;;         (nl3 (slot-value (nth 2 valises) 'couche))

;;         (nl4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'couche) nil))
;;         (nl5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'couche) nil))
;;         (nl6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'couche) nil))
;;       )

;;       (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
;;          (mod-focus-fct `(c1 ,cc1  c2 ,cc2  c3 ,cc3  c4 , cc4  c5 , cc5  c6 , cc6  
;;                           p1 ,pp1  p2 ,pp2  p3 ,pp3  p4 , pp4  p5 , pp5  p6 , pp6
;;                           l1 ,nl1  l2 ,nl2  l3 ,nl3  l4 , nl4  l5 , nl5  l6 , nl6
;;                               result , res
;;                               state , state
;;                            )
;;          )
;;          (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
;;                               `((isa arrange-state c1 , cc1  c2 , cc2  c3 , cc3  c4 , cc4  c5 , cc5  c6 , cc6
;;                                                    p1 , pp1  p2 , pp2  p3 , pp3  p4 , pp4  p5 , pp5  p6 , pp6 
;;                                                    l1 , nl1  l2 , nl2  l3 , nl3  l4 , nl4  l5 , nl5  l6 , nl6
;;                                     result , res
;;                                     state , state))))
;;          )
;;       )
;;    )
   
;;    (setf *model-action-list* nil) ;; En théorie on doit le remettre à nil si on recommence
;;    (run-full-time 10)
;;    (reverse *model-action-list*)
;; )

(defun caddddr (x)
  (car (cddr (cddr (cdr x)))))




(defun show-model-valises(valises &optional res state)
   (if (= (length valises) 3)
      (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
         (mod-focus-fct `(c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 nil c5 nil c6 nil
                              p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 nil p5 nil p6 nil
                              l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 nil l5 nil l6 nil
                              result , res
                              state , state))
         (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                              `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 nil c5 nil c6 nil
                                    p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 nil p5 nil p6 nil
                                    l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 nil l5 nil l6 nil
                                    result , res
                                    state , state)))))
      )
      (if (= (length valises) 4)
         (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
            (mod-focus-fct `(c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 nil c6 nil
                                 p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 nil p6 nil
                                 l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 nil l6 nil
                                 result , res
                                 state , state))
            (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                                 `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 nil c6 nil
                                       p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 nil p6 nil
                                       l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 nil l6 nil
                                       result , res
                                       state , state)))))
         )
         (if (= (length valises) 5)
            (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
               (mod-focus-fct `(c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 ,(slot-value (nth 4 valises) 'categorie) c6 nil
                                    p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 ,(slot-value (nth 4 valises) 'poids) p6 nil
                                    l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 ,(slot-value (nth 4 valises) 'couche) l6 nil
                                    result , res
                                    state , state))
               (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                                    `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 ,(slot-value (nth 4 valises) 'categorie) c6 nil
                                          p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 ,(slot-value (nth 4 valises) 'poids) p6 nil
                                          l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 ,(slot-value (nth 4 valises) 'couche) l6 nil
                                          result , res
                                          state , state)))))
            )
            (if (= (length valises) 6)
               (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
                  (mod-focus-fct `(c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 ,(slot-value (nth 4 valises) 'categorie) c6 ,(slot-value (nth 5 valises) 'categorie)
                                       p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 ,(slot-value (nth 4 valises) 'poids) p6 ,(slot-value (nth 5 valises) 'poids)
                                       l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 ,(slot-value (nth 4 valises) 'couche) l6 ,(slot-value (nth 5 valises) 'couche)
                                       result , res
                                       state , state))
                  (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                                       `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie) c4 ,(slot-value (cadddr valises) 'categorie) c5 ,(slot-value (nth 4 valises) 'categorie) c6 ,(slot-value (nth 5 valises) 'categorie)
                                             p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids) p4 ,(slot-value (cadddr valises) 'poids) p5 ,(slot-value (nth 4 valises) 'poids) p6 ,(slot-value (nth 5 valises) 'poids)
                                             l1 ,(slot-value (car valises) 'couche)  l2 ,(slot-value (cadr valises) 'couche) l3 ,(slot-value (caddr valises) 'couche) l4 ,(slot-value (caddr valises) 'couche) l5 ,(slot-value (nth 4 valises) 'couche) l6 ,(slot-value (nth 5 valises) 'couche)
                                             result , res
                                             state , state))))))
                  (format t "Error: valises list length is not between 3 and 6.")
            )
         )
      )
   )
   
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

   ;;(format t "Taille de la liste initiale: ~a~%" (length valise-list))
   ;;(format t "Espace restant avant ajout: ~a~%" (calculate-remaining-space valise-list))

   ;; Ajout les valises additionnelles
   (setf v (create-additional-valises valise-list count-to-add))

   
   ;;(format t "Taille de la liste apres ajout: ~a~%" (length v))
   ;;(format t "Espace restant apres ajout: ~a~%" (calculate-remaining-space v))
   (setf sorted-v (sort v #'(lambda (v1 v2) (> (slot-value v1 'poids) (slot-value v2 'poids)))))
   sorted-v); return valise-list




;;; Modèle ACT-R : 


(define-model baggage-organization

(sgp :esc nil :ans 0.1 :bll 0.5  :ncnar nil :pas nil :show-focus t :trace-detail low)

(install-device (open-exp-window "" :visible nil))

;; La variable second-l indique quelle valise se trouve sur le 2ème niveau (ex. second-l = 1, valise 1 est sur le deuxième niveau)
(chunk-type arrange-state c1 c2 c3 c4 c5 c6 p1 p2 p3 p4 p5 p6 l1 l2 l3 l4 l5 l6 result state )

(chunk-type learned-info c1 c2 c3 c4 c5 c6 p1 p2 p3 p4 p5 p6 l1 l2 l3 l4 l5 l6)
(declare-buffer-usage goal arrange-state :all)

(define-chunks
    (begin-model isa chunk)
	(second-luggage isa chunk)
	(third-luggage isa chunk)
	(fourth-luggage isa chunk)
	(fifth-luggage isa chunk)
	(sixth-luggage isa chunk)
	(switch-level isa chunk)
	(third-luggage-2 isa chunk)
	(fourth-luggage-2 isa chunk)
	(fifth-luggage-2 isa chunk)
	(sixth-luggage-2 isa chunk)	
    (remembering isa chunk) 
    (finish isa chunk)
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
   ?manual>
      state free
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
(p first-luggage
   =goal>
      state begin-model
   ?manual>
      state free
   ==>
   =goal>
      state verifying
	  l1 1
   +manual>
      cmd press-key
      key "1"
)
(p second-luggage
   =goal>
      state "second-luggage"
   ?manual>
      state free
   ==>
   =goal>
      state verifying
	  l2 1
   +manual>
      cmd press-key
      key "2"
)
(p third-luggage
   =goal>
      state "third-luggage"
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "3"
)
(p fourth-luggage
   =goal>
      state "fourth-luggage"
	- c4 nil
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "4"
)
(p no-fourth-luggage
   =goal>
      state "fourth-luggage"
	  c4 nil 
   ==>
   =goal>
      state "switch-level"
)
(p fifth-luggage
   =goal>
      state "fifth-luggage"
	  - c5 nil
   ?manual>
      state free	  
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "5"
)
(p no-fifth-luggage
   =goal>
      state "fifth-luggage"
	  c5 nil
   ==>
   =goal>
      state "switch-level"
)
(p sixth-luggage
   =goal>
      state "sixth-luggage"
	  - c6 nil 
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "6"
)
(p no-sixth-luggage
   =goal>
      state "sixth-luggage"
	  c6 nil 
   ==>
   =goal>
      state "switch-level"
)
(p switch-level
   =goal>
      state "switch-level"
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "L"
)
(p third-luggage-2
   =goal>
      state "third-luggage-2"
	- l3 1
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "3"
)
(p skip-third-luggage-2
   =goal>
      state "third-luggage-2"
	  l3 1
   ==>
   =goal>
      state "fourth-luggage-2"
)
(p fourth-luggage-2
   =goal>
      state "fourth-luggage-2"
	- c4 nil
	- l4 1
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "4"
)
(p skip-fourth-luggage-2
   =goal>
      state "fourth-luggage-2"
	  l4 1
   ==>
   =goal>
      state "fifth-luggage-2"
)
(p no-fourth-luggage-2
   =goal>
      state "fourth-luggage-2"
	  c4 nil 
   ==>
   =goal>
      state "finish"
)
(p fifth-luggage-2
   =goal>
      state "fifth-luggage-2"
	- c5 nil 
	- l5 1
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "5"
)
(p skip-fifth-luggage-2
   =goal>
      state "fifth-luggage-2"
	  l5 1
   ==>
   =goal>
      state "sixth-luggage-2"
)
(p no-fifth-luggage-2
   =goal>
      state "fifth-luggage-2"
	  c5 nil 
   ==>
   =goal>
      state "finish"
)
(p sixth-luggage-2
   =goal>
      state "sixth-luggage-2"
	- c6 nil 
	- l6 1
   ?manual>
      state free
   ==>
   =goal>
      state finish
   +manual>
      cmd press-key
      key "6"
)
(p skip-sixth-luggage-2
   =goal>
      state "sixth-luggage-2"
	  l6 1
   ==>
   =goal>
      state "finish"
)
(p no-sixth-luggage-2
   =goal>
      state "sixth-luggage"
	  c6 nil 
   ==>
   =goal>
      state "finish"
)
(p verify
   =goal>
      state "finish"
    - result "win"
   ?manual>
      state free
   ==>
   =goal>
      state verifying
   +manual>
      cmd press-key
      key "F"
)
(p show-organization
   =goal>
      state "finish"
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