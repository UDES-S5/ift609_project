
(sgp :v nil  :esc nil :bll 0.5 :ncnar nil :rt 0 :show-focus t :trace-detail low )

 * Pour avoir un meilleur apprentissage, le modèle se focalise sur les parametres :ans et :rt

 * Aprés avoir essayé plusieurs valeurs on remarque que :

 - En prenant la valeur :ans (instantaneous noise) a 0,1 le modele devient de plus en plus performant et on obtient un meilleur apprentissage sans bruit
 - En fixant la valeur de :rt (retrieval threshold ) a 0 on remarque un meilleur apprentissage qui part d'une moyenne de tentatives à peu près 
   comprise entre 1.8 et 1.35 pour 20 blocs de 100 experiences

 * le fichier lisp "projet.lisp" contient le modele ACT-R ainsi que les commandes lisp nécessaires à son exécution.