

(defun insertDim() 
	(format t "Please enter page dimension of board (number between 6 and 12): ")
	(let ((dim1 (read))) (cond 
							((and(<= dim1 12) (>= dim1 6)) (setq dim dim1) )
							(t (format t "Entered dimension is not in the range. ~%") (insertDim))
						)

	)
)

; pretvara brojeve u slova
(defun decASC (c)
  (code-char (+ 65 c)))
  
  
; pravi listu letter sa slovima
(defun listLetter()
  (setq letter0 '())
  (setq letter '())
  (loop for i from 0 to (- (* dim 2) 2) do  (push  (intern (string-upcase (decASC i))) letter0))
   (setq letter (reverse letter0)))


(defun SetAlist (letter r counter) ;Uzima slova iz liste letter i od njih pravi kljuceve za asocijativnu listu alist
	(cond 
		( (null letter) '())
		( t (cons
				(append (list (car letter))  (list (setq val1 (loop for i from 1 to counter collect '-))) ) ;U alist ubacuje najpre slovo iz liste letter, a zatim i odgovarajuci broj polja (-) 
				(SetAlist (cdr letter) (1+ r) (if (< r (- dim 2)) (1+ counter) (1- counter))) ;Rekurzivno se poziva f-ja nad ostatkom liste
			)			
		)
	)
)

;U enteredX i enteredO se ubacuju potezi koje igrac X, odnosno O odigra u formatu ((A (0)) (B (1 2 3)) (C ())....)
(defun SetEnteredX (letter r counter) 
	(cond ( (null letter) '()) 
		( t (cons
				(append (list (car letter))  (list '()))  ;U enteredX ubacuje najpre slovo iz liste letter, a vrednost je inicijalno nil 
				(SetEnteredX (cdr letter) (1+ r) (if (< r (- dim 2)) (1+ counter) (1- counter))) ;Rekurzivno se poziva f-ja nad ostatkom liste
			)			
		)
	)
)
	
(defun SetEnteredO (letter r counter)
	(cond ( (null letter) '()) 
		( t (cons
				(append (list (car letter))  (list '()))  ;U enteredO ubacuje najpre slovo iz liste letter, a vrednost je inicijalno nil 
				(SetEnteredO (cdr letter) (1+ r) (if (< r (- dim 2)) (1+ counter) (1- counter))) ;Rekurzivno se poziva f-ja nad ostatkom liste
			)			
		)
	)
)	

;unentered inicijalno svaki kljuc sadrzi sve brojeve, a nakon odigranog poteza, taj broj se izbacuje
(defun SetUnentered (letter r counter)
	(cond ( (null letter) '()) 
		( t (cons
				(append (list (car letter))  (list (setq val1 (loop for i from 1 to counter collect (1- i)))) ) ;U unentered ubacuje najpre slovo iz liste letter, a vrednost je inicijalno lista svih brojeva, odnosno pozicija za to slovo 
				(SetUnentered (cdr letter) (1+ r) (if (< r (- dim 2)) (1+ counter) (1- counter))) ;Rekurzivno se poziva f-ja nad ostatkom liste
			)			
		)
	)
)

;unenteredNodes - lista unentered u formatu ((cvor1) (cvor2) ...)
(defun initUnentered (unentered)
	(cond
			( (null unentered) '())
			( t (append
					(append (loop for i from  0 to (1- (length (cadar unentered))) collect (list (caar unentered) (nth i (cadar unentered)))))
					(initUnentered (cdr unentered))
				)
					
			)
	)
)

(defun initFirstMove ()
	(if (not (null (find (car bridge) unenteredNodes :test 'equal)))
		(list (first letter) 1)
		(list (car (last letter)) dim)
	)
)


(defun newGame() ;Postavljanje svih listi i promenljivih na inicijalne vrednosti u zavisnosti od unete dimenzije + pocetak igre - poziv f-je havannah
		
	(insertDim) ;Unos dimenzije
	(listLetter) ;Inicijalizuje listu slova - letter u zavisnosti od dimenzije

	;inicijalizacija
	(setq alist (SetAlist letter -1 dim))
	(setq alistPrint (SetAlist letter -1 dim))

	(setq enteredX (SetEnteredX letter -1 dim))
	(setq enteredO (SetEnteredO letter -1 dim))
	(setq unentered (SetUnentered letter -1 dim))
	
	(setq endgame '())
	(setq player '((X O) (O X)))
	(setq endMove (list (first letter) (1- (length (cadr (assoc (first letter) alist))))))	
	
	;za proveru figura
	(setq playedMoves '0) ;playedMoves broji koliko je poteza odigrano, potrebno je najmanje 11 poteza zajedno da bi se sastavila najkraca figura-ring, pre toga nije potrebno da se vrsi provera za figure
	(setq edges '())
		
	(setq bridge (append (list 	 ;Izvalcimo temena za proveru
		(list (first letter) 0) (list (first letter) (1- dim)) ;Na primer, za dimenziju 8, temena (A 0) (A 7) 
		(list (nth (1- dim) letter) 0) (list (nth (1- dim) letter) (- (* 2 dim) 2)) ; (H 0) (H 14)
		(list (nth (- (* 2 dim) 2) letter) 0) (list (nth (- (* 2 dim) 2) letter) (1- dim)))) ;(O 0) (O 7)
	) 
	
	(setq fork (setfork unentered))
	
	(initSides)
	
	
	;pocetak igre
	(havannah)
)



(defun havannah ()
	(setq opponent '())
	(format t "~%If you want for opponent to play first please enter 'C': ")
	(setq first (read))
    (if (equal first 'C) (setq opponent 't))
	(if opponent (progn (setq opp 'X)(format t "Opponent is first player ~% ")) (progn (setq opp 'O)(format t "You are first player ~% ")))

    (showField alistPrint (1+ dim) 1)

	(setq input '())
	(loop while (not (equal endgame 't)) do 
		(format t "~%Player ~a - " (caar player))
		(format t "Please enter your move in format (letter number): ")
		
		;igra opponent
		(when (equal (caar player) opp) 
			(setq input (read))
			(if (equal input 'Q) 
				(return))
			

		)
		
		;igra covek
		(when (not (equal (caar player) opp)) 
			(setq input (read))
			(if (equal input 'Q) 
				(return))
		)
		
		(insertMove input player playedMoves)
		(format t "~%")
		(showField alistPrint (1+ dim) 1)
		
		(if (equal endgame 't) 
			(return)
		)
		
		(setq player (reverse player))
		(setq playedMoves (1+ playedMoves))
	)
	(endGame player)
)


(defun endGame (player)
	(if (equal endgame 't) 
		(format t "Player ~a has won! ~%" (caar player) )
		(format t "You ended game! ~%")
	)	
	(format t "~%Do you want to start a new game? Y/N:   ")
	(setq input (read))
	(if (equal input 'Y)
		(newgame)
		(excl:exit) ;izlazi iz allegra	
	)
)


; funkcija za prikaz novog reda 
(defun printNewLIne ()
  (format t "~%"))
  
(defun printSpace (n) ;Stampa odgovarajuci broj razmaka za prikaz tabele
  (cond
	   ( (equal n 0) (format t ""))
	   ( (equal n 1) (format t " "))
	   (t (format t " ") (printSpace(- n 1)))
  )
)

(defun showField(alista n1 count)
               (cond
					( (= (length alista) 0)  '())
					
					( (= n1 (+ 1 dim))  ;Ukoliko je prvi red (slova)
							(printSpace n1) ;Postavlja prvi tab za slova
							(loop for i from 0 to (1- dim) do (format t "~a " (nth i letter))) ;Postavlja prvih n slova,u prvom redu
									(format t "~%") ;Novi red nakon postavljanja slova
									(showField alista  (- n1 2 ) count)
					)
					
				  
					 ( t
							(setq temp (car (cdr (car alista)))) ;U pomocnu promenljivu temp stavljamo crtice
							(if (AND (= n1 (- dim 6)) (= count (+ dim 2) )) 
								(setq n1 (- n1 1)) 							
							)
							(printSpace n1) ;Tab						
							(format t "~a " count ) ;Prikaz brojeva
							(loop for i from 0 to (1- (length temp)) do 
								(format t "~a " (nth i temp)) ;Stampa elemente iz asocijativne liste (-)
							) 
							(if (> (length alista) dim) 
								(format t "~a" (nth (length temp) letter))  ;Stampa slova sa desne strane
							)
							(format t "~%") 
									 
							(if (AND (< count dim)  (> n1 0)) 
								(showField (cdr alista)  (- n1 1) (+ 1 count))
								(showField (cdr alista)  (+ n1 1) (+ 1 count))
							)
					)
				)
)		
	


;Provera validnosti unetog poteza
(defun inputValidate (input) 
		(if
				(and 			 
					(= (length input) 2) ;Da li su unete 2 vrednosti
					(assoc (car input) alist)  ;Da li je prva vrednost slovo iz asocijativne liste				 
					(> (car (cdr input)) 0)  ;Da li je broj veci od 0
					(if (>= (position (car input) letter :test #'equal) dim ) ;Da li je indeks slova u letter listi veci od dimenzije
							(AND (< (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) )) (length (car (cdr (assoc (car input) alist))))) ;Gornja granica - da li je input manje od vrednosti koja postoji u alist
								(> (cadr input) (- (1- (* 2 dim)) (length (car (cdr (assoc (car input) alist))))) ) ;Donja granica
							)
							
							(<= (cadr input) (length (car (cdr (assoc (car input) alist))))) );Da li je broj manji ili jednak broju elemenata vrste
					(if (>= (position (car input) letter :test #'equal) dim ) ;Da li je indeks slova u letter listi veci od dimenzije
							(equal (nth (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) ))  (car (cdr (assoc (car input) alist)))) '-)
							(equal (nth (1- (cadr input)) (car (cdr (assoc (car input) alist)))) '-) ) ;Da li postoji figura na tom polju
					 
				)
			input			
			
		) 
)


;Unos poteza
(defun insertMove (input player playedMoves) 

	(cond 
			( (equalp input (inputValidate input))	;Proverava validnost upisa
			
							(if (>= (position (car input) letter :test #'equal) dim ) ;Da li je indeks slova u letter listi veci od dimenzije
								(replace (cadr(assoc(car input) alist)) (car player) ;Menja (-) sa X ili O na odgovarajucoj poziciji
									:start1 (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) )) :end1  (1+ (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) ))) :start2 0) 
								(replace (cadr(assoc(car input) alist)) (car player) :start1 (1- (cadr input)) :end1 (cadr input) :start2 0)
							) 
							(if (equal (caar player) 'X)
								(if (>= (position (car input) letter :test #'equal) dim)
									(push (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) )) (cadr(assoc(car input) enteredX)))
									(push (1- (cadr input)) (cadr(assoc(car input) enteredX)) ))
								(if (>= (position (car input) letter :test #'equal) dim)
									(push (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) )) (cadr(assoc(car input) enteredO)))
									(push (1- (cadr input)) (cadr(assoc(car input) enteredO)) ))

							)
							
							;Ubacivanje u listu za stampanje
							(if (< (cadr input) dim) ;uslov za racunanje offset-a za zamenu
								(replace (cadr (assoc (nth (1- (cadr input)) letter) alistprint)) ;menjamo element iz alistprint
										(car player) ;odgovarajucim elementom iz alist
										:start1 (position (car input) letter)  :end1 (1+ (position (car input) letter)) 
										:start2 0)
								(replace (cadr (assoc (nth (1- (cadr input)) letter) alistprint)) ;menjamo element iz alistprint
										(car player) ;odgovarajucim elementom iz alist
										:start1 (+ (- dim (position (car (assoc (nth (1- (cadr input)) letter) alistprint)) letter)) (1- (position (car input) letter)))
												:end1 (1+ (+ (- dim (position (car (assoc (nth (1- (cadr input)) letter) alistprint)) letter)) (1- (position (car input) letter))))
	 
										:start2 0)
							)
							
							
							(setq unenteredNodes (initUnentered unentered))
							
							(if (equal (caar player) 'X) 
								(setq ent enteredX) 
								(setq ent enteredO)
							)
							
							(setq inputScaled '())
							(if (>= (position (car input) letter :test #'equal) dim)
									(push (- (cadr input) (- (* 2 dim) (length (car (cdr (assoc (car input) alist)))) )) inputScaled)
									(push (1- (cadr input)) inputScaled)
							)
							(push (car input) inputScaled)
							
							
							;Dodaje u enteredSides
							;Proveravamo da li input pripada listi stranica
							;ako pripada, dodajemo input u listu svih unetih strana
							(when (not (null (find (cadr inputScaled) (cadr (assoc (car inputScaled) fork))))) ;T -> input se nalazi u fork list
								(if (equal (caar player) 'X) 
									(push inputScaled enteredSidesX)
									(push inputScaled enteredSidesO)
								)								
								;(format t "~a" enteredsides)
								(checkFork inputScaled player) ;Proveravamo na kojoj strani setougla je input
							)
							
							(if (equal (caar player) 'X) 
								(setq entSides enteredSidesX) 
								(setq entSides enteredSidesO)
							)							
														
							(if (>= playedMoves 9)
								(checkFigures inputScaled player ent entSides)
							) 
			)
			
			(t		
					(when (equal (caar player) opp) 
						(format t "~%Invalid move: ~A. Please enter your move in format (letter number): " input)
							(setq input (read))							
					)
					(when (not (equal (caar player) opp))					
							(format t "~%Invalid move: ~A. Please enter your move in format (letter number): " input)
							(setq input (read))							
					)
					(insertMove input player playedMoves)
			)
			
		)
)

		

(defun neighbours (node entered) ;Funkcija nalazi listu svih popunjenih suseda ;Prosledjuje se skalirani cvor
	;(cadr node) broj iz node npr. 3
	;(cadr (assoc (car node) enteredX/enteredO)) odgovrajuci brojevi iz slova iz node, npr. (4 3 2)
	;
	;Lista indexes sadrzi indekse cvorova za koje nalazimo susede
	(setq indexes (list (1- (position (assoc (car node) entered) entered)) (position (assoc (car node) entered) entered) (1+ (position (assoc (car node) entered) entered))))
	
	
	(setq nList '()) ;Lista suseda cvora node
	(setq firstHalf (list (list (1- (cadr node)) (cadr node)) (list (1- (cadr node)) (1+ (cadr node))) (list (cadr node) (1+ (cadr node))))) ;elementi koje treba da trazimo u prvoj polovini tabele
	(setq middle (list (list (1- (cadr node)) (cadr node)) (list (1- (cadr node)) (1+ (cadr node))) (list (1- (cadr node)) (cadr node)))) ;elementi koje treba da trazimo na sporednoj dijagonali
	(setq secondHalf (list (list (cadr node) (1+ (cadr node))) (list (1- (cadr node)) (1+ (cadr node))) (list (1- (cadr node)) (cadr node)) )) ;elementi koje treba da trazimo u drugoj polovini tabele

	(loop for i from 0 to (1- (length indexes))  do  ; i=0 levi susedi, i=1 susedi u njegovoj listi, i=2 desni susedi
		(if (AND (>= (nth i indexes) 0 ) (< (nth i indexes) (1- (* dim 2) ))) ;Provera da li indeksi ne izlaze iz opsega tabele
			(cond 
				((AND (>= (nth i indexes) 0) (< (nth i indexes) (1- dim))) ;Da li je indeks u prvoj polovini tabele
					(if (not (listp (find (car (nth i firstHalf)) (cadr (nth (nth i indexes) entered))))) ;gornji sused
						   (push (list (car (nth (nth i indexes) entered)) (find (car (nth i firstHalf)) (cadr (nth (nth i indexes) entered))))nList))
					(if (not (listp (find (cadr (nth i firstHalf)) (cadr (nth (nth i indexes) entered))))) ;donji sused
						   (push (list (car (nth (nth i indexes) entered)) (find (cadr (nth i firstHalf)) (cadr (nth (nth i indexes) entered))))nList))
					)
				
				((= (nth i indexes) (1- dim)) ;Da li je indeks na sporednoj dijagonali
					(if (not (listp (find (car (nth i middle)) (cadr (nth (nth i indexes) entered))))) ;Ukoliko je nasao element
						(push (list (car (nth (nth i indexes) entered)) (find (car (nth i middle)) (cadr (nth (nth i indexes) entered)))) nList))
					(if (not (listp (find (cadr (nth i middle)) (cadr (nth (nth i indexes) entered))))) ;Ukoliko je nasao element
						(push (list (car (nth (nth i indexes) entered)) (find (cadr (nth i middle)) (cadr (nth (nth i indexes) entered)))) nList))
				)
				(t ;Ako je indeks u drugoj polovini tabele
					(if (not (listp (find (car (nth i secondHalf)) (cadr (nth (nth i indexes) entered))))) ;Ukoliko je nasao element
						(push (list (car (nth (nth i indexes) entered)) (find (car (nth i secondHalf)) (cadr (nth (nth i indexes) entered)))) nList))
					(if (not (listp (find (cadr (nth i secondHalf)) (cadr (nth (nth i indexes) entered))))) ;Ukoliko je nasao element
						(push (list (car (nth (nth i indexes) entered)) (find (cadr (nth i secondHalf)) (cadr (nth (nth i indexes) entered)))) nList))
				)
			)
        )
	)
               nlist
)



(defun initConnected (s e entered) ;proverava lanac za starni i krajnji cvor

	(setq untreated '()) ;neobradjeni cvorovi
	(setq processed '()) ;obradjeni cvorovi
	(setq temporary '())
	(push (neighbours s entered) untreated) ;u neobradjene cvorove smestaju se susedi startnog cvora u formatu (((letter number) (....)))
	(setq untreated (car untreated))
	(push s processed) ;startni cvor smestamo u obradjene 
	(if (null untreated) '()
		(connected e entered untreated processed))
		
)		




;Proveraca da li postoji lanac izmedju pocetnog i krajnjeg cvora
(defun connected (e entered untreated processed)
(setq temporary '())
		(cond 
			((null untreated) '())
			( (AND (equal (car untreated) e) (>= (length processed) 4) ) T)
		
			(t  (push (neighbours (car untreated) entered) temporary)
				(setq temporary (car temporary)) 
				(loop for i from 0 to (1- (length temporary)) do
					(setq rev (reverse untreated))
				 
					(when (AND (null (find (nth i temporary) processed :test 'equal)) (null (find (nth i temporary) untreated :test 'equal)))  ;ukoliko ne posotji u processed ili untreated 
						(push (nth i temporary) rev) 
					
						(setq untreated (reverse rev))
			
					)
				)
				(push (car untreated) processed)
				(setq temporary '())
				(connected e entered  (cdr untreated) processed)
			)
		)
)
		

;BRIDGE
		
(defun checkBridge (player bridge entered) ;Proverava koliko temena je uneseno i vraca listu tih temena
    (cond ((null bridge) edges)
          (t ;provera se za igraca X/O da li se ima unetih njegovih figura na najmanje dva temena
				(let (
                   (enter (assoc (caar bridge) entered))) ;Izvlace se odigrani potezi za potrebno slovo, nrp. (A (0-7))
                   (if (not (listp (find (cadar bridge) (cadr enter)))) (cons (append (car bridge) edges) 
				   (checkBridge player (cdr bridge) entered)) (checkBridge player (cdr bridge) entered))
				)	
		   )		   
    )					   
)

(defun initBridge (entE entered)
	(setq processedB (list (car entE)))
	(setq untreatedB (neighbours (car entE) entered))
	(bridgeFigure entE entered untreatedB processedB)
)


(defun bridgeFigure (entE entered untreatedB processedB) ;argument je lista svih ivica koje su nadjene
	(cond 
			((null entE) '())
			(t  (setq start (car entE))
				(loop for i from 1 to (1- (length  entE)) do 
					(if (initConnected start (nth i entE) entered)  ; start end entered
						(setq mark 'T)
						(setq mark '())
					)		
				)
				(if mark 
					't 
					(bridgeFigure (cdr entE) entered untreatedB processedB)
				)
			)	
	)
)
		
;RING 

(defun initRing (start entered) ;start - cvor koji smo uneli sa insertMove
	(setq listNeighbours (neighbours start entered)) ;Dodaje u listNeighbours sve susede startnog cvora
	(setq processedR (list start))
	(ringFigure start listNeighbours entered processedR)
)



(defun ringFigure (start listNeighbours entered processedR) 
	(cond
			( (null listNeighbours) '()) 
			( t (setq untreatedR listNeighbours) 
				(setq untreatedR (remove (car listNeighbours) untreatedR)) ;Neobradjeni cvorovi za startni su svi susedi osim krajnjeg cvora (car listNeighbours)
				(if (connected (car listNeighbours) entered untreatedR processedR) ; initConnected start (car listNeighbours) entered
					T 
					(ringFigure start (cdr listNeighbours) entered (push (car  listNeighbours) processedR))
				)
			)
	
	)
)	
		

		
; FORK

(defun setFork (unentered)
	(cond 
			( (null unentered) '())
			( (OR (equal (position (caar unentered) letter) 0) (equal (position (caar unentered) letter) (- (* dim 2) 2) )) ;Ukoliko je indeks slova 0 ili 2dim-2 (za dim 8: slova A i O)
				(cons
					(append (list (caar unentered)) (list (setq val (loop for i from 1 to (- (length (cadar unentered)) 2) collect i))))	
					(setFork (cdr unentered))
				)
			)
			
			( (equal (position (caar unentered) letter) (1- dim)) ;Ukoliko je indeks slova dim-1 (za dim 8: slovo H)
				(cons
					(append (list (caar unentered)) (list (setq val '())))
					(setFork (cdr unentered))
				)
			)
			
			( t ;Za ostala slova (za dim 8: slova B-G i I-N)
				(cons
					(append (list (caar unentered)) (list(cons (first (cadar unentered)) (last (cadar unentered)))))
					(setFork (cdr unentered))
				)
			)
	
	)

)

(defun initSides () 
	(setq enteredSidesX '())
	(setq enteredSidesO '()) 
	(setq up '(0 0)) 
	(setq upRight '(0 0))
	(setq downRight '(0 0))
	(setq down '(0 0))
	(setq downLeft '(0 0))
	(setq upLeft '(0 0))
	
)

(defun permutations (lista k) ;Pronalaze se sve permutacije velicine 3 za sve unete cvorove na stranama setougla 
  (cond ((equal 0 k) (list nil))
        ((null lista) nil)
        ((null (cdr lista)) (list lista))
        (t (loop for element in lista
             append (mapcar (lambda (l) (cons element l))
(permutations (remove element lista) (- k 1)))))))


(defun checkFork (inputScaled player) ;Setuje promenljive, koje oznacavaju stranice setougla, na 1 ukoliko je odigran potez na toj stranici
	(cond
		( (equal (position (car inputScaled) letter) 0) ;A1-A6
			(if (equal (caar player) 'X)
				(setq upLeft (replace upLeft '(1) :start1 0 :end1 1 :start2 0))
				(setq upLeft (replace upLeft '(1) :start1 1 :end1 2 :start2 0))
			)
		) 
		( (equal (position (car inputScaled) letter) (- (* dim 2) 2)) ;O1-O6
			(if (equal (caar player) 'X)
					(setq downRight (replace downRight '(1) :start1 0 :end1 1 :start2 0))
					(setq downRight (replace downRight '(1) :start1 1 :end1 2 :start2 0))
			)
		)
		( (AND (> (position (car inputScaled) letter) 0) (< (position (car inputScaled) letter) (1- dim))) ;B-G
			(if (equal (cadr inputScaled) 0) ;B0-G0
				(if (equal (caar player) 'X)
					(setq up (replace up '(1) :start1 0 :end1 1 :start2 0))
					(setq up (replace up '(1) :start1 1 :end1 2 :start2 0))
				)
				;B8-G13
				(if (equal (caar player) 'X)
					(setq downLeft (replace downLeft '(1) :start1 0 :end1 1 :start2 0))
					(setq downLeft (replace downLeft '(1) :start1 1 :end1 2 :start2 0))
				)
 
			)
		)
		( (AND (> (position (car inputScaled) letter) (1- dim)) (< (position (car inputScaled) letter) (- (* 2 dim) 2))) ;I-N
			(if (equal (cadr inputScaled) 0) ;I0-N0
				(if (equal (caar player) 'X)
					(setq upRight (replace upRight '(1) :start1 0 :end1 1 :start2 0))
					(setq upRight (replace upRight '(1) :start1 1 :end1 2 :start2 0))
				)
				;I13-N8
				(if (equal (caar player) 'X)
					(setq down (replace down '(1) :start1 0 :end1 1 :start2 0))
					(setq down (replace down '(1) :start1 1 :end1 2 :start2 0))
				)
			)
		)
	)
)





(defun initFork (combinations entered) 
	(cond
		( (null combinations) '())
		( (>= (length (car combinations)) 3)
			(if (forkFigure (nth 0 (car combinations)) (nth 1 (car combinations)) (nth 2 (car combinations)) entered)
				'T
				(initFork (cdr combinations) entered)
			)
		)
		(t '())
	)
)
(defun forkFigure (a b c entered) ;Za tri tacke na tri razlictim stranicama proverava da li je formiran Fork
	(cond
		( (AND (initConnected A B entered) (initConnected B C entered)) 'T)
		( (AND (initConnected A B entered) (initConnected A C entered)) 'T)
		( (AND (initConnected A C entered) (initConnected C B entered)) 'T)
		( t '())
	)	
)


;Poziva sve f-je za proveru figura
(defun checkFigures (inputScaled player entered enteredSides)
	
	;proveravamo BRIDGE
	(setq enteredEdges (checkBridge player bridge entered)) ;U promenljivu enteredEdges smestamo rezultat izvrsenja f-je checkBridge (edges promenljivu)
	(if (>= (length enteredEdges) 2) 
		(when (initBridge enteredEdges entered) 
			(setq endgame 't)
			(format t "END OF GAME - BRIDGE FORMED ~%")
		)
	)

	;FORK
	;proveravamo da li je br dotaknutih str > 3
	;ako jeste proveravamo fork pozivom f-je initFork
	(setq combinations (permutations enteredsides 3))
	(cond 
		( (equal (caar player) 'X)
			(if (>= (+ (car up) (car upRight) (car downRight) (car down) (car downLeft) (car upLeft)) 3) ;Neophodno je da su uneta polja na najmanje tri razicite stranice kako bi se aktivirala provera za Fork
				(when (initFork combinations entered) 
					(setq endgame 't)
					(format t "END OF GAME - FORK FORMED ~%"))
			)
		)
		(t 
			(if (>= (+ (cadr up) (cadr upRight) (cadr downRight) (cadr down) (cadr downLeft) (cadr upLeft)) 3) ;Neophodno je da su uneta polja na najmanje tri razicite stranice kako bi se aktivirala provera za Fork
				(when (initFork combinations entered) 
					(setq endgame 't)
					(format t "END OF GAME - FORK FORMED ~%"))
			)
		)
	)
	
	;RING
	(when (initRing inputScaled entered)
		(setq endgame 't)
		(format t "END OF GAME - RING FORMED ~%")
	)
)


;ispituje da li je prazna tabela
(defun empty (enteredx)
                (cond ((null enteredx) 'T)
                      ((null (cadar enteredx))
                          (empty (cdr enteredx)))
                      (t '())
			    )
)
					  






(newgame)

