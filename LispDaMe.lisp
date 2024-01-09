;;; come struttura dati per la memorizzazione delle classi viene usata una hash-table
(defparameter *classes-specs* (make-hash-table))

;;; i metodi add-class-spec e ge si occupano rispettivamente di:
;;; aggiungere una nuova classe 
;;; ottenere una classe 
(defun add-class-spec (name class-spec)
;;; cambia il valore in una posizione
  (setf (gethash name *classes-specs*) class-spec))

(defun get-class-spec (name)
  (gethash name *classes-specs*))

;;; def-class definisce la struttura di una classe e la memorizza
(defun def-class (class-name parents &rest parts)
;;; controlli per eventuali valori imprevisti
  (cond ((not (atom class-name))
	 (error (format nil "Def-class: unable to create the class -> ~a is not an atom" class-name)))
	((equal class-name '())
	 (error
	  (format nil
	   "Def-class: unable to create the class -> ~a is an empty list"
	   class-name)))
	((null class-name)
	 (error
	  (format nil
		  "Def-class: unable to create the class -> ~a is NULL"
		  class-name)))
	((not (listp parents))
	 (error
	  (format nil
		  "Def-class: unable to create the class -> ~a is an not a list"
		  parents)))
	((is-class class-name)
	 (error
	  (format nil
		  "Def-class: unable to create the class -> ~a already exists"
		  class-name)))
	((and parents
	      (not (null (member nil (mapcar #'is-class parents)))))
	 (error
	  (format nil
		  "Def-class: unable to create the class -> some of the parents are not existing classes")))
	((member class-name parents)
	 (error
	  (format nil
		  "Def-class: unable to create the class -> ~a is also present in its parents' list ~a"
		  class-name parents))))
  (parent* class-name parents)
;;;creazione della classe
  (add-class-spec class-name
		  (append (list class-name)
			  (append (list parents)
				  (parts-structure parents parts))))
  class-name)

;;;classe student (person something)
;;;append (person something) to (get-parents-list (person something))
(defun get-class-parents (class-name)
  (nth 1 (get-class-spec class-name)))

(defun parent (searched-class-name class-name)
  (cond
    ((null (get-class-parents class-name)) nil)
    ((member searched-class-name (get-class-parents class-name))
     (error (format nil "Def-class: A class cannot be anchestor of itself.")))
    (T (parent* searched-class-name (get-class-parents class-name)))))

(defun parent* (searched-class-name parents)
  (when (and parents (not(parent searched-class-name (car parents))))
    (parent* searched-class-name (cdr parents)))
  (if (null parents) nil T))

;;; is-class: restituisce T se il valore in input è il nome di una classe
(defun is-class (class-name)
  (if (get-class-spec class-name) T))    

;;; is-instance: restituisce T se il valore in input è il nome di una istanza
;;; di una classe
(defun is-instance (value &optional (class-name T)) ;;; value è l'istanza
  (cond ((equal (car value) 'OOLINST)
	 (cond
;;;class-name è T -> value può essere istanza qualunque
	   ((equal class-name 'T)   T)
;;;class-name è un simbolo ->controllo che class-name 
;;;sia superclasse della classealla quale appartiene value
	   ((member class-name (cadr (get-class-spec (cadr value)))) T)))))

(defun parts-structure (parents parts)
  (if (= (list-length parts) 0)
      (error (format nil "parts-structure: list-length is equal to zero"))
      (cons
       (fields-structure parents (car parts))(list (methods-structure parents (cadr parts))))))

(defun fields-structure (parents fields)
  (when fields
    (cons
     (field-definition parents (car fields))
     (fields-structure parents (cdr fields)))))

(defun field-definition (parents current-field)
  (cond ((equal 'fields current-field)
	 'fields)
	((not (equal 'fields current-field))
	 (let ((value (length current-field)))
	   (ecase value
	     (1  (list (car current-field) 'NIL 'T))
	     (2  (list (car current-field) (cadr current-field) 'T))
;;;controllo sul tipo del campo
	     (3  (cond ((and
			 (not (equal
			       (caddr current-field) 'number))
			 (not (equal
			       (caddr current-field) 'integer))
			 (not (equal (caddr current-field) 'string))
			 (not (equal (caddr current-field) 'list))
			 (not (equal (caddr current-field) 'float))
			 (not (is-class (caddr current-field))))
			(error
			 (format nil
				 "Field-definition: ~a is not a Common-Lisp type or an existing class"
				       (caddr current-field))))
		       ((not
			 (typep (cadr current-field)
				(caddr current-field)))
			(error
			 (format nil "Field-definition: value ~a of field ~a is not of the type specified (~a)"
				       (cadr current-field) (car current-field) (caddr current-field))))
		       ((and (not (null parents))
			     (member (car current-field)
				     (get-fields-name-of-class
				      (get-parents-fields-plus-value-and-type parents)))
			     (not (subtypep   (caddr current-field)
					      (caddr (find (car current-field)
							   (get-parents-fields-plus-value-and-type parents) :test #'member)))))
			(error  (format nil "Field-definition: value ~a of field ~a is not a subtype of ~a"
					(cadr current-field) (car current-field)
					(caddr
					 (find
					  (car current-field)
					  (get-parents-fields-plus-value-and-type parents) :test #'member))))))
		 (list
		  (car current-field)
		  (cadr current-field)
		  (caddr current-field))))))))

(defun methods-structure (parents methods)
	(when methods
	 	(cons
			(method-definition parents (car methods))
			(methods-structure parents (cdr methods)))))
;;; (speak (args1 agrs2 ...) (corpo))
(defun method-definition (parents current-method)
	 (cond 
	 	((equal 'methods current-method) 'methods)
		(T 
			(cond
				((not (listp current-method)) (error (format nil "")))
				((null (car current-method)) (error (format nil "")))
				((not (symbolp (car current-method))) (error (format nil "")))
				((not (listp (cadr current-method))) (error (format nil "")))
			)
			(list (car current-method) (cadr current-method) (caddr current-method))
			(cons (car current-method) (process-method (car current-method) (cdr current-method)))
		)
	)
)

;;;make: crea una nuova istanza di una classe
(defun make (class-name &rest fields)
;;;blocco dei controlli
  (cond ((null class-name)
	 (error (format nil "Make: class-name is NULL")))
	((not (is-class class-name))
;;;(fields è null -> inizializzazio i parametri con valori di default definiti dalla classe
	 (error (format nil "Make: class ~a not found" class-name)))
;;;se inizializzo con dei parametri, verifico che siano i parametri della classe
	(T (field* class-name (get-fields-name fields))))
;;;ho controllato che la definizione dell'istanza sia corretta, posso procedere a rappresentarla
  (instance-rapresentation class-name fields))

;;;itero field su field-name per verificare appartenenza field-name alla classe
(defun field* (class-name field-name)
  (when field-name
;;;caso in cui field-name sia una lista
    (cond ((listp field-name)
	   (field class-name (car field-name))
	   (field* class-name (cdr field-name)))
;;;caso in cui field-name sia un atomo
	  ((atom field-name)
	   (field class-name field-name)))))

;;; field: estrae il valore di un campo da una classe (valore attributo nella classe)
;;;controllo che field-name sia membro dei fields della classe class-name
(defun field (class-name field-name)              
;;;istanza -> class-name è il nome dell'istanza
  (cond ((not (is-class class-name))
	 (cond ((cond ((or (atom class-name)
			   (not (listp class-name)))
		       (error (format nil "Field (instance): ~a is not a class or an instance" class-name)))
		      ((not (eql 'OOLINST (car class-name)))
		       (error (format nil "Field (instance): ~a is not a class or an instance" class-name)))))
	       ((eql 'OOLINST (car class-name))
		(format *STANDARD-OUTPUT* "Field (instance): Checking ~a in instance ~a~%" field-name class-name)
		(cond((not (member field-name (get-fields-name-of-class (nth 2 class-name))))
		      (error (format nil "Field (instance): field ~a does not exist in the instance ~a~%" field-name class-name)))
		     (t  (format *STANDARD-OUTPUT* "Field (instance): field ~a found in this instance (~a)~%" field-name (get-fields-name-of-class (nth 2 class-name))))))))
;;;classe -> class-name è il nome della classe
	(t (format *STANDARD-OUTPUT* "Field (class): Checking ~a in class ~a~%" field-name class-name)
	   (cond ((not (member field-name (get-fields-name-of-class (cdr (nth 2 (get-class-spec class-name))))))
		  (format *STANDARD-OUTPUT* "field (class): field ~a not found in this class (~a), Checking in parents...~%" field-name class-name)
		  (cond ((null (cadr (get-class-spec class-name)))
			 (error (format nil "Field (class): This class has no parents so field ~a does not exist!~%" field-name)))
			(T (get-parents (cadr (get-class-spec class-name)) field-name))))
		 (t (format *STANDARD-OUTPUT* "Field (class): field ~a found in this class (~a)~%" field-name class-name)))))
  (if (is-class class-name)
      T
      (cadr (find
	     (car (list field-name))
	     (nth 2 class-name) :test #'member))))
   
(defun instance-rapresentation (class-name fields)
  (list
   'OOLINST
   class-name
   (append
    (field-composition-make fields)
    (fields-from-parents
     (remove-duplicated-elements
;;;field della classe corrente
      (append
       (get-class-fields-plus-value-and-type (cdr (nth 2 (get-class-spec class-name))))
;;;field dei genitori (TUTTI)
       (get-parents-fields-plus-value-and-type (nth 1 (get-class-spec class-name)))))
     (field-composition-make fields)))))

(defun get-parents-fields-plus-value-and-type (parents-list)
  (when (and parents-list (get-class-fields-plus-value-and-type parents-list))
;;;field della classe corrente
    (append (get-class-fields-plus-value-and-type (cdr (nth 2 (get-class-spec (car parents-list)))))
;;;field del genitore successivo nella parents-list della classe corrente
	    (get-parents-fields-plus-value-and-type (cdr parents-list))
;;;field del genitore (del genitore corrente) della classe corrente
	    (get-parents-fields-plus-value-and-type (nth 1 (get-class-spec (car parents-list)))))))

(defun get-class-fields-plus-value-and-type (class-fields)
  (when class-fields
    (cons (car class-fields)
	  (get-class-fields-plus-value-and-type (cdr class-fields)))))

;;;fields: campi della make
(defun field-composition-make (fields)
  (when fields
    (cons (list (car fields) (cadr fields))
	  (field-composition-make (cddr fields)))))
;;;fields della make ordinati in ((attr1 val1) (attr2 val2) ...)

(defun fields-from-parents (list-of-total-fields fields-from-make)
  (if (not (null fields-from-make))
      (fields-from-parents
       (fields-from-parents-on-field list-of-total-fields (car fields-from-make))
       (cdr fields-from-make))
      (list-formatting-for-make list-of-total-fields)))
;;;Restituisce la lista data in cui a ogni sottolista è stato eliminato il terzo elemento.
(defun list-formatting-for-make (list-def-class-format)
  (mapcar #'(lambda (sublist) (subseq sublist 0 2)) list-def-class-format))

(defun fields-from-parents-on-field (list-of-total-fields field-from-make)
  (when field-from-make
    (format *STANDARD-OUTPUT* "Ffpof: checking ~a in ~a~%" (car field-from-make) list-of-total-fields)
    (cond ((member (car field-from-make) (get-fields-name-of-class list-of-total-fields))
	   (format *STANDARD-OUTPUT* "Ffpof: ~a is a member of ~a -(list of the fields' name from the tree of the class)~%"
		   (car field-from-make)
		   (get-fields-name-of-class list-of-total-fields))
	   (format *STANDARD-OUTPUT* "Ffpof: checking type matching (or subtype) between ~a and ~a~%"
		   (cadr field-from-make) (caddr (find (car field-from-make) list-of-total-fields :test #'member)))
	   (cond ((subtypep (type-of(cadr field-from-make))
			    (caddr (find (car field-from-make) list-of-total-fields :test #'member)))
		  (format *STANDARD-OUTPUT* "Ffpof: fields match~%"))
		 (t
		  (error
		   (format
		    nil
		    "Ffpof: field type ~a in make does not match its definition in def-class (~a) or is not a subtype"
		    (cadr field-from-make)
		    (caddr (find
			    (car field-from-make)
			    list-of-total-fields :test #'member)))))))
	  (t(error
	     (format
	     nil
	     "Ffpof - FATAL ERROR: THIS INSTANCE CONTAINS NOT-DEFINED FIELDS!"))))
    (remove-atom (car field-from-make) list-of-total-fields)))

;;;Restituisce la lista di liste senza le liste che contengono l'atomo dato.
(defun remove-atom (atom-to-remove list-of-lists)
  (remove-if #'(lambda (sublist) (member atom-to-remove sublist)) list-of-lists))

(defun remove-duplicated-elements (list-of-lists)
  (let ((new-list '())
	(existing-names '()))
    (dolist (sublist list-of-lists)
      (let ((nome (car sublist)))
	(if (not (member nome existing-names :test #'equal))
	    (progn
	      (setq existing-names (cons nome existing-names))
              (setq new-list (cons sublist new-list))))))
    (nreverse new-list)))
 
;;;ottengo i nomi dei campi contenuti in fields
(defun get-fields-name (fields)
  (when fields
    (cons (car fields) (get-fields-name (cddr fields)))))

;;;ottengo i nomi dei campi della classe (presenti in def-class)
(defun get-fields-name-of-class (class-fields)
  (when class-fields (cons (caar class-fields) (get-fields-name-of-class (cdr class-fields)))))

(defun get-parents-field (parents-list)
  (when parents-list
    (append (get-fields-name-of-class (cdr (nth 2 (get-class-spec (car parents-list)))))
	    (get-parents-field (cdr parents-list)))))

(defun get-parents (parents field-name)
  (unless (field* (car parents) field-name)(get-parents (cdr parents) field-name)))



;;;METHOD
(defun method* (class-name field-name)
  (when field-name
;;;caso in cui field-name sia una lista
    (cond  ((listp field-name)
	    (is-method class-name (car field-name))
	    (method* class-name (cdr field-name)))
;;;caso in cui field-name sia un atomo
	   ((atom field-name)
	    (is-method class-name field-name)))))

(defun get-parents-method (parents method-name)
;;;method-name -> car(method-name)
  (unless (method* (car parents) method-name)(get-parents-method (cdr parents) method-name)))

;;;((nome . interpreted...) (nome .interpreted...))
(defun get-methods-name-of-class (class-methods)
	(when class-methods (cons (caar class-methods) (get-methods-name-of-class (cdr class-methods))))
)
;;;  (when class-fields (cons (caar class-fields) (get-fields-name-of-class (cdr class-fields))))
(defun is-method (class-name method-name)
	
	(cond
		(
			(not (is-class class-name))
			(cond
				((or (atom class-name) (not (listp class-name)) (not (eql 'OOLINST (car class-name)))) 
					(error (format nil "")))
				(T 
					(format *STANDARD-OUTPUT* "Is-method: Checking ~a in class of ~a~%" method-name class-name)
					(cond 	(	(or 
									(null (nth 3 (get-class-spec (nth 1 class-name))))
									(not (member method-name (get-methods-name-of-class (cdr (nth 3 (get-class-spec (nth 1 class-name)))))))
								)
								(format *STANDARD-OUTPUT* "Is-method: method ~a not found in this class (~a), Checking in parents...~%" method-name (nth 1 class-name))
								(cond 	(	
											(null 	(cadr (get-class-spec (nth 1 class-name))))
											(error 	(format nil "Is-method: This class has no parents so method ~a does not exist!~%" method-name))
										)
										(T  (get-parents-method (cadr (get-class-spec (nth 1 class-name))) method-name))
								)
							)
					)
					(cdr (find (car (list method-name)) (cdr (nth 3 (get-class-spec (nth 1 class-name)))) :test #'member))
				)
			)
		)
		(T 
			(format *STANDARD-OUTPUT* "Is-method (class): Checking ~a in class ~a~%" method-name class-name)
			(cond 	(
						(not (member method-name (get-methods-name-of-class (cdr (nth 3 (get-class-spec class-name))))))
						(format *STANDARD-OUTPUT* "Is-method: method ~a not found in this class (~a), Checking in parents...~%" method-name class-name)
						(cond 	(
									(null (cadr (get-class-spec class-name)))
									(error (format nil "Is-method: This class has no parents so method ~a does not exist!~%" method-name))
								)
								(T  (get-parents-method (cadr (get-class-spec class-name)) method-name))
						)
					)
		 	)
			(cdr (find (car (list method-name)) (cdr (nth 3 (get-class-spec class-name))) :test #'member)))
	)
)


;;; get-method: estrae i metodi dai field
(defun get-method (fields)
  (cond ((null fields) nil)
	((and (listp (cadr fields)) (equal "methods" (car fields)))
	 (cons (car fields)
	       (cons (cadr fields)
		     (get-method (cdr fields)))))
	(T (get-method (cdr fields)))))

(defun get-method-name (method)
  (cond ((null method) nil)
	(T (cons (car method) (get-method-name (cddr method))))))


(defun rewrite-method-code (method-name method-spec)
	(cons 'lambda (cons 
		(cons 'this (car method-spec))
		(cdr method-spec)
	))
)

(defun process-method (method-name method-spec)
	(setf (fdefinition method-name)
	    (lambda (this &rest args)
			(apply (is-method this method-name) (append (list this) args))))
			(eval (rewrite-method-code method-name method-spec)))