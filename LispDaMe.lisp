;;; Struttura dati per la memorizzazione delle classi
(defparameter *classes-specs* (make-hash-table))

;;; Aggiunge una nuova classe e le sue specifiche 
(defun add-class-spec (name class-spec)
;;; Cambia il valore in una posizione
  (setf (gethash name *classes-specs*) class-spec))

;;; Ritorna una classe e le sue specifiche
(defun get-class-spec (name)
  (gethash name *classes-specs*))

;;; Definisce la struttura di una classe e la memorizza
(defun def-class (class-name parents &rest parts)
  ;; Controlli per eventuali valori imprevisti
  (cond ((not (atom class-name))
         (error (format nil 
          "Def-class: unable to create the class -> ~a is not an atom~%" 
          class-name)))
        ((equal class-name '())
         (error (format nil 
          "Def-class: unable to create the class -> ~a is an empty list~%"
          class-name)))
        ((null class-name)
         (error (format nil 
          "Def-class: unable to create the class -> ~a is NULL~%" 
          class-name)))
        ((not (listp parents))
         (error (format nil 
          "Def-class: unable to create the class -> ~a is not a list~%" 
          parents)))
        ((is-class class-name)
         (error (format nil 
          "Def-class: unable to create the class -> ~a already exists~%" 
          class-name)))
        ((and parents
              (not (null (member nil (mapcar #'is-class parents)))))
         (error (format nil 
          "Def-class: specified parents are not existing classes~%")))
        ((member class-name parents)
         (error (format nil 
          "Def-class: the class ~a is also present in its parents' list ~a~%"
          class-name parents))))
  (parent* class-name parents)
  ;; Creazione della classe
  (add-class-spec class-name
                  (append (list class-name)
                          (append (list parents)
                                  (parts-structure parents parts))))
  class-name)


;;; Recupera la lista parent della classe
(defun get-class-parents (class-name)
  (nth 1 (get-class-spec class-name)))

;;; Recupera la lista di tutti i parent a partire dalla classe attuale
(defun get-all-class-parents (class-name)
  (let ((parents (get-class-parents class-name)))
    (when parents
      (reduce #'append parents
              :initial-value parents
              :key #'get-all-class-parents))))

;;; Verifica che il parent speficicato è valido
(defun parent (searched-class-name class-name)
  (cond
    ((null (get-class-parents class-name)) nil)
    ((member searched-class-name (get-class-parents class-name))
     (error (format nil 
         "Def-class: A class cannot be anchestor of itself.~%")))
    (T (parent* searched-class-name (get-class-parents class-name)))))

;;; Ricerca un parent in una lista di parent
(defun parent* (searched-class-name parents)
  (when (and parents (not(parent searched-class-name (car parents))))
    (parent* searched-class-name (cdr parents)))
  (if (null parents) nil T))

;;; Restituisce T se il valore in input è il nome di una classe
(defun is-class (class-name)
  (if (get-class-spec class-name) T))    

;;; Restituisce T se il valore in input è il nome di una istanza di una classe
(defun is-instance (value &optional (class-name T))
  (cond ((equal (car value) 'OOLINST)
     (cond
;;; se class-name è T -> value può essere istanza qualunque
       ((equal class-name 'T)   T)
;;; Controllo che class-name sia superclasse della classe di value
       ((member class-name (get-all-class-parents (cadr value))) T)))))

;;; Definisce la struttura degli attributi e dei metodi 
(defun parts-structure (parents parts)
    (let
        ((value (length parts)))
        (ecase value
            (0 nil)
            (1 (cond ((and (not (equal (caar parts) 'fields))
                          (not (equal (caar parts) 'methods)))
                      (error (format nil "Parts-structure: 
                        invalid fields or methods spelling~%")))
                   (t (if (equal (caar parts) 'fields)
                             (list (fields-structure parents (car parts)))
                          (cons 'nil (list (methods-structure (car parts))))))))
            (2 (cond ((or (and  (not (equal (caar parts) 'fields))
                                    (not (equal (caar parts) 'methods)))
                          (and  (not (equal (caadr parts) 'fields))
                                (not (equal (caadr parts) 'methods))))
                      (error (format nil "Parts-structure: 
                        invalid fields or methods spelling~%"))))
                (if (equal (caar parts) 'fields)
                      (cons
                      (fields-structure parents (car parts))
                      (list (methods-structure (cadr parts))))
                    (cons
                         (fields-structure parents (cadr parts))
                      (list (methods-structure (car parts)))))))))

;;; Definisce la struttura dei campi
(defun fields-structure (parents fields)
      (when fields
        (if (contains-duplicates (get-fields-name-of-class (cdr fields)))
            (error (format nil "fields-structure: 
              duplicated fields detected~%"))
            (cons
                (field-definition parents (car fields))
                (fields-structure parents (cdr fields))))))

;;; Controlla la validità di un attributo
(defun field-definition (parents current-field)
      (cond 
        ((equal 'fields current-field) 'fields)
        (T
            (let 
                ((value (length current-field)))
                (ecase value
                    (1  (list (car current-field) 'NIL 'T))
                    (2  (list (car current-field) (cadr current-field) 'T))
    ;;;controllo sul tipo del campo
                    (3  
                        (valid-field-type (caddr current-field))
                        (type-matching current-field)
                        (is-inherited parents current-field)
                        (list
                            (car current-field)
                            (cadr current-field)
                            (caddr current-field))))))))

;;; Definisce la struttura dei metodi
(defun methods-structure (methods)
    (when methods
        (if (contains-duplicates (get-fields-name-of-class (cdr methods)))
            (error (format nil 
                "methods-structure: duplicated methods detected~%"))
            (cons
                (method-definition (car methods))
                (methods-structure (cdr methods))))))

;;; Controlla la validità di un metodo
(defun method-definition (current-method)
     (cond 
         ((equal 'methods current-method) 'methods)
        (T 
            (cond
                (
                    (not (listp current-method)) 
                    (error (format nil 
                        "method-definition: the method is not a list~%")))
                (
                    (null (car current-method)) 
                    (error (format nil 
                        "method-definition: method-name is null~%")))
                (
                    (not (symbolp (car current-method))) 
                    (error (format nil 
                        "method-definition: method-name is not a symbol~%")))
                (
                    (not (listp (cadr current-method))) 
                    (error (format nil 
                        "method-definition: method body is not a list~%"))))
            (list (car current-method) 
                    (cadr current-method) 
                    (caddr current-method))
            (cons (car current-method) 
                    (process-method 
                      (car current-method) 
                      (cdr current-method))))))

;;; Crea una nuova istanza di una classe
(defun make (class-name &rest fields)
  (cond ((null class-name)
     (error (format nil "Make: class-name is NULL~%")))
    ((not (is-class class-name))
     (error (format nil "Make: class ~a not found~%" class-name)))
    (T (field* class-name (get-fields-name fields))))
  (instance-rapresentation class-name fields))

;;; Verifico la correttezza dei campi
(defun field* (instance field-name)
  (when field-name
;;; field-name è una lista
    (cond ((listp field-name)
       (field instance (car field-name))
       (field* instance (cdr field-name)))
;;; field-name è un atomo
      ((atom field-name)
       (field instance field-name))))
    T)

;;; Estrae il valore di un campo da una classe (valore attributo nella classe)
;;;controllo che field-name sia membro dei fields della classe instance
(defun field (instance field-name)              
;;;istanza -> instance è il nome dell'istanza
  (cond ((not (is-class instance))
     (cond ((cond ((or (atom instance)
               (not (listp instance)))
               (error 
                     (format nil 
                        "Field: 
                        ~a is not a class or an instance~%" 
                        instance)))
              ((not (eql 'OOLINST (car instance)))
               (error 
                     (format nil 
                        "Field: 
                        ~a is not a class or an instance~%" 
                     instance)))))
           ((eql 'OOLINST (car instance))
        (format *STANDARD-OUTPUT* 
            "Field (instance): 
            Checking ~a in instance ~a~%" 
                field-name 
                instance)
        (cond 	(	(null field-name)
                    (error (format nil 
                      "Field (instance): 
                      field-name is NULL")))
        
                (	(listp field-name)
                    (error (format nil 
                      "Field (instance): 
                      field-name is a list")))
                
                (T (cond (	(not 
                    (member 
                        field-name 
                        (get-fields-name-of-class (nth 2 instance))))
                          (error 
                            (format 
                              nil 
                                "Field (instance): 
                                field ~a does not exist in the instance ~a~%" 
                                field-name 
                                instance)))
                         (t  (format *STANDARD-OUTPUT* 
                                "Field (instance): 
                                field ~a found in this instance (~a)~%"
                              field-name 
                            (get-fields-name-of-class (nth 2 instance))))))))))
;;;classe -> instance è il nome della classe
    (t (format *STANDARD-OUTPUT* "Field (class): Checking ~a in class ~a~%" 
        field-name 
        instance)
       (cond ((not 
                    (member 
                        field-name 
                        (get-fields-name-of-class 
                            (cdr (nth 2 (get-class-spec instance))))))
          (format *STANDARD-OUTPUT* 
              "field (class): 
              field ~a not found in this class (~a). 
              Checking in parents...~%"
              field-name 
            instance)
          (cond ((null (cadr (get-class-spec instance)))
             (error 
                 (format nil 
                    "Field (class): 
                    This class has no parents so field ~a does not exist!~%"
                    field-name)))
            (T (search-field-in-parents 
                (cadr (get-class-spec instance)) 
                field-name))))
         (t (format *STANDARD-OUTPUT* 
                 "Field (class): field ~a found in this class (~a)~%" 
            field-name 
            instance)))))
  (if (is-class instance)
      (cadr (find
         (car (list field-name))
         (cdr (nth 2 (get-class-spec instance))) :test #'member))
      (cadr (find
         (car (list field-name))
         (nth 2 instance) :test #'member))))

;;; Rappresenta un istanza di una classe
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
       (get-complete-class-fields (cdr (nth 2 (get-class-spec class-name))))
;;;field dei genitori (TUTTI)
       (get-complete-parents-fields (nth 1 (get-class-spec class-name)))))
     (field-composition-make fields)))))

;;; Estrae i nomi dei campi della make
(defun field-composition-make (fields)
  (when fields
    (cons (list (car fields) (cadr fields))
      (field-composition-make (cddr fields)))))

;;; Restituisce una lista degli attr. completi (nome valore tipo) dei parents
(defun get-complete-parents-fields (parents-list)
  (when (and parents-list (get-complete-class-fields parents-list))
;;; field della classe corrente
    (append 
        (get-complete-class-fields 
            (cdr (nth 2 (get-class-spec (car parents-list)))))
;;; field del genitore successivo nella parents-list della classe corrente
        (get-complete-parents-fields (cdr parents-list))
;;; field del genitore (del genitore corrente) della classe corrente
        (get-complete-parents-fields 
             (nth 1 (get-class-spec (car parents-list)))))))

;;; Restituisce una lista di attributi completi (nome valore tipo) della classe
(defun get-complete-class-fields (class-fields)
  (when class-fields
    (cons (car class-fields)
      (get-complete-class-fields (cdr class-fields)))))



;;;fields della make ordinati in ((attr1 val1) (attr2 val2) ...)
(defun fields-from-parents (list-of-total-fields fields-from-make)
  (if (not (null fields-from-make))
      (fields-from-parents
       (fields-from-parents-on-field list-of-total-fields 
                                       (car fields-from-make))
       (cdr fields-from-make))
      (list-formatting-for-make list-of-total-fields)))
;;; Elimina il terzo elemento da ogni sottolista della lista passata
(defun list-formatting-for-make (list-def-class-format)
  (mapcar #'(lambda (sublist) (subseq sublist 0 2)) list-def-class-format))

(defun fields-from-parents-on-field (list-of-total-fields field-from-make)
  (when field-from-make
    (format *STANDARD-OUTPUT* 
        "Ffpof: checking ~a in ~a~%" 
         (car field-from-make) list-of-total-fields)
    (cond (
        (member 
            (car field-from-make) 
            (get-fields-name-of-class list-of-total-fields))
       (format *STANDARD-OUTPUT* 
           "Ffpof: ~a is a member of ~a ~%"
           (car field-from-make)
           (get-fields-name-of-class list-of-total-fields))
       (format *STANDARD-OUTPUT* 
           "Ffpof: checking type matching (or subtype) between ~a and ~a~%"
           (cadr field-from-make) 
            (caddr 
                (find 
                    (car field-from-make) 
                    list-of-total-fields :test #'member)))
       (cond ((subtypep (type-of(cadr field-from-make))
                (caddr 
                     (find 
                         (car field-from-make) 
                        list-of-total-fields :test #'member)))
          (format *STANDARD-OUTPUT* "Ffpof: fields match~%"))
         (t
          (error
           (format
            nil
            "Ffpof: 
            field type ~a does not match its definition in its class (~a)~%"
            (cadr field-from-make)
            (caddr (find
                (car field-from-make)
                list-of-total-fields :test #'member)))))))
      (t(error
         (format
         nil
         "Ffpof: 
         you have tried to initialize the same field more than once!~%"))))
    (remove-atom (car field-from-make) list-of-total-fields)))

;;;Restituisce la lista di liste senza le liste che contengono l'atomo dato.
(defun remove-atom (atom-to-remove list-of-lists)
  (remove-if #'(lambda (sublist) 
    (member atom-to-remove sublist)) list-of-lists))

(defun remove-duplicated-elements (list-of-lists)
    (let ((new-list '())
            (existing-names '()))
        (dolist 
            (sublist list-of-lists)
          (let ((nome (car sublist)))
                (if 
                    (not (member nome existing-names :test #'equal))
                    (progn
                      (setq existing-names (cons nome existing-names))
                      (setq new-list (cons sublist new-list))))))
        (nreverse new-list)))
 
;;;ottengo i nomi dei campi contenuti in fields (per le istanza)
(defun get-fields-name (fields)
  (when fields
    (cons (car fields) (get-fields-name (cddr fields)))))

;;;ottengo i nomi dei campi della classe (presenti in def-class)
(defun get-fields-name-of-class (class-fields)
  (when class-fields 
      (cons 
        (caar class-fields) 
        (get-fields-name-of-class (cdr class-fields)))))

(defun search-field-in-parents (parents field-name)
  (unless 
      (field* (car parents) field-name)
      (search-field-in-parents (cdr parents) field-name)))

;;;METHOD
(defun method* (class-name method-name)
  (when method-name
;;;caso in cui method-name sia una lista
    (cond  ((listp method-name)
        (is-method class-name (car method-name))
        (method* class-name (cdr method-name)))
;;;caso in cui method-name sia un atomo
       ((atom method-name)
        (is-method class-name method-name)))))

;;;"Restituisce il metodo chiamato method-name dai genitori."
(defun get-parents-method (parents method-name)
  (if parents
      (let ((current-parent (car parents)))
        (if (method* current-parent method-name)
            (method* current-parent method-name)
            (get-parents-method (cdr parents) method-name)))
      nil))

(defun is-method (class-name method-name)
    (cond
        (
            (not (is-class class-name))
            (cond
                ((or 
                    (atom class-name) 
                    (not (listp class-name)) 
                    (not (eql 'OOLINST (car class-name)))) 
                    (error (format nil 
                    "Is-method: 
                    there is something wrong with this method")))
                (T 
                    (format *STANDARD-OUTPUT* 
                    "Is-method: Checking ~a in class of ~a~%" 
                        method-name 
                        class-name)
                    (cond ((or 
                              (null 
                                (nth 3 (get-class-spec (nth 1 class-name))))
                              (not 
                                  (member method-name 
                                      (get-fields-name-of-class 
                                          (cdr 
                                              (nth 3 
                                                  (get-class-spec 
                                                      (nth 1 class-name))))))))
                            (format *STANDARD-OUTPUT* 
                                "Is-method: 
                                method ~a not found in this class (~a). 
                                Checking in parents...~%" 
                                method-name 
                                (nth 1 class-name))
                            (cond 
                              (	
                                (null (cadr (get-class-spec 
                                        (nth 1 class-name))))
                                (error 
                                      (format nil 
                                          "Is-method: 
                                          this class has no parents. 
                                          Method ~a does not exist!~%"
                                          method-name)))
                              (T (get-parents-method 
                                      (cadr 
                                          (get-class-spec 
                                              (nth 1 class-name))) 
                                      method-name))))
                          (T (cdr 
                              (assoc 
                                  method-name 
                                  (cdr 
                                      (nth 3 
                                          (get-class-spec 
                                            (nth 1 class-name)))))))))))
        (T 
            (format *STANDARD-OUTPUT* 
            "Is-method (class): 
            Checking ~a in class ~a~%"
            method-name 
            class-name)
            (cond (
                    (not 
                        (member 
                            method-name 
                                (get-fields-name-of-class 
                                    (cdr 
                                      (nth 3 
                                        (get-class-spec class-name))))))
                    (format *STANDARD-OUTPUT* 
                        "Is-method: 
                        method ~a not found in this class (~a). 
                        Checking in parents...~%"
                        method-name 
                        class-name)
                    (cond (
                            (null (cadr (get-class-spec class-name)))
                            (error 
                                (format nil 
                                    "Is-method: 
                                    Class has no parents. 
                                    Method ~a does not exist!~%"
                                    method-name)))
                          (T  
                              (get-parents-method 
                                  (cadr 
                                      (get-class-spec class-name)) 
                                  method-name))))
                (T (cdr 
                      (assoc method-name 
                          (cdr 
                              (nth 3 (get-class-spec class-name))))))))))

(defun rewrite-method-code (method-name method-spec)
    (format *STANDARD-OUTPUT* "Rewriting method ~a code... ~%" method-name)
    (cons 'lambda (cons 
        (cons 'this (car method-spec))
        (cdr method-spec))))

(defun process-method (method-name method-spec)
    (setf (fdefinition method-name)
        (lambda (this &rest args)
            (apply (is-method this method-name) (append (list this) args))))
            (eval (rewrite-method-code method-name method-spec)))

;;; Controllo se il tipo specificato è valido  
(defun valid-field-type (field-type)
    (if 
        (or 
            (equal field-type 'number)
            (equal field-type 'integer)
            (equal field-type 'string)
            (equal field-type 'list)
            (equal field-type 'float)
            (equal field-type 'real)
            (equal field-type 'rational)
            (equal field-type 'complex)
            (is-class field-type)
        )
        T
        (error 
            (format nil 
            "Valid-field-type: 
            ~a is not a Common-Lisp type or an existing class" field-type))))

;;; Controllo ereditarietà del campo
(defun is-inherited (parents current-field)
    (if 
        (and 
            (not (null parents))
            (member 
                (car current-field)
                (get-fields-name-of-class 
                  (get-complete-parents-fields parents)))
            (not (subtypep 
                    (caddr current-field)
                    (caddr (find 
                                (car current-field) 
                                (get-complete-parents-fields parents) 
                                :test #'member)))))					
        (error  (format nil 
                        "Is-inherited: 
                        value ~a of field ~a is not a subtype of ~a"
                    (cadr current-field) (car current-field)
                    (caddr
                        (find
                              (car current-field)
                              (get-complete-parents-fields parents) 
                              :test #'member))))
        T))

;;; Controllo se il valore del campo coincide con il tipo specificato
(defun type-matching (current-field)
    (if (typep (cadr current-field) (caddr current-field))
        T
        (error
            (format nil 
                "Type-matching: 
                value ~a of field ~a is not of the type specified (~a)"
                (cadr current-field) 
                (car current-field)
                (caddr current-field)))))

;;; Verifico se una lista contiene duplicati
(defun contains-duplicates (lista)
  (cond
    ((null lista) nil)
    ((member (car lista) (cdr lista)) t)
    (t (contains-duplicates (cdr lista)))))

