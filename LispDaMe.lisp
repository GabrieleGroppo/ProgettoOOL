;;; come struttura dati per la memorizzazione delle classi viene usata una hash-table
(defparameter *classes-specs* (make-hash-table))

;;; i metodi add-class-spec e ge si occupano rispettivamente di:
;;; aggiungere una nuova classe 
;;; ottenere una classe 
(defun add-class-spec (name class-spec)
    ;;; cambia il valore in una posizione 
    (setf (gethash name *classes-specs*) class-spec)
    )
(defun get-class-spec (name)
    (gethash name *classes-specs*)
    )


;;; def-class definisce la struttura di una classe e la memorizza
;;; controlli per eventuali valori imprevisti
;;; !!
(defun def-class (class-name parents &rest parts)
    (cond ((or (not (atom class-name))
             (equal class-name '())
             (null class-name)
             (not (listp parents)))
           (error (format nil "unable to create the class: check Class-name or Parents"))))
    (add-class-spec class-name
        (append (list class-name)
                (append (list parents) 
                        (list (field-structure parts)))))
        class-name
    )

;;; is-class: restituisce T se il valore in input è il nome di una classe
(defun is-class (class-name)
    (if (get-class-spec class-name)
        T
        (error "Class-name does not exist")
    ))

;;; is-instance: restituisce T se il valore in input è il nome di una istanza di una classe
(defun is-instance (instance-value &optional (class-name T))
    (cond ((and (equal class-name 'T)
                (equal (car instance-value) 'OOLINST)) T)
            ((equal (cadr instance-value) class-name) T)
            ((member class-name (cadr (get-class-spec (cadr instance-value)))) T)
    ))

;;;make: crea una nuova istanza di una classe
(defun make (class-name &rest fields)
    (cond ((not (is-class class-name)))
        ((cons (list 'oolinst)
                 (list class-name
                        (field-composition
                            (field-exists class-name fields)))))
    ))

;;; field: estrae il valore di un campo da una classe (valore attributo nella classe)
(defun field (instance field-name)
    (cond ((get-value instance field-name))
        ;;; cadr = car (cdr (x))
        ((get-value (get-class-spec (cadr instance)) field-name))
        ((get-parents-field (get-parents (cadr instance)) field-name))
        ((error (format nil "Field: No method or field found for ~a" field-name)))
    ))

;;;field*
(defun field* (instance &rest field-name)
    (cond
        ((null (is-instance (field instance 
            (if (listp (car field-name))
                (caar field-name) 
                (car field-name)))))
            (error "Error: field* is not an instance"))
        ;;; equal: T se due oggetti sono strutturalmente uguali
        ;;; eq: T se due oggetti hanno gli stessi argomenti 
        ((eq (length field-name) 1)
            (field instance (if (listp (car field-name))
                                (caar field-name) 
                                (car field-name))))
        (T (field* (field instance (if (listp (car field-name))
                                       (caar field-name) (car field-name)))
                    (cdr field-name)))
    ))

;;; make-method: crea un metodo
(defun make-method (method-name method-args)
    (setf (fdefinition method-name) 
            (lambda (this &rest args) 
                ;; Applica funzione dell'istanza this con i parametri sotto
                (apply (field this method-name) (append (list this) args)))) 
    ;; Applica funzione dell'istanza this con i parametri sotto
    (eval (method-refresh method-name method-args)))

;;; method refresh: metodo viene riscritto come una funzione lambda
(defun method-refresh (method-name method-args)
    (cons 'lambda
        (cons (append (list 'this) (car (remove '=> method-args)))
              (cdr (remove ":" method-args)))
    ))

;;; get-method: estrae i metodi dai field
(defun get-method (fields)
    (cond ((null fields) nil)
          ((and (listp (cadr fields)) (equal "methods" (car fields)))
                    (cons (car fields)
                            (cons (cadr fields) 
                                    (get-method (cdr fields)))))
          (T (get-method (cdr fields)))
    ))

(defun get-method-name (method)
    (cond ((null method) nil)
          (T (cons (car method) (get-method-name (cddr method))))
    ))

(defun field-structure (fields)
    (if (= (list-length fields) 0) 
           (error (format nil "Error: list-length is equal to zero"))
           (cons (field-composition (car fields)) (method-composition (cdr fields))) 
           
           
        ; (list cdr fields))
        ;  ((member (car fields) (get-method-name (get-method fields)))
        ;   (cons (cons (car fields)
        ;            (list '=> (make-method (cdr fields) 
        ;                                    (cadr fields))))
        ;         (field-structure (cddr fields))))
        ;  ((cons (cons (car fields) (cadr fields))
        ;         (field-structure (cddr fields))))
    ))
(defun field-composition (fields)
    (when fields
        (append (list (car fields)) (field-composition (cdr fields)))
    ))

(defun method-composition (methods)
    (when methods
        (append (list (car methods)) (method-composition (cdr methods)))
    ))

(defun field-exists (class-name fields)
    ;(cond ((null fields))
            ;(error (format nil "Error: field is null"))
    ;      ((get-class-info class-name (car fields))
    ;      (cons (car fields)
    ;            (cons (cadr fields) (field-exists class-name (cddr fields)))))
    ;      (T (field-exists class-name (cddr fields)))
    (cond   ((null fields) 
                (error (format nil "Field-exists: field is NULL")))
            (member fields (field-composition (caaddr (get-class-spec class-name)))))
    
    )

;(defun get-value (instance field-name)
;   (let (format nil "~a" instance-string)
;        (cond 
;            (or ((null instance-string)
;                    (length instance-string)) nil) 
                    ;;;(error (format nil "Error: instance is null")))   ;;;caso base
;            ((not (search "fields" instance-string))
;                (if ((member field-name (cadr (member "fields" (nth 2 instance))))) 
;                    (if (null (cddr (member "fields" (nth 2 instance))))
;                    ()
;                ( (typep (cadr (member field-name (nth 3 instance)))))))))))
        
        
        
        
        ;((atom (car instance)) (get-value (caddr instance) field-name))   ;;;è un atomo
            ;;;è un metodo
        ;((and (symbolp (caar instance))
        ;       (equal (intern (symbol-name (caar instance)) "KEYWORD")
        ;              (intern (symbol-name field-name) "KEYWORD"))))
            ;;;è un attributo
        ;((and (symbolp (caar instance)) 
        ;      (equal (intern (symbol-name (caar instance)) "KEYWORD") 
        ;             (intern (symbol-name field-name) "KEYWORD")))
        ;    (if (null (cdar instance)) "undefined"                     ;;esiste ma è nil
        ;            (cdar instance)))
        ;(T (get-value (cdr instance) field-name))))

(defun get-class-info (class-name field-name)
    (cond ((get-value (get-class-spec class-name) field-name))
        ;;ricerca nei parents
          ((get-parents-field (get-parents class-name) field-name))
          ((error (format nil "Get-class-info: No method or field found for ~a" field-name)))
    ))

(defun get-parents (class-name)
    (cond ((null (cadr (get-class-spec class-name))) nil)
          ((remove-duplicates
                (append
                    (append (get-parents (caadr (get-class-spec class-name)))
                            (get-parents (cdadr (get-class-spec class-name))))
                    (cadr (get-class-spec class-name))) :from-end T))
    ))

(defun get-parents-field (parents field-name)
    (cond ((null parents) nil)
          ((null (get-value (get-class-spec (car parents)) field-name))
                (get-parents-field (cdr parents) field-name))
          ((get-value (get-class-spec (car parents)) field-name))
    ))



