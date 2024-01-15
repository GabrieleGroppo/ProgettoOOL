# Documentazione Progetto - Linguaggi di Programmazione 23/24 - Prolog

Autori:
- Groppo Gabriele - Matricola 902238 
- Caputo Lorenzo - Matricola 894528

# introduzione
Ai tempi di Simula e del primo Smalltalk, molto molto tempo prima di Python, 
Ruby, Perl e SLDJ, i programmatori Lisp già producevano una pletora di 
linguaggi object oriented. Il vostro progetto consiste nella costruzione 
di un’estensione “object oriented” di Common Lisp, chiamata OOΛ, e di 
un’estensione “object oriented” di Prolog, chiamata OOΠ. OOΛ e un linguaggio 
object-oriented con eredita multipla. Il suo scopo e didattico e mira 
soprattutto ad evidenziare aspetti dell’implementazione di linguaggi 
object-oriented: (1) il problema di dove e come recuperare i valori ereditati, 
(2) come rappresentare i metodi e le loro chiamate e (3) come manipolare il 
codice nei metodi stessi.

# Primitive

## DEF-CLASS

__SINTASSI:
'(' def-class \<class-name\> \<parents\> \<parts\>* ')'__

Definisce la struttura di una classe e la memorizza in una hash-table 
identificata da una variabile globale (*classes-specs*).
Viene eseguito un insieme di controlli che assicurino la correttezza,
sotto vari aspetti, della classe che si vuole istanziare. Se non sono
rilevati errori nella definizione della classe, essa viene aggiunta a
*classes-specs*, altrimenti viene lanciato un errore
       
## MAKE

__SINTASSI:
'(' make \<class-name\> \<fields\>* ')'__

Effettua la creazione di una nuovo oggetto di una classe.
Da considerare che *fields* rappresenta una lista di coppie (attributo valore),
che rappresentano i parametri con i quali l'oggetto viene creato.
Se la classe di appartenenza indicata è valida, viene effettuato il controllo 
sui parametri con i quali l'oggetto viene istanziato. In particolare, viene 
controllata l'appartenenza di ogni attributo alla classe indicata (o 
all'albero delle sue superclassi) e che il valore indicato sia un sottotipo del
tipo indicato nella definizione della classe alla quale l'attributo appartiene.
Nel caso il metodo fosse presente in più classi (nell'albero delle superclassi 
di *class-name*), viene considerata la sua definizione nella classe 
"__più vicina__" a *class-name*.
Se anche questo controllo viene superato con successo, si procede alla 
creazione  dell'oggetto, rappresentato come segue:
    '(' 'OOLINST \<class-name\> (\<field-name\> \<value\>)* ')'

## IS-CLASS

__SINTASSI:
'(' is-class \<class-name\> ')'__

controlla che \<class-name\> sia il nome di una classe precedentemente 
istanziata. 
Restituisce T se la condizione viene verificata 

## IS-INSTANCE

__SINTASSI:
'(' is-instance \<value\> [\<class-name\>] ')'__

controlla che *value* sia una istanza di *class-name*. Restituisce T se la 
condizione viene verificata.

## FIELD

__SINTASSI:
'(' field \<instance\> \<field-name\> ')'__

### *instance* è nome di una classe

controlla che l'attributo *field-name* appartenga alla classe *instance*. Se
non lo trova, effettua la ricerca dell'attributo nell'albero delle superclassi
di *instance*, fino ad arrivare alla radice. 
Restituisce T se lo trova, altrimenti viene lanciato un errore.

### *instance* è nome di un oggetto

controlla che l'attributo *field-name* sia presente nella struttura 
dell'oggetto.
Assicuratosi che *instance* sia un oggetto:

-se trova l'attributo, restituisce il valore associato a quest'ultimo 
all'interno della struttura dell'oggetto

-altrimenti viene lanciato un errore

## FIELD*

__SINTASSI:
'(' field* \<instance\> \<field-name\>* ')'

Premesso che *field-name* è la lista delle coppie (attributo valore) nella 
struttura dell'oggetto, chiama la funzione __FIELD__ su ogni coppia di 
*field-name*. Restituisce:
- __vero__ se tutti gli attributi in *field-name* sono presenti in *instance*
- __errore__ se anche uno solo degli attributi in *field-name* __non__ è 
presente in *instance* 

# funzioni di supporto

## GET-CLASS-PARENTS

__SINTASSI:
'(' get-class-parents \<class-name\> ')'__

Restituisce la lista *parents* di *class_name*

### funzioni supportate
- __PARENT__
- __GET-ALL-CLASS-PARENTS__

### GET-ALL-CLASS-PARENTS

__SINTASSI:
    '(' get-all-class-parents \<class-name\> ')'__

Restituisce la lista delle classi appartenenti all'albero delle superclassi di
*class-name*

### funzioni supportate
- __IS-INSTANCE__

## PARENT

__SINTASSI:
'(' parent \<searched-class-name\> \<class-name\> ')'__

Controlla che la classe *class-name* non sia una superclasse delle superclassi
di *class-name* stesssa. Evita quindi che si formi un ciclo, 
lanciando un errore qualora si creasse. 

### funzioni supportate
- __PARENT*__

## PARENT*

__SINTASSI:
'(' parent* \<searched-class-name\> \<parents\>* ')'__
Estende il controllo cominciato da __FIELD__ nell'albero delle superclassi di
*class_name*. 

-Se alla fine dell'esplorazione non viene riscontrata alcuna corrispondenza, 
viene restituito NIL

-se viene riscontrata una corrispondenza nel corso dell'esplorazione, viene 
restituito T (esplorazione ovviamente si arresta)

### funzioni supportate
- __DEF-CLASS__

## PARTS-STRUCTURE

__SINTASSI:
'(' parts-structure \<parents\> \<fields\>* ')'__

Crea la struttura della classe, scomponendo la parte dedicata agli attributi e 
quella dedicata ai metodi. Viene lanciato un errore se la lista degli attributi
è nulla

### funzioni supportate
- __DEF-CLASS__

## FIELDS-STRUCTURE

__SINTASSI:
'(' fields-structure \<fields\>* ')'__

Definisce la struttura della rappresentazione degli attributi della classe

### funzioni supportate
- __PARTS-STRUCTURE__

## FIELD-DEFINITION

__SINTASSI:
'(' field-definition \<current-field\> ')'__

Viene creata/analizzata la tripla rappresentata da *current-field* 
'(' \<attributo\> \<valore\> \<tipo\> ')':
- __*current-field* è una lista da un elemento -> '(' \<attributo\> ')':__
*valore* viene inizializzato con NIL e *tipo* con T

- __*current-field* è una coppia -> '(' \<attributo\> \<valore\> ')':__
*tipo* viene inizializzato con T

- __*current-field* è una tripla -> 
'(' \<attributo\> \<valore\> \<tipo\> ')':__
    - se *tipo* non è un tipo primitivo in Common-Lisp o il nome di una classe, 
    viene lanciato un errore (controllo eseguito da __VALID-FIELD-TYPE__)
    - se *valore* non è del tipo indicato da *tipo* viene lanciato un errore
    (controllo eseguito da __TYPE-MATCHING__)
    - se *attributo* è presente in una delle superclassi della classe che si 
    vuole creare (controllo eseguito da __IS-INHERITED__)
        - se *tipo* non è un sottotipo del tipo indicato per *attributo* nella
        superclasse considerata, viene lanciato un errore
    - altrimenti la tripla viene inizializzata correttamente

### funzioni supportate
- __FIELDS-STRUCTURE__

## METHODS-STRUCTURE

__SINTASSI:
'(' method-structure \<methods\>* ')'__

Definisce la struttura della rappresentazione dei metodi della classe. Se un 
metodo viene definito più di una volta, viene lanciato un errore

### funzioni supportate
- __PARTS-STRUCTURE__

## METHOD-DEFINITION

__SINTASSI:
'(' method-definition \<current-method\> ')'__

Viene analizzato\creato il metodo corrente. *current-method* è una 
lista siffatta:
- '(' \<method-name\> \<method-args\>* \<method-body\>* ')'

Se le varie parti passano il controllo di correttezza, viene creata la coppia:
- '(' \<method-name\> '.' \<anonymous-function\>* ')'

*anonymous-function* è la funzione anonima relativa a *method-name* 
restituita da
__PROCESS-METHOD__

### funzioni supportate
- __METHOD-STRUCTURE__

## INSTANCE_RAPRESENTATION

__SINTASSI:
'(' instance-rapresentation \<class-name\> \<fields\>* ')'__

Definisce la rappresentazione di una istanza di una classe. Se i metodi da essa 
chiamati non restituiscono errori, viene creata l'istanza, costituente nella 
lista:
- '(' 'OOLINST \<class-name\> \<fields>* ')'

*fields* è una lista che raccoglie gli attributi provenienti dalle superclassi 
e che non sono stati inizializzati nell'istanza, oltre ovviamente 
agli attributi inizializzati dalla stessa. 
Tutto questo salvo errori riguardo tipo, ereditarietà...

### funzioni supportate
- __MAKE__

## FIELD-COMPOSITION-MAKE

__SINTASSI:
'(' field-composition-make \<fields\>* ')'__

Crea e restituisce una lista di coppie (attributo valore) recuperando i valori 
dalla lista
*fields* della __MAKE__

### funzioni supportate
- __INSTANCE-RAPRESENTATION__

## GET-COMPLETE-PARENTS-FIELDS

__SINTASSI:
'(' get-complete-parents-fields \<parents-list\>* ')'__

Risale l'albero delle superclassi a partire dalla classe attuale e, per ogni 
classe di *parents-list* , recupera la parte dedicata agli attributi,
ordinandola in una lista di coppie/triple/singoletti, a seconda della 
definizione di quest'ultimi.
L'esplorazione viene eseguita sia in "lunghezza", esplorando tutti le classi 
appartenenti a *parents-list*, sia in "altezza", esplorando, per ogni classe 
di *parents-list* anche il proprio albero delle superclassi.

### funzioni supportate 
- __INSTANCE-RAPRESENTATION__

## GET-COMPLETE-CLASS-FIELDS

__SINTASSI:
'(' get-complete-class-fields \<class-fields\>* ')'__

Restituisce una lista contenente gli attrubuti della classe corrente.

### funzioni supportate
- __INSTANCE-RAPRESENTATION__
- __GET-COMPLETE-PARENTS-FIELDS__

## FIELDS-FROM-PARENTS

__SINTASSI:
'(' fields-from-parents \<list-of-total-fields\>* \<fields-from-make\>* ')'__

Notare che: 
- *list-of-total-fields* è la lista contenente tutti gli attributi
dall'albero delle superclassi
- *fields-from-make* è la lista contenente tutti gli attributi inizializzati
nell'istanza

Restituisce la lista degli attributi provenienti dalle superclassi. Vengono
considerati due casi:
- __*field-from-make* è vuota:__ l'istanza è stata creata senza inizializzare
alcun attributo, quindi viene restituita *list-of-total-fields* così per come è
- __*field-from-make* contiene attributi:__ l'istanza è stata creata 
inizializzando parte o tutti gli attributi provenienti dall'albero delle 
superclassi della classe di appartenenza, quindi viene controllato che ogni
attributo rispetti la specifica del __tipo__ definita nella classe 
di appartenenza dello stesso

### funzioni supportate
- __INSTANCE-RAPRESENTATION__

## LIST-FORMATTING-TO-MAKE

__SINTASSI:
'(' list-formatting-to-make \<list-def-class-format\>* ')'__

Converte la lista degli attributi della classe *list-def-class-format*, nella
quale non ci sono errori di tipo, nella stessa lista in formato "make", ovvero 
creando coppie (attributo valore) e la restituisce.

### funzioni supportate
- __FIELDS-FROM-PARENTS__

## FIELDS-FROM-PARENTS-ON-FIELD

__SINTASSI 
'(' fields-from-parents-on-field \<list-of-total-fields\>* 
\<field-from-make\>')'__

Notare che *field-from-make* è il nome di un attributo inizializzato 
dall'istanza.
Controlla che *field-from-make* sia un __sottotipo__ del tipo indicato nella 
definizione della classe alla quale esso appartiene. 
- __il controllo termina con esito positivo__: *field-from-make* viene rimosso
da *list-of-total-fields* poichè rappresenta una ridefinizione corretta dello 
stesso.
- __il controllo termina con esito negativo__: viene lanciato un errore.

### funzioni supportate
- __FIELDS-FROM-PARENTS__

## REMOVE-ATOM

__SINTASSI:
'(' remove-atom \<atom-to-remove\> \<list-of-lists\>* ')'

Restituisce la lista *list-of-lists* alla quale sono state rimosse tutte le 
sottoliste che hanno *atom-to-remove* come primo elemento.

### funzioni supportate 
- __FIELDS-FROM-PARENTS-ON-FIELD__

## REMOVE-DUPLICATED-ELEMENTS 

__SINTASSI:
'(' remove-duplicated-elements \<list-of-lists\>* ')'

Restituisce la lista *list-of-lists* nella quale ogni sottolista che ha come
primo elemento un atomo è presente una volta sola.

### funzioni supportate
- __INSTANCE-RAPRESENTATION__


## GET-FIELDS-NAME

__SINTASSI:
'(' get-fields-name \<fields\>* ')'__

Restituisce i nomi dei campi contenuti nella lista *fields*, che 
rappresenta gli attributi di un'istanza. 
La lista restituita conterrà i nomi dei campi in un formato leggibile.

### funzioni supportate
- __MAKE__

## GET-FIELDS-NAME-OF-CLASS

__SINTASSI: 
'(' get-fields-name-of-class \<class-fields\>* ')'__

Restituisce i nomi dei campi della classe, estratti dalla lista *class-fields*, 
che rappresenta la definizione della classe. 
La lista restituita conterrà i nomi dei campi in un formato leggibile.

### funzioni supportate
- __IS-METHOD__
- __IS-INHERITED__
- __FIELDS-STRUCTURE__
- __METHODS-STRUCTURE__
- __GET-FIELDS-NAME-OF-CLASS__
- __FIELD__
- __FIELDS-FROM-PARENTS-ON-FIELD__
- __GET-PARENTS-FIELDS__

## GET-PARENTS
__SINTASSI: 
'(' get-parents-field \<parents\>* \<field-name\> ')'__

Ricerca ricorsivamente un campo specifico in una lista di classi genitore 
e lo restituisce.

### funzioni supportate
- __FIELD__

## METHOD*
__SINTASSI: 
'(' method* \<class-name\> \<field-name\> ')'__

Ricerca ricorsivamente la presenza di un metodo in una classe specifica e con 
un nome di campo specifico e lo restituisce.

### funzioni supportate
- __GET-PARENTS-METHOD__

## GET-PARENTS-METHOD
__SINTASSI: 
'(' get-parents-method \<parents\>* \<method-name\> ')'__

Ricerca ricorsivamente la presenza di un metodo in una lista di classi genitore 
e lo restituisce.

### funzioni supportate
- __IS-METHOD__

## IS-METHOD
__SINTASSI: 
'(' is-method \<class-name\> \<method-name\> ')'__

Verifica ricorsivamente la presenza di un metodo in una classe specifica 
o nelle sue classi genitore.
### funzioni supportate
- __PROCESS-METHOD__
- __METHOD*__

## REWRITE-METHOD-CODE
__SINTASSI: 
'(' rewrite-method-code \<method-name\> \<method-spec\>* ')'__

Riscrive il codice di un metodo specifico, restituendo una nuova specifica 
di metodo.
### funzioni supportate
- __PROCESS-METHOD__

## PROCESS-METHOD
__SINTASSI: 
'(' rewrite-method-code \<method-name\> \<method-spec\>* ')'__

Aggiorna la definizione di un metodo, sostituendo la funzione originale con una
 funzione lambda che richiama il metodo esistente. 
 Inoltre, riscrive il codice del metodo usando la funzione 
 `rewrite-method-code`.

### funzioni supportate
- __DEF-CLASS__

## VALID-FIELD-TYPE
__SINTASSI: 
'(' valid-field-type \<field-type\> ')'__

Accetta un tipo di dati (`field-type`) e verifica se è un tipo di dati valido
in Common Lisp o se rappresenta una classe esistente. 
Restituisce `T` se il tipo di dati è valido, altrimenti genera un errore 
con un messaggio dettagliato.

TIPI VALIDI:
- 'number'
- 'integer'
- 'string'
- 'list'
- 'float'
- 'real'
- 'rational'
- 'complex'

### funzioni supportate
- __FIELD-DEFINITION__

## IS-INHERITED
__SINTASSI: 
'(' is-inherited \<parents\>* \<current-field\> ')'__

Accetta una lista di classi (`parents`) e un campo corrente, e verifica se 
il campo è ereditato da una di queste classi parents. 
Restituisce `T` se il campo è ereditato correttamente, altrimenti genera 
un errore con un messaggio dettagliato.

### funzioni supportate
- __FIELD-DEFINITION__

## TYPE-MATCHING
__SINTASSI:
 '(' type-matching \<current-field\> ')'__

Accetta un campo corrente e verifica se il valore associato al campo è del 
tipo di dati specificato per quel campo. In caso affermativo, la funzione 
restituirà `T`. In caso contrario, genererà un errore con un messaggio 
informativo.

### funzioni supportate
- __FIELD-DEFINITION__

## CONTAINS-DUPLICATES
__SINTASSI: 
'(' contains-duplicates \<list\>* ')'__

accetta una lista come argomento e restituisce `t` se la lista contiene
duplicati, altrimenti restituisce `nil`.

### funzioni supportate
- __METHODS-STRUCTURE__

# test effettuati

## creazione classi

### corrette 

- (def-class 'animal nil '(fields (name "Animal") (surname "Something") 
     (age 108 integer) (country "Africa")) '(methods (eat ()) (run ())))
    - ANIMAL

- (def-class 'felix '(animal) '(fields (name "Felix") (children 5 integer))
    '(methods (eat ())))
    - FELIX

- (def-class 'cat '(felix) '(fields (food-quantity 7 integer)))
    - CAT

- (def-class 'person nil '(fields (name "Eve") (age 21 integer)))
    - PERSON

- (def-class 'student '(person) ‘(fields (name "Eva Lu Ator") 
(university "Berkeley" string)) '(methods
(talk (&optional (out *standard-output*))
    (format out "My name is ~a~%My age is ~d~%" 
        (field this 'name) (field this 'age)))))
    - STUDENT

### errate

- (def-class 'person nil '(fields (name "Eve") (age 21 float)))
    - error: Type-matching: value 21 of field AGE is not of the type specified
     (FLOAT)

- (def-class 'person '(human) '(fields (name "Eve") (age 21 integer)))
    - error: Def-class: specified parents are not existing classes

- (def-class 'cat '(felix) '(fields (food-quantity 7 integer) 
(children 7.8 float)))
    - error: Is-inherited: value 7.8 of field CHILDREN is not a 
    subtype of INTEGER

- (def-class 'person nil '(fields (name "Eve") (age 21 integer) 
(name "Person" string)))
    - error: fields-structure: duplicated fields detected

- (def-class 'animal nil '(fields (name "Animal") (surname "Something") 
     (age 108 integer) (country "Africa")) '(methods (eat ()) (eat ())))
    - error: methods-structure: duplicated methods detected

## creazione istanze 
    data la creazione corretta delle classi

### corrette
- (defparameter fufi (make 'cat 'name "Fufi" 'age 6))
    - FUFI

- (defparameter whisky (make 'cat))
    - WHISKY

- (defparameter alex (make 'student 'name "Alex The Lion" 'age 25 'university
"Bicocca"))
    - ALEX

- (defparameter gabriele (make 'student))
    - GABRIELE
### errate

- (defparameter fufi (make 'cat 'name "Fufi" 'name "Ciao"))
    - error: Ffpof: you have tried to initialize the same field more than once!

- (defparameter fufi (make 'cat 'name "Fufi" 'age 107.4))
    - error: Ffpof: field type 107.4 in make does not match its definition in 
    its class (INTEGER)

- (defparameter fufi (make 'cat 'name "Fufi" 'age 14 'height 32))
    - error: Field (class): This class has no parents so field HEIGHT does not 
    exist!

- (defparameter friedmann (make 'dog))
    - error: Make: class DOG not found

## is-class e is_istance
 date le creazioni corrette di classi e istanze

### corretti
- (is-instance alex T)
    - T

- (is-instance alex 'person)
    - T

- (is-instance fufi 'animal)
    - T

- (is-instance whisky 'felix)
    - T

### errati
- (is-instance alex 'felix)
    - NIL

- (is-instance fufi 'dog)
    - NIL

- (is-instance whisky 'cat)
    - NIL 

## metodi
 date le creazioni corrette di classi e istanze

### corretti

- (talk alex)
    - My name is Alex The Lion  
    My age is 25  
    NIL

- (talk gabriele)
    - My name is Eva Lu Ator  
    My age is 21  
    NIL

### errati

- (talk fufi)
    - error: Is-method: This class has no parents so method TALK does not exist!

- (talk 45)
    - error: Is-method: There is something wrong with this method

## field
 date le creazioni corrette di classi e istanze

### corretti 

- (field gabriele 'name)
    - "Eva Lu Ator"

- (field alex 'age)
    - 25

- (field 'person 'age)
    - T

- (field whisky 'children)
    - 5

### errati

- (field gabriele 'surname)
    - error: Field (instance): field SURNAME does not exist in the instance
(OOLINST STUDENT ((NAME Eva Lu Ator) (UNIVERSITY Berkeley) (AGE 21)))

- (field 'human 'name)
    - error: Field (instance): HUMAN is not a class or an instance

- (field whisky nil)
    - error: Field (instance): field-name is NULL

- (field whisky '(name age))
    - error: Field (instance): field-name is a list

- (field 'person 'children)
    - error: Field (class): this class has no parents so field CHILDREN does
    not exist!!

## field*
 date le creazioni corrette di classi e istanze

### corretti

- (field* 'person '(name age))
    - T

- (field* whisky '(name children))
    - T

- (field* 'person 'age) -> nel caso in cui riceva un simbolo, si comporta come 
__FIELD__
    - T

I risultati dei test seguenti sono giustificati dalla possibilità di creare
una istanza senza inizializzare alcun attributo (*fields* è () o nil)
- (field* 'person ())
    - T

- (field* 'person nil)
    - T
### errati

- (field* 'human '(name age))
    - error: Field: HUMAN is not a class or an instance

- (field* gabriele '(age name surname))
    - error: Field (instance): field SURNAME does not exist in the instance
    (OOLINST STUDENT ((NAME Eva Lu Ator) (UNIVERSITY Berkeley) (AGE 21)))
