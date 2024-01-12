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
Se anche questo controllo viene superato con successo, si procede alla creazione 
dell'oggetto, rappresentato come segue:
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

controlla che l'attributo *field-name* sia presente nella struttura dell'oggetto.
Assicuratosi che *instance* sia un oggetto:

-se trova l'attributo, restituisce il valore associato a quest'ultimo all'interno della struttura dell'oggetto

-altrimenti viene lanciato un errore

## FIELD*

__SINTASSI:
'(' field* \<instance\> \<field-name\>* ')'

Premesso che *field-name* è la lista delle coppie (attributo valore) nella 
struttura dell'oggetto, chiama la funzione __FIELD__ su ogni coppia di 
*field-name*

# funzioni di supporto

## GET-CLASS-PARENTS

__SINTASSI:
'(' get-class-parents \<class-name\> ')'__

Restituisce la lista delle superclassi di *class_name*

### funzioni supportate
- __PARENT__

## PARENT

__SINTASSI:
'(' parent \<searched-class-name\> \<class-name\> ')'__

Controlla che la classe *class-name* non sia una superclasse delle superclassi
di *class-name* stesssa. Evita quindi che si formi un ciclo, lanciando un errore
qualora si creasse. 

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

- __*current-field* è una tripla -> '(' \<attributo\> \<valore\> \<tipo\> ')':__
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

Viene analizzato\creato il metodo corrente. *current-method* è una lista siffatta:
- '(' \<method-name\> \<method-args\>* \<method-body\>* ')'

Se le varie parti passano il controllo di correttezza, viene creata la coppia:
- '(' \<method-name\> '.' \<anonymous-function\>* ')'

*anonymous-function* è la funzione anonima relativa a *method-name* restituita da
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
e che non sono stati inizializzati nell'istanza, oltre ovviamente agli attributi
inizializzati dalla stessa. Tutto questo salvo errori riguardo tipo, 
ereditarietà...

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
attributo rispetti la specifica del __tipo__ definita nella classe di appartenenza
dello stesso

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

__SINTASSI:
'(' fields-from-parents-on-field \<list-of-toatal-fields\>* \<field-from-make\> 
')'__

Notare che *field-from-make* è il nome di un attributo inizializzato dall'istanza.
Controlla che *field-from-make* sia un __sottotipo__ del tipo indicato nella 
definizione della classe alla quale esso appartiene. 
- __il controllo termina con esito positivo__: *field-from-make* viene rimosso
da  