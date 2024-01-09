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
def-class, make, is-class, is-instance, field, field*

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
    '(' 'OOLINST \<class-name\> \<(\<field-name\> \<value\>)\>* ')'

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
'(' field \<class-name\> \<field-name\> ')'__

### *class-name* è nome di una classe

controlla che l'attributo *field-name* appartenga alla classe *class-name*. Se
non lo trova, effettua la ricerca dell'attributo nell'albero delle superclassi
di *class-name*, fino ad arrivare alla radice. 
Restituisce T se lo trova, altrimenti viene lanciato un errore.

### *class-name* è nome di un oggetto

controlla che l'attributo *field-name* sia presente nella struttura dell'oggetto.
Assicuratosi che *class-name* sia un oggetto:

-se trova l'attributo, restituisce il valore associato a quest'ultimo all'interno della struttura dell'oggetto

-altrimenti viene lanciato un errore

## FIELD*

__SINTASSI:
'(' field* \<class-name\> \<field-name\>* ')'

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
    viene lanciato un errore. 
    - se *valore* non è del tipo indicato da *tipo* viene lanciato un errore
    - se *attributo* è presente in una delle superclassi della classe che si 
    vuole creare 
        - se *tipo* non è un sottotipo del tipo indicato per *attributo* nella
        superclasse considerata, viene lanciato un errore
    - altrimenti la tripla viene inizializzata correttamente

### funzioni supportate
- __FIELDS-STRUCTURE__

## METHODS_STRUCTURE

## INSTANCE_RAPRESENTATION

__SINTASSI:

