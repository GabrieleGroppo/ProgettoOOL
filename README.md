
# Documentazione Progetto - Linguaggi di Programmazione 23/24 - Lisp

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

### Sintassi:
- `'(' def-class <class-name> <parents> <parts>* ')'`

Definisce la struttura di una classe e la memorizza in una hash-table 
identificata da una variabile globale (`classes-specs`).
Viene eseguito un insieme di controlli che assicurino la correttezza,
sotto vari aspetti, della classe che si vuole istanziare. Se non sono
rilevati errori nella definizione della classe, essa viene aggiunta a
`classes-specs`, altrimenti viene lanciato un errore
       
## MAKE

### Sintassi:
- `'(' make <class-name> <fields>* ')'`

Effettua la creazione di una nuovo oggetto di una classe.
Da considerare che `fields` rappresenta una lista di coppie (attributo valore),
che rappresentano i parametri con i quali l'oggetto viene creato.
Se la classe di appartenenza indicata è valida, viene effettuato il controllo 
sui parametri con i quali l'oggetto viene istanziato. In particolare, viene 
controllata l'appartenenza di ogni attributo alla classe indicata (o 
all'albero delle sue superclassi) e che il valore indicato sia un sottotipo del
tipo indicato nella definizione della classe alla quale l'attributo appartiene.
Nel caso l'attributo fosse presente in più classi (nell'albero delle superclassi 
di `class-name`), viene considerata la sua definizione nella classe 
"*più vicina*" a `class-name`.
Se anche questo controllo viene superato con successo, si procede alla 
creazione  dell'oggetto, rappresentato come segue:
-  `'(' 'OOLINST <class-name> <(<field-name> <value>)>* ')'`

## IS-CLASS

### Sintassi:
- `'(' is-class <class-name> ')'`

controlla che `class-name` sia il nome di una classe precedentemente 
istanziata.\
Restituisce **T** se la condizione viene verificata. 

## IS-INSTANCE

### Sintassi:
- `'(' is-instance <value> [<class-name>] ')'`

controlla che `value` sia una istanza di `class-name`. 

- **`class-name` è T:** 
    - `value` può essere un'istanza qualunque.
- **`class-name` è una classe:**
    - `value` deve essere istanza di una classe che ha `class-name` come 
    superclasse    

Restituisce **T** se la condizione viene verificata.

## FIELD

### Sintassi:
- `'(' field <instance> <field-name> ')'`

### `instance` è nome di una classe

controlla che l'attributo `field-name` appartenga alla classe `instance`. Se
non lo trova, effettua la ricerca dell'attributo nell'albero delle superclassi
di `instance`, fino ad arrivare alla radice.

- se trova l'attributo, restituisce il valore associato a quest'ultimo 
all'interno della definizione della classe
- altrimenti viene lanciato un errore.

### `instance` è nome di un oggetto

controlla che l'attributo `field-name` sia presente nella struttura 
dell'oggetto.
Assicuratosi che `instance` sia un oggetto:

- se trova l'attributo, restituisce il valore associato a quest'ultimo 
all'interno della struttura dell'oggetto
- altrimenti viene lanciato un errore

## FIELD*

### Sintassi:
- `'(' field* <instance> <field-name>* ')'`

Premesso che `field-name` è una lista di attributi e `instance` è il nome di una
classe o di una istanza, chiama la funzione **FIELD** 
su ogni attributo di `field-name`. Restituisce:

- **T** se tutti gli attributi in `field-name` sono presenti in `instance`
- **errore** se anche uno solo degli attributi in `field-name` **non** è 
presente in `instance`. 

# Funzioni di supporto

## GET-CLASS-PARENTS

### Sintassi:
- `'(' get-class-parents <class-name> ')'`

Restituisce la lista `parents` di `class_name`.

### Funzioni supportate
- __PARENT__
- __GET-ALL-CLASS-PARENTS__

## GET-ALL-CLASS-PARENTS

### Sintassi:
- `'(' get-all-class-parents <class-name> ')'`

Restituisce la lista delle classi appartenenti all'albero delle superclassi di
`class-name`

### Funzioni supportate
- __IS-INSTANCE__

## PARENT

### Sintassi:
- `'(' parent <searched-class-name> <class-name> ')'`

Controlla che la classe `class-name` non sia una superclasse delle superclassi
di `class-name` stessa.\
Evita quindi che si formi un ciclo, lanciando un errore qualora si creasse. 

### Funzioni supportate
- __PARENT*__

## PARENT*

### Sintassi:
- `'(' parent* <searched-class-name> <parents>* ')'`

Estende il controllo cominciato da **FIELD** nell'albero delle superclassi di
`class_name`. 

- Se alla fine dell'esplorazione non viene riscontrata alcuna corrispondenza, 
viene restituito **NIL**
- se viene riscontrata una corrispondenza nel corso dell'esplorazione, viene 
restituito **T** (esplorazione ovviamente si arresta)

### Funzioni supportate
- __DEF-CLASS__

## PARTS-STRUCTURE

### Sintassi:
- `'(' parts-structure <parents> <parts>* ')'`

Crea la struttura della classe, scomponendo la parte dedicata agli attributi e 
quella dedicata ai metodi. 

- **`parts` non contiene sottoliste:** 
    - è stata creata una classe senza definire attributi e metodi. Nella
    struttura, le parti relative ai campi e ai metodi saranno inizializzate a 
    **NIL**
- **`parts` contiene UNA sottolista:**
    - verifica che la sottolista abbia come primo elemento la parola-chiave 
    *fields* o *methods*. In caso contrario viene lanciato un errore
        - a seconda della parola-chiave, viene lanciata **FIELDS-STRUCTURE** o
        **METHODS-STRUCTURE**, mentre il resto viene memorizzato come **NIL**.
        - si assicura di memorizzare nella struttura, indipendentemente
        da quale delle due è stata presentata, prima la parte relativa
        ai **campi** e poi quella relativa ai **metodi** 
- **`parts` contiene DUE sottoliste:**
    - verifica che le due sottoliste abbiano come primo elemento rispettivamente
    *fields* e *methods* (o viceversa). In caso contrario viene lanciato un
    errore.
        - si assicura di lanciare **FIELDS-STRUCTURE** e **METHODS-STRUCTURE**
        per le rispettive sottoliste
        - si assicura di memorizzare nella struttura, indipendentemente
        dall'ordine con le quali sono state presentate, prima la parte relativa
        ai **campi** e poi quella relativa ai **metodi** 

### Funzioni supportate
- __DEF-CLASS__

## FIELDS-STRUCTURE

### Sintassi:
- `'(' fields-structure <fields>* ')'`

Definisce la struttura della rappresentazione degli attributi della classe.\
Se un metodo viene definito più di una volta, viene lanciato un errore.\
Chiama **FIELD-DEFINITION** per ogni campo per controllarne la correttezza.

### Funzioni supportate
- __PARTS-STRUCTURE__

## FIELD-DEFINITION

### Sintassi:
- `'(' field-definition <current-field> ')'`

Viene creata/analizzata la tripla rappresentata da `current-field` 
`'(' <attributo> <valore> <tipo> ')'`:

- **`current-field` è una lista da un elemento -> `'(' <attributo> ')'`:**
    - `valore` viene inizializzato con **NIL** 
    - `tipo` viene inizializzato con **T**

- **`current-field` è una coppia -> `'(' <attributo> <valore> ')'`:**
    - `tipo` viene inizializzato con **T**

- **`current-field` è una tripla -> `'(' <attributo> <valore> <tipo> ')'`:**
    - se `tipo` non è un tipo primitivo in Common-Lisp o il nome di una classe, 
    viene lanciato un errore (controllo eseguito da **VALID-FIELD-TYPE**)
    - se `valore` non è del tipo indicato da `tipo` viene lanciato un errore
    (controllo eseguito da **TYPE-MATCHING**)
    - se `attributo` è presente in una delle superclassi della classe che si 
    vuole creare (controllo eseguito da **IS-INHERITED**)
        - se `tipo` non è un sottotipo del tipo indicato per `attributo` nella
        superclasse considerata, viene lanciato un errore
    - altrimenti la tripla viene inizializzata correttamente

### Funzioni supportate
- __FIELDS-STRUCTURE__

## METHODS-STRUCTURE

### Sintassi:
- `'(' method-structure <methods>* ')'`

Definisce la struttura della rappresentazione dei metodi della classe.\
Se un metodo viene definito più di una volta, viene lanciato un errore
Chiama **METHOD-DEFINITION** per ogni metodo per controllarne la correttezza.

### Funzioni supportate
- __PARTS-STRUCTURE__

## METHOD-DEFINITION

### Sintassi:
- `'(' method-definition <current-method> ')'`

Viene analizzato\creato il metodo corrente. `current-method` è una 
lista siffatta:

- `'(' <method-name> <method-args>* <method-body>* ')'`

Se le varie parti passano il controllo di correttezza, viene creata la coppia:

- `'(' <method-name> '.' <anonymous-function> ')'`

`anonymous-function` è la funzione anonima relativa a `method-name` 
restituita da **PROCESS-METHOD**

### Funzioni supportate
- __METHOD-STRUCTURE__

## INSTANCE_RAPRESENTATION

### Sintassi:
- `'(' instance-rapresentation <class-name> <fields>* ')'`

Definisce la rappresentazione di una istanza di una classe. Se i metodi da essa 
chiamati non restituiscono errori, viene creata l'istanza, costituente nella 
lista:

- `'(' 'OOLINST <class-name> <fields>* ')'`

`fields` è una lista che raccoglie gli attributi provenienti dalle superclassi 
e che non sono stati inizializzati nell'istanza, oltre ovviamente 
agli attributi inizializzati dalla stessa. 
Tutto questo salvo errori riguardo tipo, ereditarietà...

### Funzioni supportate
- __MAKE__

## FIELD-COMPOSITION-MAKE

### Sintassi:
- `'(' field-composition-make <fields>* ')'`

Crea e restituisce una lista di coppie (attributo valore) recuperando i valori 
dalla lista `fields` della **MAKE**

### Funzioni supportate
- __INSTANCE-RAPRESENTATION__

## GET-COMPLETE-PARENTS-FIELDS

### Sintassi:
- `'(' get-complete-parents-fields <parents-list>* ')'`

Risale l'albero delle superclassi a partire dalla classe attuale e, per ogni 
classe di `parents-list` , recupera la parte dedicata agli attributi,
ordinandola in una lista di coppie/triple/singoletti, a seconda della 
definizione di quest'ultimi.
L'esplorazione viene eseguita:

- **in "*profondità*"**: esplorando, per ogni classe di `parents-list`, anche il 
proprio albero delle superclassi.
- **in "*ampiezza*"**: esplorando tutti le classi appartenenti alle 
`parents-list` di ogni classe

>NOTA: l'ordine delle superclassi è importante quando la `parents-list` di
una classe contiene più genitori.

### Funzioni supportate 
- __INSTANCE-RAPRESENTATION__

## GET-COMPLETE-CLASS-FIELDS

### Sintassi:
- `'(' get-complete-class-fields <class-fields>* ')'`

Restituisce una lista contenente gli attrubuti della classe corrente.

### Funzioni supportate
- __INSTANCE-RAPRESENTATION__
- __GET-COMPLETE-PARENTS-FIELDS__

## FIELDS-FROM-PARENTS

### Sintassi:
- `'(' fields-from-parents <list-of-total-fields>* <fields-from-make>* ')'`

Notare che: 

- `list-of-total-fields` è la lista contenente tutti gli attributi
dall'albero delle superclassi
- `fields-from-make` è la lista contenente tutti gli attributi inizializzati
nell'istanza

Restituisce la lista degli attributi provenienti dalle superclassi. Vengono
considerati due casi:

- **`field-from-make` è vuota:** l'istanza è stata creata senza inizializzare
alcun attributo, quindi viene restituita `list-of-total-fields` così per come è
- **`field-from-make` contiene attributi:** l'istanza è stata creata 
inizializzando parte o tutti gli attributi provenienti dall'albero delle 
superclassi della classe di appartenenza, quindi viene controllato che ogni
attributo rispetti la specifica del **tipo** definita nella classe 
di appartenenza dello stesso

### Funzioni supportate
- __INSTANCE-RAPRESENTATION__

## LIST-FORMATTING-TO-MAKE

### Sintassi:
- `'(' list-formatting-to-make <list-def-class-format>* ')'`

Converte la lista degli attributi della classe `list-def-class-format`, nella
quale non ci sono errori di tipo, nella stessa lista in formato "*make*", ovvero 
creando coppie (attributo valore) e la restituisce.

### Funzioni supportate
- __FIELDS-FROM-PARENTS__

## FIELDS-FROM-PARENTS-ON-FIELD

### Sintassi 
- `'(' fields-from-parents-on-field <list-of-total-fields>* 
<field-from-make>')'`

Notare che `field-from-make` è una coppia (\<*field-name*\> \<*value*\>) 
estratta dalla creazione dell'istanza (**MAKE**) e `list-of-total-fields` 
ha lo stesso significato attribuitogli precedentemente.\
L'appartenenza del nome dell'attributo in `field-from-make` a 
`list-of-total-fields` è stata già verificata da **FIELD**.\
Controlla che il valore associato al nome dell'attributo nel `field-from-make` 
sia un **sottotipo** del tipo indicato nella definizione della classe alla 
quale l'attributo appartiene. 

- **il controllo termina con esito positivo**: 
    - `field-from-make` viene rimosso da `list-of-total-fields` poichè 
    rappresenta una ridefinizione corretta dello stesso.
- **il controllo termina con esito negativo**: 
    - il valore associato a `field-from-make` non è un sottotipo valido, 
    quindi viene lanciato un errore. 
    - è stato inizializzato lo stesso attributo più di una volta, quindi viene
    lanciato un errore

### Funzioni supportate
- __FIELDS-FROM-PARENTS__

## REMOVE-ATOM

### Sintassi:
- `'(' remove-atom <atom-to-remove> <list-of-lists>* ')'`

Restituisce la lista `list-of-lists` alla quale sono state rimosse tutte le 
sottoliste che hanno `atom-to-remove` come primo elemento.

### Funzioni supportate 
- __FIELDS-FROM-PARENTS-ON-FIELD__

## REMOVE-DUPLICATED-ELEMENTS 

### Sintassi:
- `'(' remove-duplicated-elements <list-of-lists>* ')'`

Restituisce la lista `list-of-lists` nella quale ogni sottolista che ha come
primo elemento un atomo è presente una volta sola (non saranno quindi presenti
sottoliste che hanno lo stesso primo elemento).

### Funzioni supportate
- __INSTANCE-RAPRESENTATION__

## GET-FIELDS-NAME

### Sintassi:
- `'(' get-fields-name <fields>* ')'`

Restituisce i nomi dei campi o dei metodi contenuti nella lista `fields`
(`methods`), che rappresenta gli attributi\metodi di un'istanza. 
La lista restituita conterrà i nomi dei campi\metodi in un formato leggibile.

### Funzioni supportate
- __MAKE__

## GET-FIELDS-NAME-OF-CLASS

### Sintassi: 
- `'(' get-fields-name-of-class <class-fields>* ')'`

Restituisce i nomi dei campi\metodi della classe, estratti dalla lista 
`class-fields`, che rappresenta la definizione della classe. 
La lista restituita conterrà i nomi dei campi\metodi in un formato leggibile.

### Funzioni supportate
- __IS-METHOD__
- __IS-INHERITED__
- __FIELDS-STRUCTURE__
- __METHODS-STRUCTURE__
- __GET-FIELDS-NAME-OF-CLASS__
- __FIELD__
- __FIELDS-FROM-PARENTS-ON-FIELD__
- __GET-PARENTS-FIELDS__

## SEARCH-FIELD-IN-PARENTS

### Sintassi: 
- `'(' search-field-in-parents <parents>* <field-name> ')'`

Ricerca ricorsivamente `field-name` in una lista di genitori `parents` 
e restituisce il valore associato ad esso se lo trova.

### Funzioni supportate
- __FIELD__

## METHOD*

### Sintassi: 
- `'(' method* <class-name> <method-list> ')'`

Premesso che `method-list` è una lista di metodi o un solo metodo, 
lancia **IS-METHOD** per ogni metodo presente.

- Restituisce **T** se ogni metodo in `method-list` è presente in `class-name`
(nome di una classe) o nelle sue superclassi.
- Restituisce **T** se ogni metodo in `method-list` è presente nella classe a 
cui appartiene `class-name` (nome di una istanza) o nelle sue superclassi.
- Altrimenti viene lanciato un errore  

### Funzioni supportate
- __GET-PARENTS-METHOD__

## GET-PARENTS-METHOD

### Sintassi: 
- `'(' get-parents-method <parents>* <method-name> ')'`

Ricerca ricorsivamente la presenza di un metodo in una lista di classi genitore 
e lo restituisce.

### Funzioni supportate
- __IS-METHOD__

## IS-METHOD

### Sintassi: 
- `'(' is-method <class-name> <method-name> ')'`

### `class-name` è il nome di una istanza:
Verifica che `method-name` sia presente nella classe di appartenenza di 
`class-name` o, eventualmente, nell'albero delle superclassi della stessa.\
Verifico inoltre che la sua Sintassi sia corretta. Viene lanciato un errore se:

- il metodo viene invocato con dei parametri errati
- il metodo non è presente nella classe di `class-name` o nell'albero delle 
superclassi

In caso positivo, invece, restituisce la **funzione anonima** associata ad esso.

### `class-name` è il nome di una classe:
Verifica che `method-name` sia presente in `class-name` o, eventualmente, 
nell'albero delle superclassi della stessa. Viene lanciato un errore se:

- il metodo non è presente in `class-name` o nell'albero delle sue superclassi.

In caso positivo, invece, restituisce la **funzione anonima** associata ad esso.

### Funzioni supportate
- __PROCESS-METHOD__
- __METHOD*__

## REWRITE-METHOD-CODE

### Sintassi: 
- `'(' rewrite-method-code <method-name> <method-spec>* ')'`

Riscrive il codice di un metodo specifico, restituendo una nuova specifica 
di metodo, che accetta come parametro aggiuntivo **this**.

### Funzioni supportate
- __PROCESS-METHOD__

## PROCESS-METHOD

### Sintassi: 
- `'(' rewrite-method-code <method-name> <method-spec>* ')'`

Aggiorna la definizione di un metodo, sostituendo la funzione originale con una
funzione lambda che richiama il metodo esistente. 
Inoltre, riscrive il codice del metodo usando la funzione 
**rewrite-method-code**.

### Funzioni supportate
- __DEF-CLASS__

## VALID-FIELD-TYPE

### Sintassi: 
- `'(' valid-field-type <field-type> ')'`

Accetta un tipo di dati (`field-type`) e verifica se è un tipo di dati valido
in Common Lisp o se rappresenta una classe esistente. 

- Restituisce **T** se il tipo di dati è valido.
- altrimenti genera un errore con un messaggio dettagliato.

TIPI VALIDI:
- 'number'
- 'integer'
- 'string'
- 'list'
- 'float'
- 'real'
- 'rational'
- 'complex'
- `class-name` (ovviamente se `class-name` è una classe esistente)

### Funzioni supportate
- __FIELD-DEFINITION__

## IS-INHERITED

### Sintassi: 
- `'(' is-inherited <parents>* <current-field> ')'`

Accetta una lista di classi (`parents`) e un campo corrente, e verifica se 
il campo è ereditato da una di queste classi parents. 

- Restituisce **T** se il campo è ereditato correttamente.
- altrimenti genera un errore con un messaggio dettagliato.

### Funzioni supportate
- __FIELD-DEFINITION__

## TYPE-MATCHING

### Sintassi:
- `'(' type-matching <current-field> ')'`

Accetta un campo corrente e verifica se il valore associato al campo è del 
tipo di dati specificato per quel campo. 

- In caso affermativo, la funzione restituirà **T**. 
- In caso contrario, genererà un errore con un messaggio informativo.

### Funzioni supportate
- __FIELD-DEFINITION__

## CONTAINS-DUPLICATES

### Sintassi: 
- `'(' contains-duplicates <list>* ')'`

accetta una lista come argomento e:

- restituisce **T** se la lista contiene
duplicati
- altrimenti restituisce **NIL**.

### Funzioni supportate
- __METHODS-STRUCTURE__
