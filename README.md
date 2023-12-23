# ProgettoOOL
Implementazione paradigma ad Oggetti in Common Lisp
## Introduzione
Ai tempi di Simula e del primo Smalltalk, molto molto tempo prima di Python, Ruby, Perl e SLDJ, i programmatori Lisp già producevano una pletora di linguaggi object oriented. Il vostro progetto consiste nella costruzione di un’estensione “object oriented” di Common Lisp, chiamata OOΛ, e di un’estensione “object oriented” di Prolog, chiamata OOΠ. OOΛ e un linguaggio object-oriented con eredita multipla. Il suo scopo e didattico e mira soprattutto ad evidenziare aspetti dell’implementazione di linguaggi object-oriented: (1) il problema di dove e come recuperare i valori ereditati, (2) come rappresentare i metodi e le loro chiamate e (3) come manipolare il codice nei metodi stessi.

## Variabili globali
### classes-database
```lisp 
(defparameter *classes-database* (make-hash-table))
```

Come struttura dati per la memorizzazione centralizzata delle classi a livello globale viene usata una hash-table.

## Primitive
### def-class
```lisp
(def-class <class-name> <parents> <parts>*)
```

#### Descrizione
`def-class` definisce la struttura di una classe e la memorizza in una locazione centralizzata (una variabile globale).

La classe per essere definita deve rispettare i seguenti requisiti:
- Nome atomico
- Nome non nullo
- Avere una lista di parents anche vuota.

Se anche solo uno di questi requisiti non è rispettato verrà restituito un errore.
#### Parametri
- `class-name`
	- Nome della classe che si vuole definire
- `parents`
	- Superclassi dalle quali la classe che si vuole definire discende.
- `parts`
	- Campi della classe
	- Insieme di definizioni di metodo

### add-to-class-database
```lisp
; code here
```

#### Descrizione
Lo scopo di questa funzione è aggiungere una definizione di classe nella tabella hash denominata *classes-database*. La chiave nella tabella hash è il parametro *name* e il valore corrispondente è il parametro di *class-definition*.

#### Parametri
- `name`
	- nome della classe
- `class-definition`
	- definizione della classe:
		- parents
		- metodi
		- campi

## Funzioni
