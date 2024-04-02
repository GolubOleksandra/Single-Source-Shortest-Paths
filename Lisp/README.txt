Progetto realizzato da: 
GOLUB OLEKSANDRA 856706



Le dichiarazioni di hash table iniziali 
- *graphs*, utilizzato per gestire l'esistenza di grafi; 
- *vertices*, utilizzato per gestire i vertici all'interno dei grafi;
- *edges*, utilizzato per gestire gli archi tra i vertici nei grafi, inclusi i loro pesi;
- *heaps*, dove ogni heap può essere visto come una collezione di "heap_entry", 
	dove ogni elemento dello heap contiene chiavi e valori (o priorità e elementi);
- *visited*, che tiene traccia delle distanze calcolate dall'algoritmo di Dijkstra per ogni vertice; 
- *distances*, che registra quali vertici sono stati visitati durante l'esplorazione del grafo; 
- *previous*, che tiene traccia dei predecessori dei vertici in un percorso, 
	consentendo la ricostruzione del percorso minimo una volta completato l'algoritmo di Dijkstra; 



MANIPOLAZIONE DI GRAFI:
is-graph (graph-id)
- graph-id e' l'identificatore del grafo;
- definisce una funzione che verifica l'esistenza di un grafo con l'identificatore specificato, restituendo il grafo se esiste;

new-graph (graph-id)
- graph-id e' l'identificatore del nuovo grafo da creare;
- definisce una funzione che aggiunge un nuovo grafo al sistema, utilizzando graph-id come chiave;

new-vertex (graph-id vertex-id)
- graph-id e' l'identificatore del grafo a cui aggiungere il vertice;
- vertex-id e' l'identificatore del nuovo vertice;
- definisce una funzione che aggiunge un nuovo vertice al grafo specificato, creando una coppia chiave-valore nella tabella hash dei vertici;

graph-vertices (graph-id)
- graph-id e' l'identificatore del grafo di cui si desidera ottenere i vertici;
- definisce una funzione che restituisce una lista di tutti i vertici appartenenti al grafo specificato;

new-edge (graph-id vertex1-id vertex2-id &optional (weight 1))
- graph-id e' l'identificatore del grafo a cui l'arco appartiene;
- vertex1-id e vertex2-id sono gli identificatori dei vertici che formano l'arco;
- weight e' il peso dell'arco, con un valore predefinito di 1 se non specificato;
- definisce una funzione che aggiunge un nuovo arco con un peso opzionale tra due vertici in un grafo;

graph-edges (graph-id)
- graph-id e' l'identificatore del grafo di cui si desidera ottenere gli archi;
- definisce una funzione che restituisce una lista di tutti gli archi appartenenti al grafo specificato;

graph-edges-sssp (graph-id v1 v2)
- graph-id, v1, v2 identificano rispettivamente il grafo e i due vertici che formano l'arco;
- definisce una funzione che restituisce gli archi specifici che collegano i vertici v1 a v2 all'interno del grafo graph-id;

graph-print (graph-id)
- graph-id e' l'identificatore del grafo da stampare;
- definisce una funzione che stampa una lista contenente sia i vertici che gli archi del grafo specificato;

graph-vertex-neighbors (graph-id vertex-id)
- graph-id e' l'identificatore del grafo;
- vertex-id e' l'identificatore del vertice di cui si desiderano trovare i vicini;
- definisce una funzione che restituisce una lista di tutti i vertici che sono direttamente connessi al vertice specificato tramite un arco;

delete-vertices (graph-id)
- graph-id e' l'identificatore del grafo dal quale rimuovere tutti i vertici;
- definisce una funzione che rimuove tutti i vertici appartenenti al grafo specificato;

delete-edges (graph-id)
- graph-id e' l'identificatore del grafo dal quale rimuovere tutti gli archi;
- definisce una funzione che rimuove tutti gli archi appartenenti al grafo specificato;

delete-graph (graph-id)
- graph-id e' l'identificatore del grafo da eliminare;
- definisce una funzione che elimina il grafo specificato e tutti i suoi vertici e archi associati;



MINHEAP:
new-heap (heap-id &optional (capacity 42))
- heap-id e' l'identificatore dell'heap da creare;
- capacity e' la capacità iniziale dell'heap, con un valore predefinito di 42;
- definisce una funzione che crea un nuovo heap con la capacità specificata o con quella predefinita se non specificata.

heap-id (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che restituisce l'identificatore dell'heap specificato;

heap-size (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che restituisce il numero di elementi attualmente presenti nell'heap;

heap-capacity (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che restituisce la capacità totale dell'heap, determinata dalla lunghezza dell'array che lo implementa.

heap-actual-heap (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che restituisce l'array che rappresenta l'heap;

heap-delete (heap-id)
- heap-id e' l'identificatore dell'heap da eliminare;
- definisce una funzione che rimuove l'heap specificato;

heap-empty (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che verifica se l'heap e' vuoto (non contiene elementi);

heap-not-empty (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che verifica se l'heap non e' vuoto (contiene almeno un elemento);

increase-heap-size (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che incrementa il contatore della dimensione dell'heap di 1;

decrease-heap-size (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che decrementa il contatore della dimensione dell'heap di 1;

heap-insert-last (heap-id K V)
- heap-id e' l'identificatore dell'heap;
- K e' la chiave dell'elemento da inserire;
- V e' il valore dell'elemento da inserire;
- definisce una funzione che inserisce un nuovo elemento alla fine dell'heap e aumenta la dimensione dell'heap;

heap-swap2 (heap-id p1 p0)
- heap-id, p1, p0 identificano rispettivamente l'heap e le posizioni degli elementi da scambiare;
- definisce una funzione che scambia due elementi all'interno dell'heap;

heap-heapify3 (heap-id s), heap-heapify (heap-id)
- heap-id e' l'identificatore dell'heap;
- s e' la dimensione dell'heap o la posizione da cui iniziare l'heapify;
- definisce una funzione che riordina l'heap per mantenere la proprietà di heap;

heap-insert (heap-id K V)
- heap-id e' l'identificatore dell'heap;
- K e' la chiave dell'elemento da inserire;
- V e' il valore dell'elemento da inserire;
- definisce una funzione che inserisce un elemento nell'heap mantenendo la proprietà di heap;

heap-extract (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che estrae e rimuove l'elemento in testa (radice) dell'heap, riorganizzando l'heap per mantenere la proprietà di heap;

heap-modify-key (heap-id new-key old-key)
- heap-id e' l'identificatore dell'heap;
- new-key e' la nuova chiave da assegnare all'elemento;
- old-key e' la chiave dell'elemento da modificare;
- definisce una funzione che modifica la chiave di un elemento nell'heap e riordina l'heap per mantenere la proprietà di heap;

heap-head (heap-id)
- heap-id e' l'identificatore dell'heap;
- definisce una funzione che restituisce l'elemento in testa (radice) dell'heap senza rimuoverlo;

heap-print (heap-id)
- heap-id e' l'identificatore dell'heap da stampare;
- definisce una funzione che stampa la rappresentazione dell'heap specificato;



SSSP:
set-distance (graph-id vertex-id distance)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- distance è la distanza da assegnare al vertice;
- definisce una funzione che assegna al vertice specificato la distanza data nel grafo indicato;

get-distance (graph-id vertex-id)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- definisce una funzione che ritorna la distanza del vertice specificato nel grafo indicato;

set-visited (graph-id vertex-id)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- definisce una funzione che definisce una funzione che segna come visitato il vertice specificato nel grafo indicato;

visited-p (graph-id vertex-id)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- definisce una funzione che verifica se il vertice specificato è stato visitato nel grafo indicato;

set-previous (graph-id vertex-id previous-vertex-id)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- previous-vertex-id è l'identificatore del vertice precedente;
- definisce una funzione che assegna al vertice specificato il vertice precedente nel percorso minimo nel grafo indicato;

get-previous (graph-id vertex-id)
- graph-id è l'identificatore del grafo;
- vertex-id è l'identificatore del vertice;
- definisce una funzione che ritorna il vertice precedente al vertice specificato nel percorso minimo nel grafo indicato;

sssp-dist (graph-id vertex-id)
- definisce una funzione che ritorna la distanza minima del vertice specificato dalla sorgente nel grafo indicato;

sssp-visited (graph-id vertex-id)
- definisce una funzione che verifica se il vertice specificato è stato visitato durante l'esecuzione dell'algoritmo di Dijkstra nel grafo indicato;

sssp-previous (graph-id vertex-id)
- definisce una funzione che ritorna il vertice precedente al vertice specificato nel percorso minimo dalla sorgente nel grafo indicato;

change-distance (graph-id vertex-id new-dist)
- definisce una funzione che modifica la distanza del vertice specificato con il nuovo valore nel grafo indicato;

change-previous (graph-id vertex-id new-previous)
- definisce una funzione che modifica il vertice precedente al vertice specificato con il nuovo vertice precedente nel grafo indicato;

sssp-change-dist (graph-id vertex-id new-dist)
- definisce una funzione che serve per modificare la distanza del vertice specificato con il nuovo valore nel grafo indicato;

sssp-change-previous (graph-id vertex-id new-previous)
- definisce una funzione che serve per modificare il vertice precedente al vertice specificato con il nuovo vertice precedente nel grafo indicato;

sssp-dijkstra (graph-id source-vertex-id)
- definisce una funzione che esegue l'algoritmo di Dijkstra sul grafo indicato partendo dal vertice sorgente specificato;

sssp-shortest-path (graph-id source-vertex-id target-vertex-id)
- definisce una funzione che ritorna il cammino minimo dal vertice sorgente al vertice di destinazione specificati nel grafo indicato;



DI SEGUITO SONO RIPORTATI DIVERSI TEST EFFETTUATI:

-> TEST DI 'is-graph', 'new-graph', 'new-vertex' e 'graph-vertices'

CL-USER 1 > (is-graph "grafo_test")
NIL
NIL

CL-USER 2 > (new-graph "grafo_test")
"grafo_test"

CL-USER 3 > (is-graph "grafo_test")
"grafo_test"
T

CL-USER 4 > (new-vertex "grafo_test" "v1")
(VERTEX "grafo_test" "v1")

CL-USER 5 > (new-vertex "grafo_test" "v2")
(VERTEX "grafo_test" "v2")

CL-USER 6 > (new-vertex "grafo_test" "v3")
(VERTEX "grafo_test" "v3")

CL-USER 7 >  (graph-vertices "grafo_test")
("v2" "v1" "v3")


-> TEST DI 'new-edge', 'graph-edges', 'graph-edges-sssp', 'graph-print' e 'graph-vertex-neighbors'

CL-USER 8 > (new-edge "grafo_test" "v1" "v2" 5)
(EDGE "grafo_test" "v1" "v2" 5)

CL-USER 9 > (graph-edges "grafo_test")
(("grafo_test" "v1" "v2" 5))

CL-USER 10 > (new-edge "grafo_test" "v1" "v3" 2)
(EDGE "grafo_test" "v1" "v3" 2)

CL-USER 11 > (graph-edges "grafo_test")
(("grafo_test" "v1" "v2" 5) ("grafo_test" "v1" "v3" 2))

CL-USER 12 > (graph-print "grafo_test")

("v2" "v1" "v3" ("grafo_test" "v1" "v2" 5) ("grafo_test" "v1" "v3" 2)) 
("v2" "v1" "v3" ("grafo_test" "v1" "v2" 5) ("grafo_test" "v1" "v3" 2))

CL-USER 14 > (graph-edges-sssp "grafo_test" "v1" "v2")
(("grafo_test" "v1" "v2" 5))

CL-USER 15 > (graph-vertex-neighbors "grafo_test" "v1")
(("v1" "v2" 5) ("v1" "v3" 2))

CL-USER 16 > (graph-vertex-neighbors "grafo_test" "v3")
NIL

CL-USER 17 > (graph-vertex-neighbors "grafo_test" "v2")
NIL

CL-USER 18 > (new-edge "grafo_test" "v2" "v1" 3)
(EDGE "grafo_test" "v2" "v1" 3)

CL-USER 19 > (new-edge "grafo_test" "v3" "v2" 4)
(EDGE "grafo_test" "v3" "v2" 4)

CL-USER 20 > (graph-vertex-neighbors "grafo_test" "v2")
(("v2" "v1" 3))

CL-USER 21 > (graph-vertex-neighbors "grafo_test" "v3")
(("v3" "v2" 4))


-> TEST DI 'delete-vertices', 'delete-edges' e 'delete-graph'

CL-USER 22 > (graph-vertices "grafo_test")
("v2" "v1" "v3")

CL-USER 23 > (graph-edges "grafo_test")
(("grafo_test" "v1" "v2" 5) ("grafo_test" "v1" "v3" 2) ("grafo_test" "v3" "v2" 4) ("grafo_test" "v2" "v1" 3))

CL-USER 24 > (delete-vertices "grafo_test")
NIL

CL-USER 25 > (graph-vertices "grafo_test")
NIL

CL-USER 26 > (delete-edges "grafo_test")
NIL

CL-USER 27 > (graph-edges "grafo_test")
NIL

CL-USER 28 > (new-graph "grafo_test_0")
"grafo_test_0"

CL-USER 29 > (is-graph "grafo_test_0")
"grafo_test_0"
T

CL-USER 30 > (delete-graph "grafo_test_0")
NIL

CL-USER 31 > (is-graph "grafo_test_0")
NIL
NIL


-> TEST DI 'new-heap', 'heap-id', 'heap-size', 'heap-capacity' e 'heap-actual-heap'

CL-USER 39 > (make-array 3)
#(NIL NIL NIL)

CL-USER 40 >  (defparameter a (make-array 3))
A

CL-USER 41 > a
#(NIL NIL NIL)

CL-USER 42 >  (aref a 1) 
NIL

CL-USER 43 > (setf (aref a 1) 42)
42

CL-USER 44 > a
#(NIL 42 NIL)

CL-USER 45 > (aref a 1)
42

CL-USER 46 > (new-heap "mio_heap")
(HEAP "mio_heap" 0 #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))

CL-USER 47 > (heap-id "mio_heap")
"mio_heap"

CL-USER 48 > (heap-size "mio_heap")
0

CL-USER 49 > (heap-capacity "mio_heap")
42

CL-USER 50 > (heap-actual-heap "mio_heap")
#(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)


-> TEST DI 'heap-insert', 'heap-head', 'heap-extract', 'heap-empty', 'heap-not-empty' e 'heap-delete'

CL-USER 51 > (heap-insert "mio_heap" 5 "Valore5")
T

CL-USER 52 > (heap-insert "mio_heap" 3 "Valore3")
T

CL-USER 53 > (heap-head "mio_heap")
(3 "Valore3")

CL-USER 54 > (heap-extract "mio_heap")
(3 "Valore3")

CL-USER 55 > (heap-size "mio_heap")
1

CL-USER 56 > (heap-empty "mio_heap")
NIL

CL-USER 57 > (heap-not-empty "mio_heap")
T

CL-USER 58 > (heap-delete "mio_heap")
T

CL-USER 59 > (is-graph "mio_heap")
NIL
NIL


-> TEST DI 'heap-modify-key' e 'heap-print'

CL-USER 60 > (new-heap "mio_heap_test")
(HEAP "mio_heap_test" 0 #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))

CL-USER 61 > (heap-insert "mio_heap_test" 10 "A")
T

CL-USER 62 > (heap-insert "mio_heap_test" 20 "B")
T

CL-USER 63 > (heap-insert "mio_heap_test" 15 "C")
T

CL-USER 64 >  (new-heap "mio_heap_test")
(HEAP "mio_heap_test" 3 #((10 "A") (15 "C") (20 "B") NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))

CL-USER 65 > (heap-modify-key "mio_heap_test" 5 20)
T

CL-USER 66 >  (new-heap "mio_heap_test")
(HEAP "mio_heap_test" 3 #((5 "B") (10 "A") (15 "C") NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))

CL-USER 67 > (heap-print "mio_heap_test")

(HEAP "mio_heap_test" 3 #((5 "B") (10 "A") (15 "C") NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)) 
(HEAP "mio_heap_test" 3 #((5 "B") (10 "A") (15 "C") NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))

CL-USER 68 > (heap-extract "mio_heap_test")
(5 "B")


-> TEST DI 'sssp-dijkstra' e 'sssp-shortest-path'

CL-USER 1 > (new-graph "mio_grafo_3")
"mio_grafo_3"

CL-USER 2 > (new-vertex "mio_grafo_3" "s")
(VERTEX "mio_grafo_3" "s")

CL-USER 3 > (new-vertex "mio_grafo_3" "v1")
(VERTEX "mio_grafo_3" "v1")

CL-USER 4 > (new-vertex "mio_grafo_3" "v2")
(VERTEX "mio_grafo_3" "v2")

CL-USER 5 > (new-vertex "mio_grafo_3" "v42")
(VERTEX "mio_grafo_3" "v42")

CL-USER 6 > (new-edge "mio_grafo_3" "s" "v1" 5)
(EDGE "mio_grafo_3" "s" "v1" 5)

CL-USER 7 > (new-edge "mio_grafo_3" "s" "v2" 2)
(EDGE "mio_grafo_3" "s" "v2" 2)

CL-USER 8 > (new-edge "mio_grafo_3" "v1" "v2" 1)
(EDGE "mio_grafo_3" "v1" "v2" 1)

CL-USER 9 > (new-edge "mio_grafo_3" "v2" "v1" 1)
(EDGE "mio_grafo_3" "v2" "v1" 1)

CL-USER 10 > (new-edge "mio_grafo_3" "v2" "v42" 2)
(EDGE "mio_grafo_3" "v2" "v42" 2)

CL-USER 11 > (new-edge "mio_grafo_3" "v1" "v42" 5)
(EDGE "mio_grafo_3" "v1" "v42" 5)

CL-USER 12 > (sssp-dijkstra "mio_grafo_3" "s")
NIL

CL-USER 13 > (sssp-shortest-path "mio_grafo_3" "s" "v42")
(("v2" "v42" 4) ("s" "v2" 2) "s")

CL-USER 15 > (sssp-shortest-path "mio_grafo_3" "s" "v1")
(("v2" "v1" 3) ("s" "v2" 2) "s")
