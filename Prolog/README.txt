Progetto realizzato da: 
GOLUB OLEKSANDRA 856706


Inizialmente viene dichiarata una serie di predicati come dinamici:
- dynamic graph/1 che gestisce l'esistenza di grafi;
- dynamic vertex/2 che gestisce i vertici all'interno dei grafi;
- dynamic edge/4 che gestisce gli archi tra i vertici nei grafi, compreso il loro peso;
- dynamic heap/2 che serve per rappresentare uno heap;
- dynamic heap_entry/4 che rappresenta le entrate all'interno dello heap;
- dynamic distance/3 che tiene traccia delle distanze calcolate;
- dynamic visited/2 che registra i vertici che sono stati visitati durante l'esplorazione di un grafo;
- dynamic previous/3 che tiene traccia dei predecessori dei vertici in un percorso;


MANIPOLAZIONE DI GRAFI:
new_graph(G)/1 
- G e' il nome del grafo;
- definisce un predicato per aggiungere un nuovo grafo;

delete_graph(G)/1 
- G e' il nome del grafo;
- definisce un predicato per eliminare un grafo;

new_vertex(G, V)/2 
- G e' il nome del grafo; 
- V e' il nome del vertice;
- definisce un predicato per aggiungere un vertice;

vertices(G, Vs)/2 
- G e' il nome del grafo;
- Vs e' la lista dei vertici del grafo;
- definisce un predicato per ottenere una lista di tutti i vertici di un grafo;

list_vertices(G)/1 
- G e' il nome del grafo; 
- definisce un predicato che stampa tutti i vertici del grafo G;

new_edge(G, U, V, Weight)/4 
- G e' il nome del grafo;
- U e' il nome del vertice sorgente;
- V e' il nome del vertice destinazione;
- Weight e' il peso dell'arco;
- definisce un predicato per aggiungere un arco;

new_edge(G, U, V)/3 
- G e' il nome del grafo;
- U e' il nome del vertice sorgente;
- V e' il nome del vertice destinazione;
- definisce un predicato per aggiungere un arco con Weight = 1;

edges(G, Es)/2 
- G e' il nome del grafo;
- Es e' la lista degli archi del grafo;
- definisce un predicato per ottenere una lista di tutti gli archi di un grafo;

neighbors(G, V, Ns)/3 
- G e' il nome del grafo;
- V e' il nome del vertice destinazione; 
- Ns e' la lista degli archi che congiungono V agli altri vertici;
- definisce un predicato per ottenere una lista di tutti i vicini di un vertice in un grafo;

list_edges(G)/1 
- G e' il nome del grafo;
- definisce un predicato per stampare una lista degli archi di un grafo;

list_graph(G)/1 
- G e' il nome del grafo;
- definisce un predicato per stampare una lista dei vertici e degli archi di un grafo;


MINHEAP:
new_heap(H)/1 
- H e' il nome di heap;
- definisce un predicato che inserisce un nuovo heap;

delete_heap(H)/1 
- H e' il nome di heap;
- definisce un predicato che rimuove tutto lo heap;

heap_size(H, S)/2 
- H e' il nome di heap;
- S e' la dimensione di heap;
- definisce un predicato che restituisce la dimensione corrente dello heap;

empty(H)/1 
- H e' il nome di heap;
- definisce un predicato che verifica se lo heap è vuoto;

not_empty(H)/1 
- H e' il nome di heap;
- definisce un predicato che verifica se lo heap non è vuoto;

head(H, K, V)/3 
- H e' il nome di heap;
- K e' la chiave dell'elemento da inserire;
- V e' il valore dell'elemento da inserire;
- definisce un predicato che restituisce l'elemento con chiave minima dello heap;

insert(H, K, V)/3 
- H e' il nome di heap;
- K e' la chiave dell'elemento da inserire;
- V e' il valore dell'elemento da inserire; 
- definisce un predicato che inserisce un elemento nello heap con una data chiave;

heapify_insert(H, I)/2 
- H e' il nome di heap;
- I e' l'indice della testa del sottoalbero;
- definisce un predicato che restituisce true se heap H soddisfa la proprieta' di heap;

extract(H, K, V)/3 
- H e' il nome di heap;
- K e' la chiave dell'elemento da inserire; 
- V e' il valore dell'elemento da inserire; 
- definisce un predicato che rimuove l'elemento con chiave minima dallo heap e restituisce la sua chiave e il suo valore;

heapify_extract(H, I)/2 
- H e' il nome di heap; 
- I e' l'indice della testa del sottoalbero;
- definisce un predicato che restituisce true se il sottoalbero di heap H con radice nell'elemento I soddisfa la proprieta' di heap;

smallest (H, X, Y, Smallest)/4 
- H e' il nome di heap; 
- X e' la posizione del primo elemento;
- Y e' la posizione del secondo elemento;
- Smallest e' la posizione dell'elemento piu' piccolo tra i due;
- definisce un predicato che determina tra due elementi quale ha la chiave minore, assumendo che entrambi gli elementi esistano;

swap(H, X, Y)/3 
- H e' il nome di heap;
- X e' la posizione del primo elemento;
- Y e' la posizione del secondo elemento;
- definisce un predicato che ritorma true se e' stato possibile scambiare gli elementi in posizione X ed Y all'interno dell'heap H e se l'heap soddisfa ancora la proprieta' di heap;

modify_key(H, NewKey, OldKey, V)/4 
- H e' il nome di heap; 
- NewKey e' la nuova chiave da assegnare all'elemento con chiave OldKey e valore V;
- OldKey e' la chiave dell'elemento da modificare;
- V e' il valore dell'elemento da modificare;
- definisce un predicato che modifica la chiave di un elemento dello heap e restituisce il suo valore;

move_up(H, P)/2 
- H e' il nome di heap;
- P e' la posizione dell’elemento dello heap che si vuole spostare;
- definisce un predicato che sposta un elemento verso l'alto se necessario;

move_down(H, P)/2 
- H e' il nome di heap;
- P e' la posizione dell’elemento dello heap che si vuole spostare;
- definisce un predicato che sposta un elemento verso il basso se necessario;

list_heap(H)/1 
- H e' il nome di heap;
- definisce un predicato che stampa lo stato interno dello heap;


SSSP:
change_distance(G, V, NewDist)/3
- G e' il nome del grafo;
- V e' il nome del vertice;
- NewDist e' la nuova distanza di un vertice V all'interno di un grafo G;
- definisce un predicato che serve per modificare la distanza di un vertice V all'interno di un grafo G a un nuovo valore NewDist

change_previous(G, V, U)/3
- G e' il nome del grafo;
- V e' il nome del vertice;
- U e' il nuovo predecessore del vertice V all'interno del grafo G;
- definisce un predicato che serve per cambiare il predecessore di un vertice V nel grafo G, impostandolo al vertice U

inserth(G, H, W)/3
- G e' il nome del grafo;
- V e' il nome del vertice;
- U e' il nuovo peso da assegnare al vertice H;
- definisce un predicato che serve per aggiornare il peso di un vertice nell'heap. Se l'entrata esiste già, ne modifica la chiave (peso); altrimenti, inserisce un nuovo elemento con quel peso;

neighborsV(G, V, Ns)/3
- G e' il nome del grafo;
- V e' il nome del vertice;
- Ns e' la lista dei vicini del vertice V;
- definisce un predicato che utilizza findall per creare una lista Ns contenente tutti i vertici connessi direttamente a V tramite un arco;

aggiorna(G, S, L)/3
- G e' il nome del grafo;
- S e' il nome del vertice sorgente;
- L e' la lista dei vertici da aggiornare;
- definisce un predicato che aggiorna distanze e predecessori di una lista di vertici partendo da un vertice sorgente, usato nell'algoritmo di Dijkstra;

dijkstra_sssp(G, Source)/2
- G e' il nome del grafo;
- S e' il nome del vertice sorgente;
- definisce un predicato che implementa l'algoritmo di Dijkstra per trovare i cammini minimi dal vertice sorgente a tutti gli altri vertici del grafo;

sp1(G, Source)/2
- G e' il nome del grafo;
- S e' il nome del vertice sorgente;
- definisce un predicato che inizializza l'algoritmo impostando le distanze iniziali;

sp2(G)/1
- G e' il nome del grafo;
- definisce un predicato che continua l'esecuzione di Dijkstra finché ci sono vertici da visitare;

add(X, Y, Z)/3
- X e' l'elemento da aggiungere;
- Y e' la lista originale;
- Z e' la lista risultante dopo l'aggiunta di X;
- definisce un predicato che aggiunge un elemento X alla lista Y, formando una nuova lista Z;

f(G, S, N, L1, L2, L3)/6
- G e' il nome del grafo;
- S e' il nome del vertice sorgente;
- N e' il nome del vertice corrente;
- L1, L2, e L3 sono liste utilizzate per accumulare i vertici visitati durante la ricostruzione del percorso;
- definisce un predicato che ricostruisce il percorso da un vertice N al vertice sorgente S;

archi(G, L, L1, L3)/4
- G e' il nome del grafo;
- L, L1, e L3 sono liste usate per accumulare gli archi del cammino più breve;
- definisce un predicato che raccoglie gli archi che formano il cammino più breve a partire da una lista di vertici;

shortest_path(G, Source, V, Path)/4
- G e' il nome del grafo;
- Source e' il nome del vertice sorgente;
- V e' il vertice destinazione;
- Path e' la lista degli archi che compongono il cammino più breve da Source a V;
- definisce un predicato che ricostruisce il percorso più breve dal vertice sorgente a un vertice destinazione, utilizzando i predicati f e archi per determinare il percorso e gli archi coinvolti;



DI SEGUITO SONO RIPORTATI DIVERSI TEST EFFETTUATI:

-> TEST DI 'new_graph/1', 'graph/1' e 'delete_graph/1':

?- new_graph(il_mio_grafo).
true.

?- new_graph(il_mio_grafo2).
true.

?- graph(G).
G = il_mio_grafo ;
G = il_mio_grafo_2.

?- delete_graph(il_mio_grafo2).
true.

?- graph(G).
G = il_mio_grafo.



-> TEST DI 'new_vertex/2', 'vertex/2', 'vertices/2' e 'list_vertices/1':

?- new_vertex(il_mio_grafo, v1).
true.

?- new_vertex(il_mio_grafo, v2).
true.

?- new_vertex(il_mio_grafo, v5).
true.

?- vertex(il_mio_grafo, Lista).
Lista = v1 ;
Lista = v2 ;
Lista = v5.

?- vertices(il_mio_grafo, Lista).
Lista = [v1, v2, v5].

?- list_vertices(il_mio_grafo).
:- dynamic vertex/2.
vertex(il_mio_grafo, v1).
vertex(il_mio_grafo, v2).
vertex(il_mio_grafo, v5).
true.



-> TEST DI 'new_edge/4', 'edge/4', 'edges/2' e 'list_edges/1':

?- new_edge(il_mio_grafo, v1, v5, 10).
true.

?- new_edge(il_mio_grafo, v1, v2, 14).
true.

?- edge(il_mio_grafo, v1, v5, 10).
true.

?- edge(il_mio_grafo, v1, v2, 14).
true.

?- edge(il_mio_grafo, v1, v5, 0).
false.

?- edges(il_mio_grafo, Lista).
Lista = [edge(il_mio_grafo, v1, v5, 10), edge(il_mio_grafo, v1, v2, 14)].

?- list_edges(il_mio_grafo).
:- dynamic edge/4.
edge(il_mio_grafo, v1, v5, 10).
:- dynamic edge/4.
edge(il_mio_grafo, v1, v2, 14).
true.



-> TEST di 'neighbors/3' e 'list_graph/1':

?- neighbors(il_mio_grafo, v1, Lista).
Lista = [edge(il_mio_grafo, v1, v5, 10), edge(il_mio_grafo, v1, v2, 14)].

?- list_graph(il_mio_grafo).
:- dynamic vertex/2.
vertex(il_mio_grafo, v1).
vertex(il_mio_grafo, v2).
vertex(il_mio_grafo, v5).
:- dynamic edge/4.
edge(il_mio_grafo, v1, v5, 10).
:- dynamic edge/4.
edge(il_mio_grafo, v1, v2, 14).
true.



-> TEST di 'new_heap/1', 'heap/2', 'delete_heap/1' e 'heap_size/2':

?- new_heap(mio_heap).
true.

?- new_heap(mio_heap2).
true.

?- new_heap(mio_heap3).
true.

%%%% Una query che chiede di trovare tutti gli heap H con dimensione 0
?- heap(H, 0).
H = mio_heap ;
H = mio_heap2.

%%%% Una query che chiede di trovare tutti gli heap H con dimensione 1
?- heap(H, 1).
false.

?- delete_heap(mio_heap2).
true.

?- heap(H, 0).
H = mio_heap ;
H = mio_heap3.

?- heap_size(mio_heap, S).
S = 0.

?- heap_size(mio_heap3, S).
S = 0.



-> TEST di 'insert/3', 'list_heap/1', 'head/3' e 'extract/3':

?- insert(mio_heap3, 1, "uno").
true.

?- insert(mio_heap3, 3, "tre").
true.

?- insert(mio_heap3, 4, "quattro").
true.

?- list_heap(mio_heap3).
:- dynamic heap_entry/4.
heap_entry(mio_heap3, 1, 1, "uno").
heap_entry(mio_heap3, 2, 3, "tre").
heap_entry(mio_heap3, 3, 4, "quattro").
true.

?- head(mio_heap3, K, V).
K = 1,
V = "uno".

?- heap_size(mio_heap3, S).
S = 3.

%%%% Una query che chiede di trovare tutti gli heap H con dimensione 3
?- heap(H, 3).
H = mio_heap3.

?- extract(mio_heap3, K, V).
K = 1,
V = "uno".

?- list_heap(mio_heap3).
:- dynamic heap_entry/4.
heap_entry(mio_heap3, 1, 3, "tre").
heap_entry(mio_heap3, 2, 4, "quattro").
true.

?- heap_size(mio_heap3, S).
S = 2.



-> TEST di 'empty/1', 'not_empty/1' e 'modify_key/4':

?- empty(H).
H = mio_heap.

?- not_empty(H).
H = mio_heap3.

?- modify_key(mio_heap3, 103, 3, "tre").
true .

?- list_heap(mio_heap3).
:- dynamic heap_entry/4.
heap_entry(mio_heap3, 1, 4, "quattro").
heap_entry(mio_heap3, 2, 103, "tre").
true.



-> TEST di 'change_distance/3', 'distance/3', 'change_previous/3', 'previous/3':

?- new_graph(mio_grafo_3).
true.

?- new_vertex(mio_grafo_3, s).
true.

?- new_vertex(mio_grafo_3, v1).
true.

?- new_vertex(mio_grafo_3, v2).
true.

?- change_distance(mio_grafo_3, v1, 10).
true.

?- distance(mio_grafo_3, v1, D).
D = 10.

?- distance(mio_grafo_3, V, 10).
V = v1.

?- change_previous(mio_grafo, v1, s).
true.

?- previous(mio_grafo, v1, Prev).
Prev = s.



-> TEST di 'dijkstra_sssp/2' e 'shortest_path/3':

?- new_graph(mio_grafo_3).
true.

?- new_vertex(mio_grafo_3, s).
true.

?- new_vertex(mio_grafo_3, v1).
true.

?- new_vertex(mio_grafo_3, v2).
true.

?- new_vertex(mio_grafo_3, v42).
true.

?- new_edge(mio_grafo_3, s, v1, 5).
true.

?- new_edge(mio_grafo_3, s, v2, 2).
true.

?- new_edge(mio_grafo_3, v1, v2, 1).
true.

?- new_edge(mio_grafo_3, v2, v1, 1).
true.

?- new_edge(mio_grafo_3, v2, v42, 2).
true.

?- new_edge(mio_grafo_3, v1, v42, 5).
true.

?- dijkstra_sssp(mio_grafo_3, s).
true.

?- shortest_path(mio_grafo_3, s, v2, ShortestPath).
ShortestPath = [edge(mio_grafo_3, s, v2, 2)].

?- shortest_path(mio_grafo_3, s, v1, ShortestPath).
ShortestPath = [edge(mio_grafo_3, v2, v1, 1), edge(mio_grafo_3, s, v2, 2)] 

?- shortest_path(mio_grafo_3, s, v42, ShortestPath).
ShortestPath = [edge(mio_grafo_3, s, v2, 2), edge(mio_grafo_3, v2, v42, 2)].