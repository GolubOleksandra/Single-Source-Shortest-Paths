%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic distance/3.
:- dynamic visited/2.
:- dynamic previous/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Definisci un predicato per aggiungere un nuovo grafo
new_graph(G) :- graph(G), !.          	
new_graph(G) :- assert(graph(G)), !.  	


% Definisci un predicato per eliminare un grafo 
delete_graph(G) :-
    graph(G), 							
    retractall(graph(G)), 				
    retractall(vertex(G, _)), 			
    retractall(edge(G, _, _, _)), !. 	


% Definisci un predicato per aggiungere un vertice
new_vertex(G, V) :-
    graph(G), 							
    \+ vertex(G, V), 					
    assert(vertex(G, V)), !. 			

% Definisci un predicato per ottenere una lista di tutti 
% i vertici di un grafo
vertices(G, Vs) :-
    graph(G), 							
    findall(V, vertex(G, V), Vs), !. 	


% Definisci un predicato che stampa tutti i vertici del grafo G
list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).


% Definisci un predicato per aggiungere un arco
new_edge(G, U, V, Weight) :-
    graph(G), 							
    vertex(G, U), 						
    vertex(G, V), 						
    \+ edge(G, U, V, _), 				
    assert(edge(G, U, V, Weight)), !. 	


% Definisci un predicato per aggiungere un arco con Weight = 1
new_edge(G, U, V) :- new_edge(G, U, V, 1).


% Definisci un predicato per ottenere una lista di tutti 
% gli archi di un grafo
edges(G, Es) :-
    graph(G), 							
    findall(edge(G, U, V, W), edge(G, U, V, W), Es), !. 	


% Definisci un predicato per ottenere una lista di tutti 
% i vicini di un vertice in un grafo
neighbors(G, V, Ns) :-
    graph(G), 							
    vertex(G, V), 						
    findall(edge(G, V, N, W), edge(G, V, N, W), Ns), !. 	


% Definisci un predicato per stampare una lista degli archi di un grafo
list_edges(G) :-
    edges(G, Es), 						
    listing(Es). 						


% Definisci un predicato per stampare una lista dei vertici 
% e degli archi di un grafo
list_graph(G) :-
    list_vertices(G), 					
    list_edges(G). 						
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Definisci un predicato che inserisce un nuovo heap
new_heap(H) :- heap(H, _S), !. 				
new_heap(H) :- assert(heap(H, 0)), !.	 	
	
	
% Definisci un predicato che rimuove tutto lo heap
delete_heap(H) :- 
    retractall(heap_entry(H, _, _, _)), 	
    retract(heap(H, _)), !. 				


% Definisci un predicato che restituisce 
% la dimensione corrente dello heap
heap_size(H, S) :- heap(H, S).
	

% Definisci un predicato che verifica se lo heap è vuoto
empty(H) :- heap_size(H, 0).
	

% Definisci un predicato che verifica se lo heap non è vuoto
not_empty(H) :- heap_size(H, S), S > 0.
	

% Definisci un predicato che restituisce l'elemento 
% con chiave minima dello heap
head(H, K, V) :- heap_entry(H, 1, K, V). 	


% Definisci un predicato che inserisce un elemento 
% nello heap con una data chiave
insert(H, K, V) :-
    new_heap(H),                       
    retract(heap(H, S)),               
    S1 is S + 1,                       
    assert(heap(H, S1)),               
    assert(heap_entry(H, S1, K, V)),   
    heapify_insert(H, S1).             
	
	
% Definisci un predicato che restituisce true se 
% heap H soddisfa la proprieta' di heap
heapify_insert(_, I) :-
    I =< 1,                            
    !.

heapify_insert(_, I) :-
    P is floor(I / 2),                 
    heap_entry(H, I, K, _),            
    heap_entry(H, P, PK, _),           
    K >= PK,                           
    !.                                 

heapify_insert(H, I) :-
    P is floor(I / 2),                 
    heap_entry(H, I, K, V),            
    heap_entry(H, P, PK, PV),          
    K < PK,                            
    !,
    retract(heap_entry(H, I, K, V)),   
    retract(heap_entry(H, P, PK, PV)), 
    assert(heap_entry(H, P, K, V)),    
    assert(heap_entry(H, I, PK, PV)),  
    heapify_insert(H, P).              
	
	
	
% Definisci un predicato che rimuove l'elemento 
% con chiave minima dallo heap e restituisce la sua chiave 
% e il suo valore
extract(H, K, V) :-
    head(H, K, V),                      
    heap_size(H, S),                    
    compare(=, S, 1),                   
    !,
    retract(heap_entry(H, 1, K, V)),    
    retract(heap(H, S)),                
    assert(heap(H, 0)).                 

extract(H, K, V) :-
    head(H, K, V),                      
    heap_size(H, S),                    
    S > 1,                              
    !,
    retract(heap_entry(H, 1, K, V)),    
    retract(heap_entry(H, S, SK, SV)),  
    assert(heap_entry(H, 1, SK, SV)),   
    retract(heap(H, S)),                
    S1 is S - 1,                        
    assert(heap(H, S1)),                
    heapify_extract(H, 1).              


% Definisci un predicato che restituisce true se il sottoalbero 
% di heap H con radice nell'elemento I soddisfa la proprieta' di heap
heapify_extract(H, I) :- 						
    L is I * 2,                         
    R is I * 2 + 1,                     
    heap_size(H, S),
    L > S, R > S,                       
    !. 

heapify_extract(H, I) :-						
    L is I * 2,
    R is I * 2 + 1,
    smallest(H, I, L, Stemp),           
    smallest(H, Stemp, R, Smallest),    
    !,
    swap(H, I, Smallest).               
	
	
% Definisci un predicato che determina tra due elementi quale ha 
% la chiave minore, assumendo che entrambi gli elementi esistano
smallest(H, X, Y, Smallest) :-
    heap_size(H, S),
    Y =< S,                             
    heap_entry(H, X, XK, _),            
    heap_entry(H, Y, YK, _),            
    YK =< XK,                           
    !,
    Smallest is Y.                      

smallest(H, X, Y, Smallest) :-
    heap_size(H, S),
    Y =< S,                             
    heap_entry(H, X, XK, _),            
    heap_entry(H, Y, YK, _),            
    YK > XK,                           
    !,
    Smallest is X.

smallest(H, X, Y, Smallest) :-
    heap_size(H, S),
    Y > S,                              
    !,
    Smallest is X.                      
	
	
% Definisci un predicato che ritorma true se e' stato possibile 
% scambiare gli elementi in posizione X ed Y all'interno dell'heap H 
% e se l'heap soddisfa ancora la proprieta' di heap
swap(_, X, Y) :-						
    compare(=, X, Y),
    !.

swap(H, X, Y) :-						
    X \= Y,
    !,
    retract(heap_entry(H, Y, YK, YV)),  
    retract(heap_entry(H, X, XK, XV)),  
    assert(heap_entry(H, X, YK, YV)),   
    assert(heap_entry(H, Y, XK, XV)),   
    heapify_extract(H, Y).              


% Definisci un predicato che modifica la chiave di un elemento 
% dello heap e restituisce il suo valore
modify_key(H, NewKey, OldKey, V) :- 
    heap_entry(H, P, OldKey, V), 				
    retract(heap_entry(H, P, OldKey, V)), 		
    assert(heap_entry(H, P, NewKey, V)), 		
    (NewKey < OldKey -> move_up(H, P) ; 
		move_down(H, P)). 						


% Definisci un predicato che sposta un elemento verso l'alto 
move_up(H, P) :- 								
    P > 1, 										
    PP is P // 2, 								
    heap_entry(H, P, K, V), 					
    heap_entry(H, PP, PK, PV), 					
    K < PK, 									
    retract(heap_entry(H, P, K, V)), 			
    retract(heap_entry(H, PP, PK, PV)), 		
    assert(heap_entry(H, P, PK, PV)), 			
    assert(heap_entry(H, PP, K, V)), 			
    move_up(H, PP). 							


% Definisci un predicato che sposta un elemento verso il basso 
move_down(H, P) :- 								
    heap_size(H, S), 							
    PL is P * 2, 								
    PR is P * 2 + 1, 							
    PL =< S, 									
    heap_entry(H, P, K, V), 					
    heap_entry(H, PL, KL, VL), 					
    (PR =< S -> heap_entry(H, PR, KR, VR) ; 
		KR is inf, VR is inf), 					
    (KL < KR -> (MinK, MinV, MinP) = 
		(KL, VL, PL) ; (MinK, MinV, MinP) = 
		(KR, VR, PR)), 							
    K > MinK, 									
    retract(heap_entry(H, P, K, V)), 			
    retract(heap_entry(H, MinP, MinK, MinV)), 	
    assert(heap_entry(H, P, MinK, MinV)), 		
    assert(heap_entry(H, MinP, K, V)), 			
    move_down(H, MinP). 						
move_down(H, P) :- 
    heap_size(H, S), 							
    PL is P * 2, 								
    PL > S, !. 									


% Definisci un predicato che stampa lo stato interno dello heap
list_heap(H) :- 
	listing(heap_entry(H, _, _, _)). 			



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Definisci un predicato che cambia la distanza di un vertice 
% a un nuovo valore
change_distance(G, V, NewDist) :-
    retractall(dist(G, V, _)),
    assert(dist(G, V, NewDist)).


% Definisci un predicato che cambia il predecessore di un vertice
change_previous(G, V, U) :-
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)).


% Definisci un predicato che inserisce o aggiorna un vertice 
% nell'heap con un nuovo peso
inserth(G, H, W) :-
    heap_entry(G, _, K, H),
    modify_key(G, W, K, H),
    !.
inserth(G, H, W) :-
    insert(G, W, H),
    !.


% Definisci un predicato che trova tutti i vicini di un vertice
neighborsV(G, V, Ns) :-
    vertex(G, V),
    findall(N, edge(G, V, N, _), Ns), !.


% Definisci un predicato che aggiorna distanze e i predecessori 
% per una lista di vertici
aggiorna(_, _, []).
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, H, W1),
    dist(G, S, W2),
    edge(G, S, H, W3),
    W4 is W2 + W3,
    W1  =< W4,
    aggiorna(G, S, T),
    !.
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, H, W1),
    dist(G, S, W2),
    edge(G, S, H, W3),
    W4 is W2 + W3,
    W1 > W4,
    change_distance(G, H, W4),
    change_previous(G, H, S),
    inserth(G, H, W4),
    aggiorna(G, S, T),
    !.
aggiorna(G, S, L) :-
    L = [H|T],
    dist(G, S, W1),
    edge(G, S, H, W2),
    W3 is W1 + W2,
    change_distance(G, H, W3),
    change_previous(G, H, S),
    inserth(G, H, W3),
    aggiorna(G, S, T),
    !.


% Definisci un predicato che esegue l'algoritmo di Dijkstra 
% a partire da un vertice sorgente
dijkstra_sssp(G, Source) :-
    sp1(G, Source),
    sp2(G).


% Inizializzazione di Dijkstra per un vertice sorgente
sp1(G, Source) :-
    vertex(G, Source),
    assert(dist(G, Source, 0)),
    assert(visited(G, Source)),
    new_heap(G),
    neighborsV(G, Source, L),
    aggiorna(G, Source, L),
    !.


% Continuaione dell'esecuzione dell'algoritmo di Dijkstra
% fino a quando lo heap non è vuoto
sp2(G) :-
    not_empty(G),
    extract(G, _, V),
    assert(visited(G, V)),
    neighborsV(G, V, L),
    aggiorna(G, V, L),
    sp2(G),
    !.
sp2(G) :-
    empty(G),
    !.


% Definisci un predicato che aggiunge un elemento a una lista
add(X, [], [X]).
add(X, Y, Z) :- append(Y, [X], Z), !.


% Definisci un predicato che ricostruisce il percorso 
% da un vertice alla sorgente
f(G, S, N, L1, L2, L3) :-
    N \= S,
    add(N, L1, L2),
    previous(G, N, X),
    f(G, S, X, L2, _, L3),
    !.
f(_, _, N, L1, _, L3) :-
    add(N, L1, L3).


% Definisci un predicato che raccoglie gli archi che 
% compongono il cammino più breve
archi(G, L, L1, L3) :-
    L = [H1, H2|T],
    findall(edge(G, H2, H1, W), edge(G, H2, H1, W), R1),
    append(R1, L1, R2),
    archi(G, [H2|T], R2, L3),
    !.
archi(_, _, L1, L3) :-
    L3 = L1,
    !.


% Definisci un predicato che ricostruisce il percorso più breve 
% dal vertice sorgente a un vertice destinazione
shortest_path(G, Source, V, Path) :-
    f(G, Source, V, [], _, R1),
    archi(G, R1, [], Path).
