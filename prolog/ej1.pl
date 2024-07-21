arbol(A) :- A = bin(bin(bin(nil,4,nil), 2, bin(nil,5,bin(nil,6,nil))),1,bin(bin(nil,7,nil),3,nil)).

% camino(+A, -C)
camino(nil, []).
camino(bin(nil,V,nil), [V]).
camino(bin(I,V,_), [V|Xs]) :- I \= nil, camino(I, Xs).
camino(bin(_,V,D), [V|Xs]) :- D \= nil, camino(D, Xs).

% caminoMasLargo(+A, -C) 
caminoMasLargo(A,C) :- camino(A,C), length(C,L), not((camino(A,C1),length(C1,L1),L1>L)).

% caminoLongFija(+A, +N, -C) 
caminoLongFija(A,N,C) :- camino(A,C), length(C,N).

% caminoUnicoDeLong(+A, +N, -C) 
caminoUnicoDeLong(A,N,C) :- caminoLongFija(A,N,C), not((caminoLongFija(A,N,C1), C\=C1)).
