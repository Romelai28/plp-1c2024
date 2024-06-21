

% Ejercicio 4:

% juntar(?Lista1, ?Lista2, ?Lista3), tiene éxito si Lista3 es la concatenación de Lista1 y Lista2.
juntar([], Lista2, Lista2).
juntar([X|XS], Lista2, [X|Lista3]) :- juntar(XS, Lista2, Lista3).


% Ejercicio 5:

% Item 1:
% last(?L, ?U), donde U es el último elemento de la lista L.
last(L, U) :- append(_, [U], L).


% Item 2:
% reverse(+L, -L1), donde L1 contiene los mismos elementos que L, pero en orden inverso.
reverse([], []).
reverse([X|XS], P) :- reverse(XS, R),  append(R, [X], P).


% Item 3:
% prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(P, L) :- append(P, _, L).


% Item 4:
% sufijo(?S, +L), donde S es sufijo de L.
sufijo(S, L) :- append(_, S, L).


% Item 5:
% sublista(?S, +L), donde S es sublista de L.
sublista([], _).
sublista([X|XS], L) :- prefijo(P, L), sufijo([X|XS], P).


% Item 6:
% pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.
pertenece(X,L) :- append(L1, _, L), append(_, [X], L1).

% Otra forma:
pertenecePrima(X,L) :- sublista([X], L).


% Ejercicio 6:

%aplanar(+Xs, -Ys)
aplanar([], []).
% PENDIENTE


% Ejercicio 7:

% Item 1:
% palíndromo(+L, ?L1), donde L1 es un palíndromo cuya primera mitad es L.
palindromo(L, L1) :- append(L, R, L1), reverse(L, R).

% Item 2:
% iésimo(?I, +L, -X), donde X es el I-ésimo elemento de la lista L.
iesimo(0, [X|_], X).
iesimo(I, [_|YS], X) :- iesimo(I2, YS, X), I is I2+1.


% Ejercicio 8:

% Item 1:


% Item 2:
% borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.
borrar([], _, []).                                                      % Caso base.
borrar([X|XS], X, ListaSinXs) :- borrar(XS, X, ListaSinXs).             % Caso recursivo: Si la cabeza es el elemento que queremos eliminar.
borrar([Y|YS], X, [Y|ListaSinXs]) :- X\=Y, borrar(YS, X, ListaSinXs).   % Caso recursivo: Si la cabeza NO es el elemento que queremos eliminar.


% Item 3:
% sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.
sacarDuplicados([], []).
sacarDuplicados([X|XS], [X|L2]) :- borrar(XS, X, ListaFiltrada), sacarDuplicados(ListaFiltrada, L2).

