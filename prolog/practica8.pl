% Auxiliares:



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 4:

% juntar(?Lista1, ?Lista2, ?Lista3), tiene éxito si Lista3 es la concatenación de Lista1 y Lista2.
juntar([], Lista2, Lista2).
juntar([X|XS], Lista2, [X|Lista3]) :- juntar(XS, Lista2, Lista3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 6:

% aplanar(+Xs, -Ys)
aplanar([], []).
% PENDIENTE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 7:

% Item 1:
% palíndromo(+L, ?L1), donde L1 es un palíndromo cuya primera mitad es L.
palindromo(L, L1) :- append(L, R, L1), reverse(L, R).


% Item 2:
% iésimo(?I, +L, -X), donde X es el I-ésimo elemento de la lista L.
iesimo(0, [X|_], X).
iesimo(I, [_|YS], X) :- iesimo(I2, YS, X), I is I2+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 8:

% Item 1:
% intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respetando en L3 el orden en que aparecen los elementos en L1.
interseccion([], _, []).
interseccion([X|L1], L2, [X|L3]) :- sacarDuplicados(L2, T2), member(X, T2), borrar(L1, X, T1), interseccion(T1, T2, L3).
interseccion([X|L1], L2, L3)     :- not(member(X, T2)), interseccion(L1, T2, L3).


% Item 2:
% borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.
borrar([], _, []).                                                      % Caso base.
borrar([X|XS], X, ListaSinXs) :- borrar(XS, X, ListaSinXs).             % Caso recursivo: Si la cabeza es el elemento que queremos eliminar.
borrar([Y|YS], X, [Y|ListaSinXs]) :- X\=Y, borrar(YS, X, ListaSinXs).   % Caso recursivo: Si la cabeza NO es el elemento que queremos eliminar.


% Item 3:
% sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.
sacarDuplicados([], []).
sacarDuplicados([X|XS], [X|L2]) :- borrar(XS, X, ListaFiltrada), sacarDuplicados(ListaFiltrada, L2).


% Item 4:
% permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. ¿Hay una manera más eficiente de definir este predicado para cuando L2 está instanciada?
permutacion([], []).
permutacion([X|XS], P) :- permutacion(XS, L), insertar(X, L, P).


% insertar(+X, +L, ?LX), tiene exito si LX es la lista L con el elemento X agregado en cualquier posición.
insertar(X, L, LX) :- append(I, D, L), append(I, [X|D], LX).


% Item 5:
% reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.
reparto(L, 1, [L]).
reparto(L, N, [I|LListas]) :- N>1, append(I, D, L), N1 is N-1, reparto(D, N1, LListas).


% Item 6:
% repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de LListas puede ser vacía, y la longitud de LListas puede variar.
repartoSinVacias(L, LListas) :-
    length(L, Long),
    between(1, Long, N),
    reparto(L, N, LListas),
    not((member(XS, LListas), length(XS, 0))).


% No es lo que pide el enunciado, esta es un reparto/3 del item 5 con la condición de que ninguna de las listas de LListas puede ser vacía.
% repartoSinVaciasPrima(+L, +N, -LListas)
repartoSinVaciasPrima(L, 1, [L]) :- L \= [].     % La condición solo para evitar tener exito con el llamado directo: repartoSinVacias([], 1, LL)
repartoSinVaciasPrima(L, N, [I|LListas]) :-
    N>1, append(I, D, L),
    length(I, Long_I), Long_I > 0,
    length(D, Long_D), Long_D > 0,
    N1 is N-1,
    repartoSinVaciasPrima(D, N1, LListas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 9:

% elementosTomadosEnOrden(+L,+N,-Elementos) que tenga éxito si L es una lista, N ≥ 0 y Elementos es una lista de N elementos de L, preservando el orden en que aparecen en la lista original.
% PENDIENTE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 10:

% desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

% i. ¿Cómo deben instanciarse los parámetros para que el predicado funcione? (Es decir, para que no se cuelgue ni produzca un error). ¿Por qué?

% Deben instanciarse: desde(+X, -Y)
% X debe estar instanciada pues hacemos una operación aritmetica con ella cuando "N is X+1".
% Y debe estar sin instanciar. Si la pasamos instanciada, al pedir más soluciones se colgara, ya que intentará con desde(X+1, Y), desde(X+2, Y), desde(X+3, Y), ...
% creciendo infinitamente, habremos entrado en una resolución infinita.

% ii. Dar una nueva versión del predicado que funcione con la instanciación desde2(+X,?Y), tal que
%     si Y está instanciada, sea verdadero si Y es mayor o igual que X, y si no lo está genere todos los Y de X en adelante.

% desde2(+X, ?Y)
desde2(X, X).
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).
desde2(X, Y) :- nonvar(Y), X<Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 11:

