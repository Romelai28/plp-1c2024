% Ejercicio 1:

% tablero(+Filas, +Columnas, -Tablero) 
% Instancia una estructura de tablero en blanco de Filas × Columnas, con todas las celdas libres.
% Requiere que F > 0 && C > 0:
tablero(0, C, []).
tablero(F, C, [Fila|Res]) :- F>0, F2 is F-1, crearFila(C, Fila), tablero(F2, C, Res).

% crearFila(+Largo, ?Lista)
% Si Lista no esta instanciada, devuelve true sii la lista tiene longitud igual al Largo
crearFila(0, []).
crearFila(Largo, [X|Resto]) :- Largo > 0, Largo2 is Largo -1, crearFila(Largo2, Resto).


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(X,Y), T) :- nth0(X, T, Fila), nth0(Y, Fila, ocupado).


tablero(ej5x5, T) :-
tablero(5, 5, T),
ocupar(pos(1, 1), T),
ocupar(pos(1, 2), T).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(X,Y), T, V) :- X1 is X+1, enRango(T, X1, Y), V = pos(X1, Y).  % Caso me voy abajo.
vecino(pos(X,Y), T, V) :- X1 is X-1, enRango(T, X1, Y), V = pos(X1, Y).  % Caso me voy arriba.
vecino(pos(X,Y), T, V) :- Y1 is Y+1, enRango(T, X, Y1), V = pos(X, Y1).  % Caso me voy derecha.
vecino(pos(X,Y), T, V) :- Y1 is Y-1, enRango(T, X, Y1), V = pos(X, Y1).  % Caso me voy izquierda.

% enRango(?T, +X, +Y)
% Cuando T no esta instanciado, genera infinitos (no todos) tableros en los que X, Y esten en rango.
enRango([T|TS], X, Y) :- length([T|TS], Long_X), 0 =< X, X < Long_X,
                         length(T, Long_Y), 0=< Y, Y < Long_Y.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(X,Y), T, V) :- vecino(pos(X,Y), T, V), estaLibre(V, T).

% estaLibre agregar especificación de los inputs!
estaLibre(pos(X,Y), T) :- nth0(X, T, Fila), nth0(Y, Fila, Elem), not(atom(Elem)).
