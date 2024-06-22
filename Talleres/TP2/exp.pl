% Completar especificación de instanciaciones de parametros.
n_filas(T, F) :- length(T, F).

% Completar especificación de instanciaciones de parametros.
n_columnas([Fila|_], C) :- length(Fila, C).



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
ocupar(pos(X,Y), T) :- nth0(X, T, Fila), nth0(Y, Fila, ocupado).  % ocupar indirectamente checkea que este enRango por el nth0.

% ejemplo:
tablero(ej5x5, T) :-
tablero(5, 5, T),
ocupar(pos(1, 1), T),
ocupar(pos(1, 2), T).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(X,Y), T, V) :- X1 is X+1, enRango(pos(X1,Y), T), V = pos(X1, Y).  % Caso me voy abajo.
vecino(pos(X,Y), T, V) :- X1 is X-1, enRango(pos(X1,Y), T), V = pos(X1, Y).  % Caso me voy arriba.
vecino(pos(X,Y), T, V) :- Y1 is Y+1, enRango(pos(X,Y1), T), V = pos(X, Y1).  % Caso me voy derecha.
vecino(pos(X,Y), T, V) :- Y1 is Y-1, enRango(pos(X,Y1), T), V = pos(X, Y1).  % Caso me voy izquierda.

% enRango(+Pos, ?T)
% Cuando T no esta instanciado, genera infinitos (no todos) tableros en los que X, Y esten en rango.
enRango(pos(X,Y), [T|TS]) :- length([T|TS], Long_X), 0 =< X, X < Long_X,
                             length(T, Long_Y), 0=< Y, Y < Long_Y.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(X,Y), T, V) :- vecino(pos(X,Y), T, V), estaLibre(V, T).

% estaLibre agregar especificación de los inputs!
estaLibre(pos(X,Y), T) :- nth0(X, T, Fila), nth0(Y, Fila, Elem), not(atom(Elem)).


%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

camino(Inicio, Fin, T, [Inicio|Camino]) :-
    estaLibre(Inicio, T), estaLibre(Fin, T),  % estaLibre indirectamente checkea que este enRango por el nth0.
    caminoAux(Inicio, Fin, T, [Inicio], Camino).


caminoAux(Pos, Pos, T, Visitados, []).
caminoAux(Inicio, Fin, T, Visitados, [V|Camino]) :-
    vecinoLibre(Inicio, T, V),
    not(member(V, Visitados)),
    caminoAux(V, Fin, T, [V|Visitados], Camino).

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(Inicio , Fin, Tablero, Camino) :-
    n_filas(Tablero, F), n_columnas(Tablero, C),
    Techo is (F*C), camino2Aux(Inicio, Fin, Tablero, Camino, Techo, Techo).

camino2Aux(Inicio, Fin, Tablero, Camino, Iterador, Techo) :-
    0=< Iterador, Iterador =< Techo,
    It2 is Iterador-1,
    camino2Aux(Inicio, Fin, Tablero, Camino, It2, Techo).

camino2Aux(Inicio, Fin, Tablero, Camino, Iterador, Techo) :-
    0=< Iterador, Iterador =< Techo,
    caminoLongFija(Inicio, Fin, Tablero, Camino, Iterador).

caminoLongFija(Inicio, Fin, Tablero, Camino, Longitud) :- camino(Inicio, Fin, Tablero, Camino), length(Camino, N), N =:= Longitud.


%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(Inicio, Fin, Tablero1, Tablero2, Camino) :-
    unionTableros(Tablero1, Tablero2, T),
    camino(Inicio, Fin, T, Camino).

unionTableros(T1, T2, TableroRes) :-
    n_filas(T1, F1), n_filas(T2, F2),
    F is min(F1, F2),
    n_columnas(T1, C1), n_columnas(T2, C2),
    C is min(C1, C2),
    tablero(F, C, TableroRes).
    % TERMINAR DE ESCRIBIR LA FUNCIÓN, QUEDA COPIAR LOS OCUPADO DE TABLERO1 A TABLERO RES Y COPIAR LOS OCUPADO DE TABLERO2 A TABLERO RES !!!
    