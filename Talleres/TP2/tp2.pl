%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Tableros auxiliares para testear:
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej2x2, T) :- tablero(2, 2, T), ocupar(pos(0,1), T).
tablero(ej2x2V2, T) :- tablero(2, 2, T), ocupar(pos(1,0), T).
tablero(libre2x2, T) :- tablero(2, 2, T).
tablero(libre3x3, T) :- tablero(3, 3, T).
tablero(ej3x3, T) :- tablero(3, 3, T), ocupar(pos(1, 0), T), ocupar(pos(0, 1), T).
tablero(pared, T) :- tablero(3,3,T), ocupar(pos(0,1), T), ocupar(pos(1,1),T), ocupar(pos(2,1),T).
tablero(ej4x5, T) :- tablero(4,5, T), ocupar(pos(2,1), T), ocupar(pos(1,3), T).

%% Auxiliares generales:
%% cant_filas(+T, ?F)
cant_filas(T, F) :- length(T, F).

%% cant_filas(+T, ?C)
cant_columnas([Fila|_], C) :- length(Fila, C).

%% desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

% paresNatQueSuman(?A, ?B, +Total)
% Por el contexto que la usamos (en ocupar cuando Tablero no está instanciado), solo considera los pares que cumplan (A >= 1 && B >= 1).
paresNatQueSuman(A, B, Total):- S is Total-1, between(1, S, A), B is Total-A.

% enRango(+Pos, +T)
enRango(pos(X,Y), T) :- cant_filas(T, F), 0 =< X, X < F, cant_columnas(T, C), 0 =< Y, Y < C.

% borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.
borrar([], _, []).                                                      % Caso base.
borrar([X|XS], X, ListaSinXs) :- borrar(XS, X, ListaSinXs).             % Caso recursivo: Si la cabeza es el elemento que queremos eliminar.
borrar([Y|YS], X, [Y|ListaSinXs]) :- X\=Y, borrar(YS, X, ListaSinXs).   % Caso recursivo: Si la cabeza NO es el elemento que queremos eliminar.

% sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.
sacarDuplicados([], []).
sacarDuplicados([X|XS], [X|L2]) :- borrar(XS, X, ListaFiltrada), sacarDuplicados(ListaFiltrada, L2).

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(F, C, []) :- F*C =:= 0.
tablero(F, C, [Fila|Res]) :- F>0, C>0,F2 is F-1, crearFila(C, Fila), tablero(F2, C, Res).

% crearFila(+Largo, ?Lista)
% Si Lista no esta instanciada, tiene éxito sii la lista tiene longitud igual al Largo.
crearFila(0, []).
crearFila(Largo, [_|Resto]) :- Largo > 0, Largo2 is Largo-1, crearFila(Largo2, Resto).


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(X,Y), T) :- nth0(X, T, Fila), nth0(Y, Fila, ocupado).  % ocupar indirectamente verifica que Pos este enRango por los nth0.
%% (Como en nuestro caso de uso el Suma no esta instanciado, usamos desde y no desde2 visto en clase)


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

%% Nuestra implementación es vecino(+Pos, +Tablero, ?PosVecino):
%% El Pos debe venir instanciado para poder operar 'X+1' y 'Y+1'.
%% El PosVecino puede venir o no instanciado:
    %% Cuando posVecino esta instanciado, verifica que sea un vecino y que sea una posición válida.
    %% Cuando posVecino no esta instanciado, se va instanciado en todas las posiciones que cumplan ser un vecino y que sea una posición válida.
%% OBS: Una posición inválida puede tener un vecino válido: Por ejemplo en un tablero de 3x3, pos(3,0) (invalida) tiene como vecino al pos(2,0).
%% Si no quisieramos ese comportamiento, bastaría con agregar que pos(X,Y) este en rango.
vecino(pos(X,Y), T, pos(X1, Y)) :- X1 is X+1, enRango(pos(X1,Y), T).  % Caso me voy abajo.
vecino(pos(X,Y), T, pos(X1, Y)) :- X1 is X-1, enRango(pos(X1,Y), T).  % Caso me voy arriba.
vecino(pos(X,Y), T, pos(X, Y1)) :- Y1 is Y+1, enRango(pos(X,Y1), T).  % Caso me voy derecha.
vecino(pos(X,Y), T, pos(X, Y1)) :- Y1 is Y-1, enRango(pos(X,Y1), T).  % Caso me voy izquierda.

%% Podríamos considerar vecino, y luego por lo tanto, vecinoLibre Generate&Test ya que genera todo el espacio de soluciones posibles (los 4 casilleros adyacentes) y testea si son válidos / válidos y libres, respectivamente.


%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

%% Nuestra implementación es vecinoLibre(+Pos, +Tablero, ?PosVecino)
%% Aprovechando que nuestras implementaciones son vecino(+Pos, +Tablero, ?V) y estaLibre(?V, +Tablero).
vecinoLibre(pos(X,Y), Tablero, V) :- vecino(pos(X,Y), Tablero, V), estaLibre(V, Tablero).

%% estaLibre(?Pos, +Tablero)
%% Si Pos esta instanciado, tiene éxito si en el elemento del tablero en la posición Pos esta libre.
%% Si Pos no esta instanciado, lo instancia en todas las posiciones que cumplan que esa posición en el tablero esta libre.
%% Su reversibilidad proviene gracias a la reversibilidad de nth0.
%% Además indirectamente verifica que Pos este enRango por el nth0.
estaLibre(pos(X,Y), Tablero) :- nth0(X, Tablero, Fila), nth0(Y, Fila, Elem), var(Elem).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

%% El caso de un camino de v a v, el único elemento del camino es si mismo.
camino(Inicio, Fin, Tablero, [Inicio|Camino]) :-
    estaLibre(Inicio, Tablero), estaLibre(Fin, Tablero),  % estaLibre indirectamente verifica que este enRango por el nth0.
    caminoAux(Inicio, Fin, Tablero, [Inicio], Camino).

%% caminoAux(+Inicio, ?Fin, +Tablero, +Visitados, ?Camino)
%% caminoAux nos permite tener una lista auxiliar con las posiciones visitadas.
%% Inicio debe venir instanciado porque es usado en vecinoLibre en el primer argumento.
caminoAux(Pos, Pos, _, _, []).  % Que Pos sea valida fue verificado en camino/4 por estaLibre
caminoAux(Inicio, Fin, Tablero, Visitados, [V|Camino]) :-
    vecinoLibre(Inicio, Tablero, V),
    not(member(V, Visitados)),
    caminoAux(V, Fin, Tablero, [V|Visitados], Camino).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%   Si camino no esta instanciada, entonces fin puede o no estar instanciada. Si esta instanciado caminoAux va a ir recorriendo los vecinos
%   de inicio y luego de los sucesivos v hasta llegar al fin y cuando llegue va a devolver el camino entre el inicio y el fin.
%   Si fin no esta instanciada, entonces va a buscar todos los caminos para toda instanciacion de fin en estaLibre(Fin, Tablero)
%   y va a devolver los caminos desde inicio hasta toda instanciacion posible de fin.
%   Si fin no esta instanciada, camino puede o no estar instanciada, si esta instanciado entra a caminoAux donde V y camino ya estan instanciados,
%   con una posicion libre del tablero que busco previamente en estaLibre(Fin, Tablero) y va recorriendo todas las celdas 
%   del camino hasta que camino sea [] y entonces si la posicion de fin es igual a la ultima v que recorrio, entonces la devuelve como una 
%   posicion posible y vuelve a camino y hace lo mismo hasta quedarse sin posiciones libres en el tablero, si no es igual vuelve a 
%   camino y sigue probando con posiciones libres del tablero que instancia en estaLibre(Fin, Tablero).
%   El caso donde no esta instanciado es el mismo de cuando para camino no instanciado, fin no esta instanciado.

% Notemos que en el caso en que camino no esté instanciado, V tampoco lo va a estar. Esto podría ser un problema para vecinoLibre por su especificación en el enunciado. Sin embargo, nuestra implementación es reversible respecto a V.  

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

%% Como Camino no debe tener repetidos, un camino no puede tener longitud más grande que F*C (La cantidad de casilleros), por lo tanto lo usamos como cota superior.
%% Los caminos tienen al menos un elemento (El caso de un camino de v a v, el único elemento del camino es si mismo) por lo tanto lo usamos como cota inferior.
camino2(Inicio , Fin, Tablero, Camino) :-
    cant_filas(Tablero, F), cant_columnas(Tablero, C),
    Techo is F*C, between(1, Techo, L),
    caminoLongFija(Inicio, Fin, Tablero, Camino, L).

% caminoLongFija(?Inicio, ?Fin, +Tablero, ?Camino, ?Longitud)
% Cuando Inicio, Fin no esta instanciado, Camino esta instanciado y Longitud no esta instanciado, instancia Inicio, Fin con el inicio y el fin del camino, la Longitud se instancia correctamente. (Falla si el camino no era válido)
% Cuando Camino no esta instanciado y Longitud no esta instanciado, instancia Camino y Longitud adecuadamente.
% Cuando Camino no esta instanciado y Longitud esta instanciado, instancia Camino solo con los caminos de longitud Longitud.
% Cuando Camino esta instanciado y Longitud no esta instanciado, instancia Longitud en la longitud del camino. (Falla si el camino no era válido)
caminoLongFija(Inicio, Fin, Tablero, Camino, Longitud) :- camino(Inicio, Fin, Tablero, Camino), length(Camino, Longitud).

%% La implamentacion de caminoLongFija es generate&Test porque utiliza el esquema, al generar todos los caminos candidatos y filtrar por la longitud


%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

%   camino2(?Inicio, +Fin, +Tablero, ?Camino)
%   Si inicio no está instanciado, camino puede o no estar instanciado. Si está instanciado, el programa va a entrar a caminoLongFija con los L generados en el between y va a dar false hasta llegar al caso donde L sea la longitud del camino y ahi va a llamar a camino que va a instanciar
%   posiciones libres del tablero en Inicio con estaLibre(Inicio, Tablero) y para cada una va a llamar a caminoAux que va a devolver si
%   el camino que le viene instanciado inicia en esa posicion o no. Si inicia lo va a devolver y va a seguir buscando. 
%   Si no, va a seguir buscando.
%   Si inicio no esta instanciado y camino tampoco, entonces va a para cada L generada por el between va a entrar a caminoLongFija que luego
%   va a llamar a camino que le va a generar todos los caminos para toda posicion libre que instancia como inicio en estaLibre(inicio, tablero)
%   hasta el fin que le pasa caminoLongFija y luego va a chequear cuales son los que cumplen la longitud.
%   Si camino no esta instanciado, e inicio si. En ese caso camino2 va a para cada L generada con 
%   el between(1, Techo) va a entrar a caminoLongFija que va a llamar a Camino que va a generar todos los caminos que inicien en Inicio
%   y terminen en Fin y se va a quedar con los de longitud L. Va a hacer eso hasta recorrer todos los L y ahi va a terminar una vez que el 
%   between termine.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio, Fin, Tablero, Camino) :-
    caminoLongFija(Inicio, Fin, Tablero, Camino, L),
    not((caminoLongFija(Inicio, Fin, Tablero, Camino2, L2), L2 < L)).
%% Camino2 puede ser remplazado por '_' pero resultaba más declarativo.

%% Usamos el esquema Generate&Test porque dado un camino, generamos todos los caminos y chequeamos que ninguno sea más corto que él.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(Inicio, Fin, Tablero1, Tablero2, Camino) :-
        camino(Inicio, Fin, Tablero1, Camino), camino(Inicio, Fin, Tablero2, Camino).

%% Esta implementacion de caminoDual es Generate&Test, porque primero busca todas las soluciones candidatas que van a ser los caminos
%% entre Inicio y Fin en el tablero1 y luego filtra cuales de esos caminos valen en el tablero2. Entonces caminoDual utiliza el esquema de 
%% Generate&Test.

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(6). 
testTablero(1) :- tablero(0,0,[]). 
testTablero(2) :- tablero(2,1,[[_],[_]]).
testTablero(3) :- tablero(2,3, [[_,_,_],[_,_,_]]).
testTablero(4) :- ocupar(pos(0,0), [[ocupado]]).
testTablero(5) :- ocupar(pos(1,1), [[_, _], [_,_]]). 
testTablero(6) :- not(ocupar(pos(1, 1), [[ocupado, _]])).

cantidadTestsVecino(9). 
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)). % Caso en el que parto de una posición válida y caigo en un vecino válido.
testVecino(2) :- vecino(pos(0,2), [[_,_]], pos(0,1)). % Caso en el que parto de una posición invalida y caigo en un vecino válido.
testVecino(3) :- not(vecino(pos(1,1), [[_,_],[_,_]], pos(2,1))). % Caso en el que parto de una posición válida y caigo en un vecino invalido.
testVecino(4) :- not(vecino(pos(3,1), [[_,_],[_,_]], pos(3, 2))). % Caso en el que parto de una posición invalida y caigo en un vecino invalido.
testVecino(5) :- not(vecino(pos(0,0), [[_]], V)). % Caso en el que no tengo vecinos.
testVecino(6) :- vecinoLibre(pos(0,0), [[_,_]], pos(0,1)). % Caso en el que parto de una posición válida y tengo un vecino libre
testVecino(7) :- not(vecinoLibre(pos(1,0), [[ocupado,_],[_,ocupado]], V)). % Caso en el que parto de una posición válida y no tengo vecinos libres.
testVecino(8) :- vecinoLibre(pos(0,0), [[_,_],[_,_]], pos(0,1)), vecinoLibre(pos(0,0), [[_,_],[_,_]], pos(1,0)). % Caso en el que parto de una posición válida y tengo dos vecinos libres.
testVecino(9) :- vecinoLibre(pos(1,0), [[_,_]], pos(0,0)). % Caso en el que parto de una posición invalida y tengo un único vecino libre.
testVecino(10) :- tablero(libre3x3, T), bagof(V, vecino(pos(1,1), T, V), ListaVecinos), length(ListaVecinos, 4).
testVecino(11) :- tablero(libre2x2, T), bagof(V, vecino(pos(0,0), T, V), ListaVecinos), length(ListaVecinos, 2).
testVecino(12) :- tablero(ej5x5, T), bagof(V, vecinoLibre(pos(1,3), T, V), ListaVecinos), length(ListaVecinos, 3).
testVecino(13) :- tablero(ej5x5, T), bagof(V, vecinoLibre(pos(0,1), T, V), ListaVecinos), length(ListaVecinos, 2).

cantidadTestsCamino(27).
%% Tests para camino: 
testCamino(1) :- tablero(ej5x5, T), bagof(Camino, (camino(pos(0,0), pos(2,3), T, Camino)), ListaCaminos), length(ListaCaminos, Longitud), Longitud == 287. % Chequeo que la cantidad de caminos posibles distintos es igual a 287 que es la respuesta correcta.

%% No hay casilleros repetidos en Camino.
testCamino(2) :-
    tablero(ej5x5, T),
    not((camino(pos(3,3), pos(2,1), T, Camino),
    length(Camino, L),
    sacarDuplicados(Camino, CaminoSinRepes),
    length(CaminoSinRepes, L1),
    L \= L1)).

testCamino(3) :- tablero(libre2x2, T), camino(pos(0,0), pos(1,0), T, [pos(0,0),pos(1,0)]), camino(pos(0,0), pos(1,0), T, [pos(0,0),pos(0,1),pos(1,1),pos(1,0)]), bagof(Camino, (camino(pos(0,0),pos(1,0), T, Camino)), ListaCaminos), length(ListaCaminos, 2). % Chequeo que solo haya dos posibles caminos desde pos(0,0) a pos(1,0) y que sean los correctos.
testCamino(4) :- tablero(libre3x3, T), camino(pos(0,0), pos(0,0), T, [pos(0,0)]), bagof(Camino, (camino(pos(0,0), pos(0,0), T, Camino)), ListaCaminos), length(ListaCaminos, 1). % Chequeo que solo haya un camino posible de una casilla hacia si misma y ese camino está conformado por la posición misma.
testCamino(5) :- tablero(ej3x3, T), not(camino(pos(0,0),pos(1,1), T, Camino)). % Caso en el que no existe camino entre los nodos.
testCamino(6) :- tablero(2, 2, T), not(camino(pos(1,1), pos(2,2), T, C)).  %% No existe un camino que empiece dentro del tablero y termine en una posición de afuera.
testCamino(7) :- tablero(2, 2, T), not(camino(pos(3,2), pos(0,0), T, C)).  %% No existe un camino que empiece fuera del tablero y comience en una posición de afuera.
testCamino(8) :- tablero(ej4x5, T), not(camino(pos(1,3), pos(0,0), T, Camino)).  %% Camino imposible: empieza en una casilla ocupada.
testCamino(9) :- tablero(ej4x5, T), not(camino(pos(0,0), pos(1,3), T, Camino)).  %% Camino imposible: termina en una casilla ocupada.
testCamino(10) :- tablero(ej4x5, T), not(camino(pos(2,1), pos(1,3), T, Camino)).  %% Camino imposible: empieza y termina en una casila ocupada.
testCamino(11) :- tablero(ej4x5, T), not(camino(pos(1,3), pos(1,3), T, Camino)).  %% Camino imposible: camino a si mismo, caso ocupado.
testCamino(12) :- tablero(ej4x5, T), camino(pos(0,0), pos(0,0), T, [pos(0,0)]).  %% Camino posible: camino a si mismo, caso libre.
testCamino(13) :- tablero(ej5x5, T), camino(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(2,1)]).  %% Camino válido y óptimo.
testCamino(14) :- tablero(ej5x5, T), camino(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(3,0), pos(2,0), pos(2,1)]). %% Camino válido pero no óptimo.


%% Tests para camino2:

%% No hay casilleros repetidos en Camino.
testCamino(15) :-
    tablero(ej5x5, T),
    not((camino2(pos(3,3), pos(2,1), T, Camino),
    length(Camino, L),
    sacarDuplicados(Camino, CaminoSinRepes),
    length(CaminoSinRepes, L1),
    L \= L1)).

%% Testea que se cumpla la propiedad de camino2 de dar los caminos de manera creciente en longitudes. 
testCamino(16) :-
    tablero(ej5x5, T),
    findall(Long, (camino2(pos(0,0), pos(2,3), T, Camino), length(Camino, Long)), ListaLongitudes),
    length(ListaLongitudes, Size),
    Size2 is Size-2,
    not((between(0, Size2, I),
    Iplus1 is I+1,
    nth0(I, ListaLongitudes, LongCamino),
    nth0(Iplus1, ListaLongitudes, LongCamino2),
    LongCamino > LongCamino2)).

testCamino(17) :- tablero(pared, T), not(camino2(pos(0,0), pos(2,2), T, C)).  %% Camino imposible: porque debe cruzar la pared.
testCamino(18) :- tablero(2, 2, T), not(camino2(pos(1,1), pos(2,2), T, C)).  %% No existe un camino que empiece dentro del tablero y termine en una posición de afuera.
testCamino(19) :- tablero(2, 2, T), not(camino2(pos(3,2), pos(0,0), T, C)).  %% No existe un camino que empiece fuera del tablero y comience en una posición de afuera.
testCamino(20) :- tablero(ej4x5, T), not(camino2(pos(1,3), pos(0,0), T, Camino)).  %% Camino imposible: empieza en una casilla ocupada.
testCamino(21) :- tablero(ej4x5, T), not(camino2(pos(0,0), pos(1,3), T, Camino)).  %% Camino imposible: termina en una casilla ocupada.
testCamino(22) :- tablero(ej4x5, T), not(camino2(pos(2,1), pos(1,3), T, Camino)).  %% Camino imposible: empieza y termina en una casila ocupada.
testCamino(23) :- tablero(ej4x5, T), not(camino2(pos(1,3), pos(1,3), T, Camino)).  %% Camino imposible: camino a si mismo, caso ocupado.
testCamino(24) :- tablero(ej4x5, T), camino2(pos(0,0), pos(0,0), T, [pos(0,0)]).  %% Camino posible: camino a si mismo, caso libre.
testCamino(25) :- tablero(ej5x5, T), camino2(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(2,1)]).  %% Camino válido y óptimo.
testCamino(26) :- tablero(ej5x5, T), camino2(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(3,0), pos(2,0), pos(2,1)]). %% Camino válido pero no óptimo.
testCamino(27) :- tablero(ej5x5, T), bagof(Camino, (camino2(pos(0,0), pos(2,3), T, Camino)), ListaCaminos), length(ListaCaminos, Longitud), Longitud == 287.


cantidadTestsCaminoOptimo(14).
testCaminoOptimo(1) :- tablero(ej5x5, T), caminoOptimo(pos(3,3), pos(2,1), T, Camino), length(Camino, 4).  %% No tiene casillas ocupadas en el camino directo.
testCaminoOptimo(2) :- tablero(ej5x5, T), caminoOptimo(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(2,1)]).
testCaminoOptimo(3) :- tablero(ej5x5, T), caminoOptimo(pos(1,3), pos(1,0), T, Camino), length(Camino, 6).  %% Tiene que desviarse del camino directo para evitar las casillas ocupadas.

testCaminoOptimo(4) :- tablero(pared, T), not(caminoOptimo(pos(0,0), pos(2,2), T, C)).  %% Camino imposible: porque debe cruzar la pared.
testCaminoOptimo(5) :- tablero(2, 2, T), not(caminoOptimo(pos(1,1), pos(2,2), T, C)).  %% No existe un camino que empiece dentro del tablero y termine en una posición de afuera.
testCaminoOptimo(6) :- tablero(2, 2, T), not(caminoOptimo(pos(3,2), pos(0,0), T, C)).  %% No existe un camino que empiece fuera del tablero y comience en una posición de afuera.
testCaminoOptimo(7) :- tablero(ej4x5, T), not(caminoOptimo(pos(1,3), pos(0,0), T, Camino)).  %% Camino imposible: empieza en una casilla ocupada.
testCaminoOptimo(8) :- tablero(ej4x5, T), not(caminoOptimo(pos(0,0), pos(1,3), T, Camino)).  %% Camino imposible: termina en una casilla ocupada.
testCaminoOptimo(9) :- tablero(ej4x5, T), not(caminoOptimo(pos(2,1), pos(1,3), T, Camino)).  %% Camino imposible: empieza y termina en una casila ocupada.
testCaminoOptimo(10) :- tablero(ej4x5, T), not(caminoOptimo(pos(1,3), pos(1,3), T, Camino)).  %% Camino imposible: camino a si mismo, caso ocupado.
testCaminoOptimo(11) :- tablero(ej4x5, T), caminoOptimo(pos(0,0), pos(0,0), T, [pos(0,0)]).  %% Camino posible: camino a si mismo, caso libre.
testCaminoOptimo(12) :- tablero(ej5x5, T), caminoOptimo(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(2,1)]).  %% Camino optimo.
testCaminoOptimo(13) :- tablero(ej5x5, T), not(caminoOptimo(pos(3,3), pos(2,1), T, [pos(3,3), pos(3,2), pos(3,1), pos(3,0), pos(2,0), pos(2,1)])). %% Camino no optimo.

%% No hay casilleros repetidos en Camino.
testCaminoOptimo(14) :-
    tablero(ej5x5, T),
    not((caminoOptimo(pos(3,3), pos(2,1), T, Camino),
    length(Camino, L),
    sacarDuplicados(Camino, CaminoSinRepes),
    length(CaminoSinRepes, L1),
    L \= L1)).

cantidadTestsCaminoDual(3). 
testCaminoDual(1) :- tablero(libre2x2, T1), tablero(libre3x3, T2), caminoDual(pos(0,0),pos(1,0),T1, T2, Camino). % Chequeo que Camino sea un camino válido desde las posicines indicadas para un tablero libre de 2x2 y uno libre de 3x3.
testCaminoDual(2) :- tablero(libre3x3, T1), tablero(ej3x3, T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, Camino)). % Caso en el que solo existe el camino en un tablero y en el otro no. 
testCaminoDual(3) :- tablero(ej2x2, T1), tablero(ej2x2V2, T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, Camino)). % Caso en el que existe camino en ambos tableros, pero es distinto. 

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).
