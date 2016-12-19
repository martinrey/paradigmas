% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

listaNats(LInf, LSup, []) :- LInf > LSup.
listaNats(LInf, LSup, [H|T]) :- 
	LInf =< LSup, 
	H is LInf,
	listaNats(LInf+1, LSup, T).
	
	
%%% TESTS EJ 1
:- begin_tests(ej1).
:- use_module(library(lists)).
test(sinInstanciar, [nondet]) :- listaNats(1, 3, X), member(1, X), member(2, X), member(3, X), length(X, 3).
test(instanciado, [nondet]) :- listaNats(1, 3, [1,2,3]).
test(vacio, [nondet]) :- listaNats(2, 1, []).
test(lInfIgualALSup, [nondet]) :- listaNats(2, 2, [2]).
:- end_tests(ej1).


%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.

nPiezasDeCada(_, [], []).
nPiezasDeCada(Cant, [T|TS], [P|PS]) :- P = pieza(T, Cant), nPiezasDeCada(Cant, TS, PS).

%%% TESTS EJ 2
:- begin_tests(ej2).
test(sinInstanciar, [nondet]) :- nPiezasDeCada(1, [1,2], X), X = [pieza(1,1), pieza(2,1)].
test(unSoloTamanio, [nondet]) :- nPiezasDeCada(4, [1], X), X = [pieza(1,4)].
test(vacio) :- nPiezasDeCada(_, [], []).
:- end_tests(ej2).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 
resumenPiezas([], []).
resumenPiezas([S|SS], [P|PS]) :- 
	agarrarMinimo([S|SS], M), 
	contarOcurrencias(M, [S|SS], O), 
	P = pieza(M, O), 
	eliminarAparicionesDe(M, [S|SS], ListaSinRepe), 
	resumenPiezas(ListaSinRepe, PS).

% contarOcurrencias(+Item, +Lista, -Ocurrencias)
contarOcurrencias(_, [], 0).
contarOcurrencias(Item, [H|T], O) :- contarOcurrencias(Item, T, Op), Item = H, O is Op+1.
contarOcurrencias(Item, [H|T], O) :- contarOcurrencias(Item, T, Op), Item \= H, O is Op.

% eliminarAparicionesDe(+Item, +Lista, -ListaSinRepetidosDeItem)
eliminarAparicionesDe(_, [], []).
eliminarAparicionesDe(H, [H|T], ListaSinRepe) :- eliminarAparicionesDe(H, T, ListaSinRepe).
eliminarAparicionesDe(I, [H|T], [H|ListaSinRepe]) :- I \= H, eliminarAparicionesDe(I, T, ListaSinRepe).

% agarrarMinimo(+Lista, ?Minimo)
agarrarMinimo([H], H).
agarrarMinimo([H|T], Min) :- agarrarMinimo(T, Min1), H >= Min1, Min is Min1.
agarrarMinimo([H|T], Min) :- agarrarMinimo(T, Min1), H < Min1, Min is H.


%%% TESTS EJ 3
:- begin_tests(ej3).
test(agarrarMinimoVacio, [nondet]) :- not(agarrarMinimo([], _)).
test(agarrarMinimoMinimoAlPrincipio, [nondet]) :- agarrarMinimo([1,2,3], 1).
test(agarrarMinimoMinimoAlFinal, [nondet]) :- agarrarMinimo([5,6,1], 1).
test(agarrarMinimoMinimoEnElMedio, [nondet]) :- agarrarMinimo([5,1,2,3], 1).
test(agarrarMinimoSinInstanciar, [nondet]) :- agarrarMinimo([5,1,2,3], X), X =:= 1.

test(elimApDeNingunaAparicion, [nondet]) :- eliminarAparicionesDe(3, [1,2], X), X = [1,2].
test(elimApDeUnaAparicion, [nondet]) :- eliminarAparicionesDe(3, [1,2,3], X), X = [1,2].
test(elimApDeMuchasApariciones, [nondet]) :- eliminarAparicionesDe(3, [3,1,2,3], X), X = [1,2].

test(contarOcNingunaAparicion, [nondet]) :- contarOcurrencias(3, [1,2], X), X = 0.
test(contarOcUnaAparicion, [nondet]) :- contarOcurrencias(3, [1,2,3], X), X = 1.
test(contarOcMuchasApariciones, [nondet]) :- contarOcurrencias(3, [3,1,2,3,3,4,3], X), X = 4.

test(resPiezasVacio, [nondet]) :- resumenPiezas([], X), X = [].
test(resPiezasPiezasEnOrdenSecOrden, [nondet]) :- resumenPiezas([1,1,1,2,2], X), X = [pieza(1,3), pieza(2,2)].
test(resPiezasPiezasEnOrdenSecDesorden, [nondet]) :- resumenPiezas([2,1,1,2,2], X), X = [pieza(1,2), pieza(2,3)].
:- end_tests(ej3).


% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

generarConRepetidos(0, _, []).

generarConRepetidos(Total, [pieza(P, _)|PS], Solucion) :- 
	P =< Total, X is Total-P,
	generarConRepetidos(X, [pieza(P, _)|PS], PreSol),
	append(A, B, PreSol), append(A, [P|B], Solucion).
	
generarConRepetidos(Total, [pieza(P, _)|PS], Solucion) :- 
	P < Total, % piezas en orden, no hace falta buscar en el resto de las piezas si la que tengo ya es mayor que el total
	generarConRepetidos(Total, PS, Solucion).
	
generar(Total, Piezas, Solucion) :- 
	setof(SolPerm, generarConRepetidos(Total, Piezas, SolPerm), Soluciones), 
	member(Solucion, Soluciones).

%%% TESTS EJ 4
:- begin_tests(ej4).
test(generarConTotal3, [nondet]) :- setof(SolPerm, generarConRepetidos(3, [pieza(1,1), pieza(2,1)], SolPerm), Soluciones), 
									member([1,1,1], Soluciones),
									member([2,1], Soluciones),
									member([1,2], Soluciones),
									length(Soluciones, 3).
test(generarConVacioYTotalFalla, [nondet]) :- not(setof(SolPerm, generarConRepetidos(3, [], SolPerm), _)).
test(generarConVacioYTotalCero, [nondet]) :- setof(SolPerm, generarConRepetidos(0, [], SolPerm), Soluciones), member([], Soluciones) , length(Soluciones, 1).
test(generarImposible, [nondet]) :- not(setof(SolPerm, generarConRepetidos(3, [pieza(90,1)], SolPerm), _)).
:- end_tests(ej4).

%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(PS, Sol) :- resumenPiezas(Sol, Piezas), cumpleLimiteSolPiezas(PS, Piezas).

cumpleLimiteSolPiezas(_, []).
cumpleLimiteSolPiezas([pieza(P, Cant)|PS], [pieza(P, Cantt)|PPS]) :-
	Cant >= Cantt,
	cumpleLimiteSolPiezas(PS, PPS).

cumpleLimiteSolPiezas([pieza(P, _)|PS], [pieza(PP, Cantt)|PPS]) :- % Veo si mas adelante esta la pieza
	P < PP,
	cumpleLimiteSolPiezas(PS, [pieza(PP, Cantt)|PPS]).

	
%%% TESTS EJ 5
:- begin_tests(ej5).
test(cumpleLimiteVerdadero, [nondet]) :- cumpleLimite([pieza(1,1), pieza(2,1)], [1,2]).
test(cumpleLimiteFalso, [nondet]) :- not(cumpleLimite([pieza(1,1), pieza(2,1)], [1,1,1])).
test(cumpleLimiteFalso2, [nondet]) :- not(cumpleLimite([], [1,1,1])).
:- end_tests(ej5).

%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.
construir1(Total, Piezas, Sol):- 
	generar(Total, Piezas, Solucion),
	cumpleLimite(Piezas, Solucion),
	Sol = Solucion.

%%% TESTS EJ 6
:- begin_tests(ej6).
test(construir1ConTotal3, [nondet]) :- setof(Sol, construir1(3, [pieza(1,1), pieza(2,1)], Sol), Soluciones), 
									member([2,1], Soluciones),
									member([1,2], Soluciones),
									length(Soluciones, 2).
test(construir1ConTotal7, [nondet]) :- setof(Sol, construir1(7, [pieza(1,1), pieza(2,2), pieza(3,2)], Sol), Soluciones), 
									member([3,3,1], Soluciones),
									member([3,1,3], Soluciones),
									member([1,3,3], Soluciones),
									member([2,3,2], Soluciones),
									member([2,2,3], Soluciones),
									member([3,2,2], Soluciones),
									length(Soluciones, 6).
:- end_tests(ej6).


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

:- dynamic subso/4.	
	
construir2(0, _, []).
	
construir2(Total, [pieza(_, 0)|PS], Solucion):- 
	construir2(Total, PS, Solucion).
	
construir2(Total, [pieza(P, Cant)|PS], Solucion) :- 
	Cant > 0, P =< Total, X is Total-P, NuevaCant is Cant-1,
	subso(X, [pieza(P, NuevaCant)|PS], P, Solucion).
	
construir2(Total, [pieza(P, Cant)|PS], Solucion) :- 
	Cant > 0, P < Total,
	subso(Total, PS, 0, Solucion).
	
construir2(Total, [pieza(P, Cant)|PS], Solucion) :- 
	Cant > 0, P =< Total, X is Total-P, NuevaCant is Cant-1,
	not(subso(X, [pieza(P, NuevaCant)|PS], P, PreSol)),
	construir2(X, [pieza(P, NuevaCant)|PS], PreSol),
	append(A, B, PreSol), append(A, [P|B], Solucion), 
	not(subso(X, [pieza(P, NuevaCant)|PS], P, Solucion)),
	asserta(subso(X, [pieza(P, NuevaCant)|PS], P, Solucion)).
	
construir2(Total, [pieza(P, Cant)|PS], Solucion) :- 
	Cant > 0, P < Total,
	not(subso(Total, PS, 0, Solucion)), % 0 es la no pieza
	construir2(Total, PS, Solucion),
	asserta(subso(Total, PS, 0, Solucion)).


	%%% TESTS EJ 7
:- begin_tests(ej7).
test(construir2ConTotal3, [nondet]) :- setof(Sol, construir2(3, [pieza(1,1), pieza(2,1)], Sol), Soluciones), 
									member([2,1], Soluciones),
									member([1,2], Soluciones),
									length(Soluciones, 2),
									retractall(subso(_,_,_,_)).
test(construir2ConTotal7, [nondet]) :- setof(Sol, construir2(7, [pieza(1,1), pieza(2,2), pieza(3,2)], Sol), Soluciones), 
									member([3,3,1], Soluciones),
									member([3,1,3], Soluciones),
									member([1,3,3], Soluciones),
									member([2,3,2], Soluciones),
									member([2,2,3], Soluciones),
									member([3,2,2], Soluciones),
									length(Soluciones, 6),
									retractall(subso(_,_,_,_)).
:- end_tests(ej7).

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%%% CORRIDA 1:
%%%%   1 ?- retractall(subso(_,_,_,_)).
%%%%   true.
%%%%   
%%%%   2 ?- time(todosConstruir2(10, [pieza(1,3), pieza(2,3), pieza(3,1), pieza(7,2)], Y, M)).
%%%%   % 3,393 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
%%%%   Y = [[1, 1, 1, 2, 2, 3], [1, 1, 1, 2, 3, 2], [1, 1, 1, 3, 2, 2], [1, 1, 1, 7], [1, 1, 2, 1|...], [1, 1, 2|...], [1, 1|...], [1|...], [...|...]|...],
%%%%   M = 92.
%%%%   
%%%%   3 ?- set_prolog_stack(global, limit(100 000 000 000)).
%%%%   true.
%%%%   
%%%%   4 ?- time(todosConstruir1(10, [pieza(1,3), pieza(2,3), pieza(3,1), pieza(7,2)], Y, M)).
%%%%   % 33,402,333 inferences, 6.016 CPU in 6.436 seconds (93% CPU, 5552596 Lips)
%%%%   Y = [[1, 1, 1, 2, 2, 3], [1, 1, 1, 2, 3, 2], [1, 1, 1, 3, 2, 2], [1, 1, 1, 7], [1, 1, 2, 1|...], [1, 1, 2|...], [1, 1|...], [1|...], [...|...]|...],
%%%%   M = 92.

%%%% CORRIDA 2:
%%%%   1 ?- retractall(subso(_,_,_,_)).
%%%%   true.
%%%%   
%%%%   2 ?- time(todosConstruir2(20, [pieza(2,3), pieza(3,5), pieza(5,3)], Y, M)).
%%%%   % 8,480 inferences, 0.000 CPU in 0.005 seconds (0% CPU, Infinite Lips)
%%%%   Y = [[2, 2, 2, 3, 3, 3, 5], [2, 2, 2, 3, 3, 5, 3], [2, 2, 2, 3, 5, 3|...], [2, 2, 2, 5, 3|...], [2, 2, 3, 2|...], [2, 2, 3|...], [2, 2|...], [2|...], [...|...]|...],
%%%%   M = 256.
%%%%   
%%%%   3 ?- set_prolog_stack(global, limit(100 000 000 000)).
%%%%   true.
%%%%   
%%%%   4 ?- time(todosConstruir1(20, [pieza(2,3), pieza(3,5), pieza(5,3)], Y, M)).
%%%%   % 33,791,328 inferences, 6.203 CPU in 6.589 seconds (94% CPU, 5447468 Lips)
%%%%   Y = [[2, 2, 2, 3, 3, 3, 5], [2, 2, 2, 3, 3, 5, 3], [2, 2, 2, 3, 5, 3|...], [2, 2, 2, 5, 3|...], [2, 2, 3, 2|...], [2, 2, 3|...], [2, 2|...], [2|...], [...|...]|...],
%%%%   M = 256.

%%%% CORRIDA 3:
%%%%   1 ?- retractall(subso(_,_,_,_)).
%%%%   true.
%%%%   
%%%%   2 ?- time(todosConstruir2(24, [pieza(1,1), pieza(2,3), pieza(3,5), pieza(5,3)], Y, M)).
%%%%   % 6,740,074 inferences, 0.609 CPU in 0.659 seconds (93% CPU, 11060634 Lips
%%%%   Y = [[1, 2, 2, 2, 3, 3, 3, 3|...], [1, 2, 2, 2, 3, 3, 3|...], [1, 2, 2, 2, 3, 3|...], [1, 2, 2, 2, 3|...], [1, 2, 2, 2|...], [1, 2, 2|...], [1, 2|...], [1|...], [...|...]|...],
%%%%   M = 5053.
%%%%   
%%%%   3 ?- set_prolog_stack(global, limit(100 000 000 000)).
%%%%   true.
%%%%   
%%%%   4 ?- time(todosConstruir1(24, [pieza(1,1), pieza(2,3), pieza(3,5), pieza(5,3)], Y, M)).
%%%%   % 2,510,891,160 inferences, 207.953 CPU in 230.321 seconds (90% CPU, 12074313 Lips)
%%%%   ---> NO ALCANZO EL STACK PARA ESTO INCLUSO AGRANDÁNDOLO a 100 000 000 000

%%%% CORRIDA 4:
%%%%   1 ?- retractall(subso(_,_,_,_)).
%%%%   true.
%%%%   
%%%%   2 ?- time(todosConstruir2(15, [pieza(1,3), pieza(2,3), pieza(3,1), pieza(7,2)], Y, M)).
%%%%   % 7,320 inferences, 0.016 CPU in 0.005 seconds (312% CPU, 468480 Lips)
%%%%   Y = [[1, 1, 1, 2, 3, 7], [1, 1, 1, 2, 7, 3], [1, 1, 1, 3, 2, 7], [1, 1, 1, 3, 7|...], [1, 1, 1, 7|...], [1, 1, 1|...], [1, 1|...], [1|...], [...|...]|...],
%%%%   M = 243.
%%%%   
%%%%   3 ?- set_prolog_stack(global, limit(100 000 000 000)).
%%%%   true.
%%%%   
%%%%   4 ?- time(todosConstruir1(500, [pieza(1,2), pieza(10,5), pieza(100,3), pieza(250,2)], Y, M)).
%%%%   % 2,762,433,143 inferences, 242.016 CPU in 268.941 seconds (90% CPU, 11414276 Lips
%%%%   ---> NO ALCANZÓ EL STACK PARA ESTO INCLUSO AGRANDÁNDOLO a 100 000 000 000

%%%% CORRIDA 5:
%%%%   1 ?- retractall(subso(_,_,_,_)).
%%%%   true.
%%%%   
%%%%   2 ?- time(todosConstruir2(500, [pieza(1,2), pieza(10,5), pieza(100,3), pieza(250,2)], Y, M)).
%%%%   % 15,174 inferences, 0.000 CPU in 0.005 seconds (0% CPU, Infinite Lips)
%%%%   Y = [[10, 10, 10, 10, 10, 100, 100, 250], [10, 10, 10, 10, 10, 100, 250|...], [10, 10, 10, 10, 10, 250|...], [10, 10, 10, 10, 100|...], [10, 10, 10, 10|...], [10, 10, 10|...], [10, 10|...], [10|...], [...|...]|...],
%%%%   M = 169.
%%%%   
%%%%   3 ?- set_prolog_stack(global, limit(100 000 000 000 000 000)). % aun más stack! (*100000 más que el anterior)
%%%%   true.
%%%%   
%%%%   4 ?- time(todosConstruir1(500, [pieza(1,2), pieza(10,5), pieza(100,3), pieza(250,2)], Y, M)).
%%%%   ---> NO ALCANZÓ EL STACK PARA ESTO INCLUSO AGRANDÁNDOLO a 100 000 000 000 000 000 (se murió la PC inclusive)



%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.
%% --> Consideramos que los tests no hacen falta corriendo los time() de arriba y además todas las cosas que utiliza todosConstruir1 ya están testeadas o son de las librerías de Prolog

todosConstruir1(Total, Piezas, Soluciones, N):- setof(Solucion, construir1(Total, Piezas, Solucion), Soluciones), length(Soluciones, N).


%%% Ejercicio 9
%% --> Consideramos que los tests no hacen falta corriendo los time() de arriba y además todas las cosas que utiliza todosConstruir2 ya están testeadas o son de las librerías de Prolog

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Total, Piezas, Soluciones, N):- setof(Solucion, construir2(Total, Piezas, Solucion), Soluciones), length(Soluciones, N).

% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

construirConPatron(Total, Piezas, Patron, Solucion):- construir1(Total, Piezas, Solucion), tienePatron(Patron, Solucion).

% tienePatron(+Patron, ?Lista)
tienePatron([], []).
tienePatron([P|PS], Lista) :- append([P|PS], [], Lista).
tienePatron([P|PS], Lista) :- append([P|PS], [H|T], Lista), tienePatron([P|PS], [H|T]).

%%% TESTS EJ 10
:- begin_tests(ej10).
test(noTienePatronListaVacia, [nondet]) :- not(tienePatron([], [1,1,1,1])).
test(tienePatronDeUnos, [nondet]) :- tienePatron([A, A], [1,1,1,1]), A = 1.
test(noTienePatronParListaImpar, [nondet]) :- not(tienePatron([B, B], [1,1,1])).
test(tienePatronRepetido, [nondet]) :- tienePatron([A, B], [2,1,2,1]), A = 2, B = 1.
test(noTienePatron, [nondet]) :- not(tienePatron([_, _], [2,1,4,5])).
test(tienePatronAlgunosFijos, [nondet]) :- tienePatron([A, 1, B, 2, C], [2,1,3,2,7]), A = 2, B = 3, C = 7.
test(tienePatronSinInstanciar, [nondet]) :- tienePatron([A, A], Lista), Lista = [A, A, A, A, A, A]. % solo una de todas las listas que genera
:- end_tests(ej10).