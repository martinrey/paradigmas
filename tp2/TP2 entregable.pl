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


%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.

nPiezasDeCada(_, [], _).
nPiezasDeCada(Cant, [T|TS], [P|PS]) :- P = pieza(T, Cant), nPiezasDeCada(Cant, TS, PS).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 
resumenPiezas([], []).
resumenPiezas([S|SS], [P|PS]) :- 
	agarrarMinimo([S|SS], M), 
	contarOcurrencias(M, [S|SS], O), 
	P = pieza(M, O), 
	sacarRepetidos(M, [S|SS], ListaSinRepe), 
	resumenPiezas(ListaSinRepe, PS).

% contarOcurrencias(+Item, +Lista, -Ocurrencias)
contarOcurrencias(_, [], 0).
contarOcurrencias(Item, [H|T], O) :- contarOcurrencias(Item, T, Op), Item = H, O is Op+1.
contarOcurrencias(Item, [H|T], O) :- contarOcurrencias(Item, T, Op), Item \= H, O is Op.

% sacarRepetidos(+Item, +Lista, -ListaSinRepetidosDeItem)
sacarRepetidos(_, [], []).
sacarRepetidos(H, [H|T], ListaSinRepe) :- sacarRepetidos(H, T, ListaSinRepe).
sacarRepetidos(I, [H|T], [H|ListaSinRepe]) :- I \= H, sacarRepetidos(I, T, ListaSinRepe).

% agarrarMinimo(+Lista, ?Minimo)
agarrarMinimo([H], H).
agarrarMinimo([H|T], Min) :- agarrarMinimo(T, Min1), H >= Min1, Min is Min1.
agarrarMinimo([H|T], Min) :- agarrarMinimo(T, Min1), H < Min1, Min is H.


% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

generar(Total, Piezas, Solucion) :- 
	generarSolSinPerm(Total, Piezas, SolSinPerm), 
	permutaciones(SolSinPerm, Permutaciones), 
	member(Solucion, Permutaciones).

generarSolSinPerm(0, _, []).
generarSolSinPerm(N, [pieza(P, Cant)|PS], [P|PSS]) :-  % PONGO LA PIEZA
	P =< N, X is N-P,
	generarSolSinPerm(X, [pieza(P, Cant)|PS], PSS).
	
generarSolSinPerm(N, [pieza(P, _)|PS], PSS) :-  % NO PONGO LA PIEZA
	P =< N,
	generarSolSinPerm(N, PS, PSS).
	
% ESTA LINEA PODRIA ESTAR PERO SE COMENTA YA QUE LAS PIEZAS ESTAN EN ORDEN ASCENDENTE
% generarSolSinPerm(N, [pieza(P, _)|PS], PSS) :- N \= 0, P > N, generarSolSinPerm(N, PS, PSS).

permutaciones(Lista, Perms) :- setof(X, permutar(Lista, X), Perms).

%permutar(+Lista, -Perm)
permutar([],[]).
permutar(List,[H|Perm]):- 
	delete(H,List,Rest), 
	permutar(Rest,Perm).

%delete(+X, +T, -TsinX)
delete(X,[X|T],T).
delete(X,[H|T],[H|NT]):- delete(X,T,NT).



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


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.
construir1(Total, Piezas, Sol):- 
	generar(Total, Piezas, Solucion),
	cumpleLimite(Piezas, Solucion),
	Sol = Solucion.


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

:- dynamic subsolucion/3.

generarSolSinPermDin(0, _, []).

generarSolSinPermDin(N, [pieza(P, Cant)|PS], [P|PSS]) :-  % Si pongo la pieza, está precalculado?
	P =< N, X is N-P,
	subsolucion(X, [pieza(P, Cant)|PS], Subsol),
	PSS = Subsol.
	
generarSolSinPermDin(N, [pieza(P, _)|PS], PSS) :-  % Si no pongo la pieza, está precalculado?
	P =< N,
	subsolucion(N, PS, Subsol),
	PSS = Subsol.
	
generarSolSinPermDin(N, [pieza(P, Cant)|PS], [P|PSS]) :- % Pongo la pieza
	P =< N, X is N-P,
	not(subsolucion(X, [pieza(P, Cant)|PS], PSS)),
	generarSolSinPermDin(X, [pieza(P, Cant)|PS], PSS),
	asserta(subsolucion(X, [pieza(P, Cant)|PS], PSS)).
	
generarSolSinPermDin(N, [pieza(P, _)|PS], PSS) :- % NO PONGO LA PIEZA
	P =< N,
	not(subsolucion(N, PS, PSS)),
	generarSolSinPermDin(N, PS, PSS),
	asserta(subsolucion(N, PS, PSS)).

generar2(Total, Piezas, Solucion) :- 
	generarSolSinPermDin(Total, Piezas, SolSinPerm), 
	permutaciones(SolSinPerm, Permutaciones), 
	member(Solucion, Permutaciones).	

construir2(Total, Piezas, Sol):- 
	generar2(Total, Piezas, Solucion),
	cumpleLimite(Piezas, Solucion),
	Sol = Solucion.

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(Total, Piezas, Soluciones, N):- setof(Solucion, construir1(Total, Piezas, Solucion), Soluciones), length(Soluciones, N).


%%% Ejercicio 9

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
tienePatron(Patron, Lista) :- tienePatronBis(Patron, Patron, Lista).

tienePatronBis([], _, []).
tienePatronBis([], PS, L) :- tienePatronBis(PS, PS, L).
tienePatronBis([P|PS], PSS, [H|T]) :- nonvar(P), P =:= H, tienePatronBis(PS, PSS, T).
tienePatronBis([P|PS], PSS, [H|T]) :- var(P), P = H, tienePatronBis(PS, PSS, T).
