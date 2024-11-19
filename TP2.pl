%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 1

% proceso(+P)
proceso(escribir(_,_)).
proceso(computar).
proceso(leer(_)).
proceso(secuencia(P,Q)):- proceso(P), proceso(Q).
proceso(paralelo(P,Q)):- proceso(P), proceso(Q).


% Ejercicio 2

% buffersUsados(+P,-BS)
buffersUsados(P,BS) :- proceso(P), setof(L,identificarBuffer(P,L),BS). % esto hara el trabajo de eliminar los repetidos.

% este hara el trabajo de identificar que se uso 

% identificarBuffer(+P, -BS)
identificarBuffer(computar,_). 
identificarBuffer(escribir(B,_),B).
identificarBuffer(leer(B),B).

identificarBuffer(secuencia(P,_),BS):- identificarBuffer(P,BS).
identificarBuffer(secuencia(_,Q),BS):- identificarBuffer(Q,BS).
identificarBuffer(paralelo(P,_),BS):- identificarBuffer(P,BS).
identificarBuffer(paralelo(_,Q),BS):- identificarBuffer(Q,BS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)

intercalar([], YS, YS).
intercalar(XS, [], XS).
intercalar([X|XS], YS, [X|ZS]) :- YS \= [], intercalar(XS, YS, ZS).
intercalar(XS, [Y|YS], [Y|ZS]) :- XS \= [], intercalar(XS, YS, ZS).

% 1 2 3 4
% 1 3 2 4
% 1 3 4 2
% 3 4 1 2
% 3 1 4 2
% 3 1 2 4


%% Ejercicio 4
%% serializar(+P,?XS)

% quiero dividir en casos, cuando sea un paralelo tengo que usar la función de intercalar, 
% si es secuencia devuelvo la lista como en buffersUsados

serializar(escribir(B,E), [escribir(B,E)]).
serializar(leer(B), [leer(B)]).
serializar(computar, [computar]).


serializar(secuencia(P,Q),XS):- serializar(P,PS), serializar(Q,QS), append(PS,QS,XS).
serializar(paralelo(P,Q),XS):- serializar(P,PS), serializar(Q,QS), intercalar(PS,QS,XS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)

% si es proceso lo serializo para que sea mas facil
contenidoBuffer(B,P,L) :- proceso(P), serializar(P, XS), contenidoBuffer(B,XS,L).

contenidoBuffer(B,XS,L) :- is_list(XS), listarBuffers(B, XS, [], L).

%listarBuffers(+B,+P,+L,-Out)
listarBuffers(_, [], L,  L).

listarBuffers(B, [computar|XS], L, Out) :- listarBuffers(B, XS, L, Out).

listarBuffers(B, [escribir(B,E)|XS], L, Out) :- append(L, [E], L2), listarBuffers(B, XS, L2, Out).
listarBuffers(B, [escribir(B2,_)|XS], L, Out) :- B \= B2, listarBuffers(B, XS, L, Out).


listarBuffers(B, [leer(B)|XS], [_|L], Out) :- listarBuffers(B, XS, L, Out).
listarBuffers(B, [leer(B2)|XS], L, Out) :- B \= B2, listarBuffers(B, XS, L, Out).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)

contenidoLeido([],[]).
contenidoLeido(P,Leidos) :- proceso(P), serializar(P, XS), contenidoLeido(XS,Leidos).
contenidoLeido([escribir(X,Y)|T],[Contenido|L1]) :-  select(leer(X),T,Res), Contenido = Y, 
                                                     contenidoLeido(Res,L1).
contenidoLeido([escribir(X,_)|T],Res) :- \+(select(leer(X),T,Res)), contenidoLeido(T,Res).
contenidoLeido([computar|T],Res) :- contenidoLeido(T,Res).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)


esSeguro(P):- proceso(P), buffersUsados(P, BS),
              forall( member(B, BS), (contenidoBuffer(B, P, L), is_list(L), paralelosUnicos(P))).


% paralelosUnicos(+P)
% retorna true <=> todos los procesos paralelos de P utilizan buffers distintos.
paralelosUnicos(escribir(_,_)).
paralelosUnicos(computar).
paralelosUnicos(leer(_)).

paralelosUnicos(secuencia(P,Q)):- paralelosUnicos(P), paralelosUnicos(Q).
paralelosUnicos(paralelo(P,Q)):- buffersUsados(P, BP),  buffersUsados(Q, BQ), intersection(BP,BQ,[]),  
              paralelosUnicos(P), paralelosUnicos(Q).

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
% ejecucionSegura(_,_,_).

% ejecucionSegura(XS, BS, CS) :- member(B, BS), member(C, CS), 



ejecucionesSeguras(XS,Buffers,Contenidos):- desde(1,Len), generador(Len,Buff,Valor,XS),
                                            length(XS,Len),        
                                            noLeeVacio(XS).

generador(0,Buffers,Contenidos,[]).
generador(Longitud,Buffers,Contenidos,[escribir(Buff,Valor)|L1]):- Longitud \= 0 , member(Buffers,Buff), member(Contenidos,Valor),
                                                          N is Longitud -1
                                                          generador(N,Buffers,Contenidos,L1). 
generador(Longitud,Buffers,Contenidos,[leer(Buff)|L1]):- Longitud \= 0, member(Buffers,Buff), generador(Buffers,Contenidos,L1)
generador(Longitud,Buffers,Contenidos,[computar|L1]):-Longitud \= 0, generador(Buffers,Contenidos,L1).
generador(Longitud,Buffers,Contenidos,Res).

desde(X, X). 
desde(X, Y) :- N is X+1, desde(N, Y).








  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



% %%%%%%%%%%%
% %% TESTS %%
% %%%%%%%%%%%

% % Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

% cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
% testBasico(1) :- proceso(computar).
% testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
% testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% % Agregar más tests

% cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% % Agregar más tests

% cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% % Agregar más tests

% cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% % Agregar más tests


% tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
% tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
% tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
% tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

% tests(todos) :-
%   tests(basico),
%   tests(procesos),
%   tests(buffers),
%   tests(seguros).

% tests :- tests(todos).