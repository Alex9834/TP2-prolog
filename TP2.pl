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
              forall( member(B, BS), (noLeeVacio(P, B), paralelosUnicos(P))).

% noLeeVacio(+ProcesoOLista)
noLeeVacio(P, B) :- proceso(P), contenidoBuffer(B, P, L), is_list(L).
noLeeVacio(XS, B) :- is_list(XS), contenidoBuffer(B, XS, L), is_list(L).


% paralelosUnicos(+P)
% retorna true <=> todos los procesos paralelos de P utilizan buffers distintos.
paralelosUnicos(escribir(_,_)).
paralelosUnicos(computar).
paralelosUnicos(leer(_)).

paralelosUnicos(secuencia(P,Q)):- paralelosUnicos(P), paralelosUnicos(Q).
paralelosUnicos(paralelo(P,Q)):- buffersUsados(P, BP),  buffersUsados(Q, BQ), intersection(BP,BQ,[]),  
              paralelosUnicos(P), paralelosUnicos(Q).

%% Ejercicio 8
%% ejecucionSegura(?XS,+BS,+CS)
ejecucionSegura(XS, BS, CS):- nonvar(XS), length(XS, Len), generador(Len, BS, CS, XS),   
                              forall( member(B, BS), noLeeVacio(XS, B)), !.


ejecucionSegura(XS, BS, CS):- var(XS), desde(0, Len), generador(Len, BS, CS, XS),
                              length(XS, Len),        
                              forall( member(B, BS), noLeeVacio(XS, B)).

generador(0, _, _, []).
generador(Len, BS, CS, [computar|L1]):- Len \= 0, 
                                        N is Len - 1,
                                        generador(N, BS, CS, L1).

generador(Len, BS, CS, [escribir(B,C)|L1]):-  Len \= 0, member(B, BS), member(C, CS),
                                              N is Len - 1,
                                              generador(N, BS,CS,L1). 
        
generador(Len, BS, CS, [leer(B)|L1]):-  Len \= 0, member(B, BS),  
                                        N is Len - 1,
                                        generador(N, BS, CS, L1).




desde(X, X). 
desde(X, Y) :- N is X+1, desde(N, Y).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.

  % XS es reversible siempre y cuando agreguemos un caso aparte para cuando está instanciada, haciendo que termine la ejecución 
  % con el "!" apenas encuentra una lista que unifique, además hay que eliminar el predicado Desde(1, Len), ya que solo nos interesa
  % generar listas de la longitud de XS. Si no agregamos el "!", el generador continuará generando listas e intentando unificar con XS
  % infinitamente.

%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(4).

testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1, pepe),escribir(2, pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
testBasico(4) :- buffersUsados(paralelo(escribir(3, hola), secuencia(leer(1),escribir(2, hola))), [1,2,3]).

cantidadTestsProcesos(4).

testProcesos(1) :- intercalar([1,2,3],[4,5,6],[4,1,5,2,6,3]).
testProcesos(2) :- not(intercalar([1,2,3],[4,5,6],[4,2,5,1,6,3])).
testProcesos(3) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),[leer(1),leer(3),leer(2),leer(4)]).
testProcesos(4) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),[leer(3),leer(1),leer(4),leer(2)]).

cantidadTestsBuffers(8).

testBuffers(1) :- contenidoBuffer(1,[escribir(1,pa),escribir(2,ma),escribir(1,hola),computar,escribir(1,mundo),leer(1)],[hola, mundo]).
testBuffers(2) :- contenidoBuffer(2,[escribir(1,pp),escribir(2,ala),escribir(1,ola),computar,escribir(1,mundo),leer(1)],[ala]).
testBuffers(3) :- contenidoBuffer(2,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))),[sol]).
testBuffers(4) :- contenidoBuffer(1,paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1))),[]).
testBuffers(5) :- contenidoBuffer(1,paralelo(leer(1),escribir(1,agua)),[]).
testBuffers(6) :- contenidoLeido(paralelo(secuencia(escribir(2,sol),leer(2)),secuencia(escribir(1,agua),leer(1))),[agua, sol]).
testBuffers(7) :- not(contenidoLeido([escribir(1, agua), escribir(2, sol), leer(1), leer(1)],_)).
testBuffers(8) :- contenidoLeido(paralelo(secuencia(escribir(2,sol),secuencia(leer(2),escribir(2,agua))),secuencia(leer(2),computar)),[sol, agua]).

cantidadTestsSeguros(8).

testSeguros(1) :- not(esSeguro(secuencia(leer(1),escribir(1,agua)))).
testSeguros(2) :- not(esSeguro(paralelo(escribir(1,sol),secuencia(escribir(1,agua),leer(1))))).
testSeguros(3) :- esSeguro(paralelo(escribir(2,sol),secuencia(escribir(1,agua),leer(1)))).
testSeguros(4) :- ejecucionSegura([],[1,2],[a,b]).
testSeguros(5) :- ejecucionSegura([computar, escribir(1, a)],[1,2],[a,b]).
testSeguros(6) :- ejecucionSegura([escribir(1, a), leer(1)] ,[1,2],[a,b]).
testSeguros(7) :- ejecucionSegura([escribir(1, a), escribir(2, a)] ,[1,2],[a,b]).
testSeguros(8) :- ejecucionSegura([escribir(1, b), escribir(2, a)] ,[1,2],[a,b]).


tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).