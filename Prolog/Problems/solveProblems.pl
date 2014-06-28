
% ===========================================================PROBLEMA A===========================================================

% Definicion de valores para los apartados del primer problema(modificables):

cubo1(5).
cubo2(8).

misioneros(3).
canibales(3).
capacidadBarca(2).

personas([1,2,5,8]).

% ---Funciones auxiliares:

nat(0).
nat(N):- nat(M), N is M+1.

check(C,M):- C=<M.
check(_,0).

add(ELEM, L, [ELEM | L]).

erase(X,[X|T],T).
erase(X,[H|T],[H|T1]):- erase(X,T,T1).

max(X,Y,Y):- Y>=X.
max(X,Y,X):- X>Y.


camino( E, E, C, C, _).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal, Problema):-
	unPaso( EstadoActual, EstSiguiente, Problema), % implementar
	\+member(EstSiguiente,CaminoHastaAhora), % añadir a EstSiguiente CaminoHastaAhora
camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal, Problema). % there are no repeated States

unPaso(EstadoActual,EstadoSiguiente,0):- unPasoHacerAguas(EstadoActual,EstadoSiguiente).
unPaso(EstadoActual,EstadoSiguiente,1):- unPasoMisioneros(EstadoActual,EstadoSiguiente).
unPaso(EstadoActual,EstadoSiguiente,2):- unPasoPuente(EstadoActual,EstadoSiguiente).

% ----------------------UN PASO HACER AGUAS----------------------

% llenar cubo
unPasoHacerAguas([C1,C2], [N, C2]):- cubo1(N), C1<N.
unPasoHacerAguas([C1,C2], [C1, N]):- cubo2(N), C2<N.

% vaciar cubo
unPasoHacerAguas([C1,C2], [0, C2]):- C1>0.
unPasoHacerAguas([C1,C2], [C1, 0]):- C2>0.

% verter todo el contenido de un cubo a otro
unPasoHacerAguas([C1,C2], [0, FIN]):- cubo2(N), C1>0, FIN is (C1+C2), FIN=<N.
unPasoHacerAguas([C1,C2], [FIN, 0]):- cubo1(N), C2>0, FIN is (C1+C2), FIN=<N.

% verter parte del contenido de un cubo a otro hasta llenarlo
unPasoHacerAguas([I1,I2], [FIN, N]):- cubo2(N), I1>0, AUX is (I1+I2), AUX>=N, FIN is (I1-(N-I2)). 
unPasoHacerAguas([I1,I2], [N, FIN]):- cubo1(N), I2>0, AUX is (I1+I2), AUX>=N, FIN is (I2-(N-I1)).


% ----------------------UN PASO MISIONEROS----------------------

% Selecciona los parametros de pasarGeneral en función de a que orilla se encuentra la barca
unPasoMisioneros([MI,CI,0,MD,CD], [MIF,CIF,1,MDF,CDF]):- pasarGeneral(MI,CI,MD,CD,MIF,CIF,MDF,CDF).
unPasoMisioneros([MI,CI,1,MD,CD], [MIF,CIF,0,MDF,CDF]):- pasarGeneral(MD,CD,MI,CI,MDF,CDF,MIF,CIF).

% Pasar MNUM misioneros, CNUM canibales de una orilla a otra 
pasarGeneral(M1,C1,M2,C2,M1F,C1F,M2F,C2F):- % de misioneros i canibales se pueden pasar como maximo la capacidad de la barca
					    capacidadBarca(LIMIT),between(0,LIMIT,MNUM), between(0,LIMIT,CNUM),
					    CAP is MNUM+CNUM, CAP =< LIMIT, CAP > 0, 
					    M1 >= MNUM, C1 >= CNUM, 
					    M1F is (M1-MNUM), C1F is (C1-CNUM), 
					    M2F is (M2+MNUM), C2F is (C2+CNUM),
					    check(C1F,M1F), check(C2F,M2F).


% ----------------------UN PASO PUENTE----------------------


unPasoPuente( [LI, 0, LD, Coste], [LIF, 1, LDF, CosteMaxMov]):-
	member(T1, LI),
	member(T2, LI),
	max(T1,T2,Tiempo),
	CosteMaxMov is Coste + Tiempo,
	erase(T1, LI, LIAUX),
	erase(T2, LIAUX, LIF),
	add(T1, LD, LDAUX),
	add(T2, LDAUX, LDF).

unPasoPuente( [LI, 1, LD, Coste], [LIF, 0, LDF, CosteMaxMov]):-
	member(T1, LD),
	CosteMaxMov is Coste + T1,
	add(T1, LI, LIF),
	erase(T1, LD, LDF).


% ----------------------SOLUCION HACER AGUAS----------------------


solucionA1:-
nat(N), % Buscamos solución de "coste" 0; si no, de 1, etc.
camino([0,0],[0,4],[[0,0]],C,0), % En "hacer aguas": -un estado es [cubo5,cubo8], i
length(C,N), % -el coste es la longitud de C.
write('Camino: '),write(C),nl,
write('Coste: '), write(N).


% ----------------------SOLUCION MISIONEROS----------------------


solucionA2:-
nat(N), % Buscamos solución de "coste" 0; si no, de 1, etc.
% En "Misioneros": -un estado es [MisionerosIzq, CanibalesIzq,PosicionBarca,MisionerosDer,CanibalesDer]
camino([3,3,0,0,0],[0,0,1,3,3],[[3,3,0,0,0]],C,1),
length(C,N), % -el coste es la longitud de C.
write('Camino: '),write(C),nl,
write('Coste: '), write(N).


% ----------------------SOLUCION PUENTE----------------------


solucionA3:- nat(N), % Buscamos solución de "coste" 0; si no, de 1, etc.
% En "Puente": -un estado es [ListaPersonasIzq,PosicionLinterna,ListaPersonasDer,TiempoTranscurrido]
camino([[1,2,5,8],0,[],0], [[],1,[1,2,5,8],N], [[[1,2,5,8],0,[],0]], C,2),
write('Pasos: '),nl,
write(C), nl,
write('Coste: '), write(N).



% ===========================================================PROBLEMA B===========================================================



datosEjemplo( [[1,2,6],[1,6,7],[2,3,8],[6,7,9],[6,8,9],[1,2,4],[3,5,6],[3,5,7],
[5,6,8],[1,6,8],[4,7,9],[4,6,9],[1,4,6],[3,6,9],[2,3,5],[1,4,5],
[1,6,7],[6,7,8],[1,2,4],[1,5,7],[2,5,6],[2,3,5],[5,7,9],[1,6,8]] ).

% Prova les permutacions de una petició amb els 3 slots horaris que hi ha
unPasoB(EstadoActual,EstadoSiguiente,PET):- permutation(PET,PERM), addToSlots(PERM,EstadoActual,EstadoSiguiente).

% Si la xerrada es al slot no s'afegeix, en cas contrari s'afegeix al slot
addToSlots([X1,X2,X3],[AI,BI,CI],[AF,BF,CF]):- addB(X1,AI,AF), addB(X2,BI,BF), addB(X3,CI,CF).

addB(X,L,L):- member(X,L).
addB(X,L,[X|L]):- \+member(X,L).

caminoB( [A,B,C],[A,B,C],[],N ):-length(A,LEN1), length(B,LEN2), length(C,LEN3), 
				SUM is (LEN1+LEN2+LEN3), N = SUM.

caminoB( [A1,B1,C1], EstadoFinal, [PET|DATA], N):- length(A1,LEN1), length(B1,LEN2), length(C1,LEN3),
					SUM is (LEN1+LEN2+LEN3), N >= SUM,
					unPasoB([A1,B1,C1],[A2,B2,C2],PET),
					caminoB( [A2,B2,C2], EstadoFinal, DATA, N).

solucionB:- nat(N), datosEjemplo(DATA),
	% un estado es [[Charlas en A],[Charlas en B],[Charlas en C]]
	caminoB([[],[],[]],[A,B,C],DATA,N),
	write('Slot A: '), write(A), nl,
	write('Slot B: '), write(B), nl,
	write('Slot C: '), write(C), nl,
	write('Coste: '), write(N).

























