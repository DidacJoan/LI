% Exercici 2
prod([],1).
prod([X|L],P):- prod(L,N), P is N*X.

% Exercici 3
pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,N), AUX is X*Y, P is AUX + N.

% Exercici 4
inter([],_,[]).
inter([X|L1],L2,[X|L3]):- member(X,L2),inter(L1,L2,L3).
inter([_|L1],L2,L3):- inter(L1,L2,L3).

union([],L,L).
union([X|L1],L2,L2):- member(X,L2),union(L1,L2,L2).
union([X|L1],L2,[X|L2]):- union(L1,L2,L2).

% Exercici 5
concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

ultimo(L,X):- concat(_,[X],L).

inverso([],[]).
inverso([X|L1],R):- concat(L2,[X],R), inverso(L1,L2). 

% Exercici 6
fib(1,1).
fib(2,1).
fib(N,F):- N1 is N-1,
	   N2 is N-2,
           fib(N1,F1), fib(N2,F2),!, 
	   F is F1+F2.

% Exercici 7
dados(0,0,[]).
dados(P,N,[X|L]):- N>0,member(X,[1,2,3,4,5,6]), N1 is N-1, P1 is P-X, dados(P1,N1,L).

% Exercici 8
erase(_,[],[]).
erase(X,[X|L],R):- erase(X,L,R),!.  
erase(X,[Y|L],[Y|R]):- erase(X,L,R).

aux(X,L,L2):- member(X,L), erase(X,L,L2).

sum(0,[]).
sum(R,[X|L]):- R1 is R-X, sum(R1,L),!.

suma_demas(L):- aux(X,L,R), sum(X,R),!.

% Exercici 9
suma_ants(L):- concat(L1,[X|_],L), sum(X,L1),!.

% Exercici 10 FALLA
apar(_,[],0).
apar(X,[X|L],R):- R is R1+1, apar(X,L,R1).
apar(X,[_|L],R):- apar(X,L,R).

card(L,R):- member(X,L), apar(X,L,R).

% Exercici 11
compare(_,[]).
compare(X,[N|_]):- N >= X.

orden([]).
orden([X|L]):- compare(X,L), orden(L),!.

% Exercici 12

% Exercici 13

























