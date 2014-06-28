:- use_module(library(clpfd)).

% --------------------------------------SUDOKU---------------------------------------

exemple :- sudoku([_,_,_,_,_,_,8,_,_,
		   1,7,_,_,_,_,2,3,_,
		   5,2,3,_,1,_,_,9,6,
		   8,_,6,5,_,_,_,_,1,
		   2,_,7,9,3,_,5,6,_,
		   _,3,5,6,8,_,7,2,_,
                   7,_,_,_,_,_,_,_,3,
                   _,6,4,_,_,_,_,_,2,
                   _,5,1,_,9,2,6,7,_]).

sudoku(L) :-
	L = [X11, X12, X13, X14, X15, X16, X17, X18, X19,
	     X21, X22, X23, X24, X25, X26, X27, X28, X29,
	     X31, X32, X33, X34, X35, X36, X37, X38, X39,
	     X41, X42, X43, X44, X45, X46, X47, X48, X49,
	     X51, X52, X53, X54, X55, X56, X57, X58, X59,
	     X61, X62, X63, X64, X65, X66, X67, X68, X69,
	     X71, X72, X73, X74, X75, X76, X77, X78, X79,
	     X81, X82, X83, X84, X85, X86, X87, X88, X89,
	     X91, X92, X93, X94, X95, X96, X97, X98, X99],

	L ins 1..9,
        
        % Files.
	all_different([X11,X12,X13,X14,X15,X16,X17,X18,X19]),
	all_different([X21,X22,X23,X24,X25,X26,X27,X28,X29]),
	all_different([X31,X32,X33,X34,X35,X36,X37,X38,X39]),
	all_different([X41,X42,X43,X44,X45,X46,X47,X48,X49]),
	all_different([X51,X52,X53,X54,X55,X56,X57,X58,X59]),
	all_different([X61,X62,X63,X64,X65,X66,X67,X68,X69]),
	all_different([X71,X72,X73,X74,X75,X76,X77,X78,X79]),
	all_different([X81,X82,X83,X84,X85,X86,X87,X88,X89]),
	all_different([X91,X92,X93,X94,X95,X96,X97,X98,X99]),

        % Columnes.
	all_different([X11,X21,X31,X41,X51,X61,X71,X81,X91]),
	all_different([X12,X22,X32,X42,X52,X62,X72,X82,X92]),
	all_different([X13,X23,X33,X43,X53,X63,X73,X83,X93]),
	all_different([X14,X24,X34,X44,X54,X64,X74,X84,X94]),
	all_different([X15,X25,X35,X45,X55,X65,X75,X85,X95]),
	all_different([X16,X26,X36,X46,X56,X66,X76,X86,X96]),
	all_different([X17,X27,X37,X47,X57,X67,X77,X87,X97]),
	all_different([X18,X28,X38,X48,X58,X68,X78,X88,X98]),
	all_different([X19,X29,X39,X49,X59,X69,X79,X89,X99]),

        % Quadrats 3x3.
	all_different([X11,X21,X31,X12,X22,X32,X13,X23,X33]),
	all_different([X14,X24,X34,X15,X25,X35,X16,X26,X36]),
	all_different([X17,X27,X37,X18,X28,X38,X19,X29,X39]),
	all_different([X41,X51,X61,X42,X52,X62,X43,X53,X63]),
	all_different([X44,X54,X64,X45,X55,X65,X46,X56,X66]),
	all_different([X47,X57,X67,X48,X58,X68,X49,X59,X69]),
	all_different([X71,X81,X91,X72,X82,X92,X73,X83,X93]),
	all_different([X74,X84,X94,X75,X85,X95,X76,X86,X96]),
	all_different([X77,X87,X97,X78,X88,X98,X79,X89,X99]),

        % Es generen els candidats a solucions.
	label(L),

	pinta(L).

pinta(L):- pinta_aux(L, 9).

pinta_aux([], _).
pinta_aux(L, 0):- L\=[],  nl,  pinta_aux(L, 9).
pinta_aux([X|L], N):-
	N>0,  write(X),  write(' '),
	N1 is N-1,  pinta_aux(L, N1).


% --------------------------------------CHINA----------------------------------------

china:- L = [A,B,C,D,E,F],
        L ins 0..80,
	1*A + 2*B + 3*C + 5*D + 6*E + 7*F #=< 80,
        labeling( [ max( 1*A + 4*B + 7*C +11*D + 14*E + 15*F ) ], L), 
	write(L), nl, !.

% --------------------------------------MONEY----------------------------------------

sendMoreMoney:-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9, 
	all_different(Vars),
	M #\= 0, S #\= 0,
	S*1000 + E*100 + N*10 + D + 
	M*1000 + O*100 + R*10 + E #= 
	M*10000 + O*1000 + N*100 + E*10 + Y,
        label(Vars),
	write('[S][E][N][D] [M][O][R][E] [M][O][N][E][Y]'), nl,
	writeValue(S), writeValue(E), writeValue(N), writeValue(D), write(' '), 
	writeValue(M), writeValue(O), writeValue(R), writeValue(E), write(' '), 
	writeValue(M), writeValue(O), writeValue(N), writeValue(E), writeValue(Y),
	nl.

writeValue(L):- write('['), write(L),write(']').

% --------------------------------------TASKS----------------------------------------

obtainTime([V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14], SUM):-  
	SUM is 8*V1 + 6*V2 + 7*V3 + 5*V4 + 2*V5 + 3*V6 + 8*V7 + 6*V8 + 2*V9 + 6*V10 + 1*V11 + 2*V12 + 6*V13 + 4*V14.

tareas:-
	L =    [A1,  B1,  C1,  D1,  E1,
	 	A2,  B2,  C2,  D2,  E2,
	 	A3,  B3,  C3,  D3,  E3,
	 	A4,  B4,  C4,  D4,  E4,
	 	A5,  B5,  C5,  D5,  E5,
	 	A6,  B6,  C6,  D6,  E6, 
	 	A7,  B7,  C7,  D7,  E7, 
	 	A8,  B8,  C8,  D8,  E8, 
	 	A9,  B9,  C9,  D9,  E9,
	 	A10, B10, C10, D10, E10,
	 	A11, B11, C11, D11, E11,
	 	A12, B12, C12, D12, E12,
	 	A13, B13, C13, D13, E13,
	 	A14, B14, C14, D14, E14],
		 
	% Cada elemento de L es una asignacion, si esta a 1 quiere decir que esa asignación se hace
	L ins 0..1,

	A1  + B1  + C1  +  D1 +  E1 #= 1,
	A2  + B2  + C2  +  D2 +  E2 #= 1,
	A3  + B3  + C3  +  D3 +  E3 #= 1,
	A4  + B4  + C4  +  D4 +  E4 #= 1,
	A5  + B5  + C5  +  D5 +  E5 #= 1,
	A6  + B6  + C6  +  D6 +  E6 #= 1,
	A7  + B7  + C7  +  D7 +  E7 #= 1,
	A8  + B8  + C8  +  D8 +  E8 #= 1,
	A9  + B9  + C9  +  D9 +  E9 #= 1,
	A10 + B10 + C10 + D10 + E10 #= 1,
	A11 + B11 + C11 + D11 + E11 #= 1,
	A12 + B12 + C12 + D12 + E12 #= 1,
	A13 + B13 + C13 + D13 + E13 #= 1,
        A14 + B14 + C14 + D14 + E14 #= 1,

	8*A1 + 6*A2 + 7*A3 + 5*A4 + 2*A5 + 3*A6 + 8*A7 + 6*A8 + 2*A9 + 6*A10 + 1*A11 + 2*A12 + 6*A13 + 4*A14 #< K,
	8*B1 + 6*B2 + 7*B3 + 5*B4 + 2*B5 + 3*B6 + 8*B7 + 6*B8 + 2*B9 + 6*B10 + 1*B11 + 2*B12 + 6*B13 + 4*B14 #< K,
	8*C1 + 6*C2 + 7*C3 + 5*C4 + 2*C5 + 3*C6 + 8*C7 + 6*C8 + 2*C9 + 6*C10 + 1*C11 + 2*C12 + 6*C13 + 4*C14 #< K,
	8*D1 + 6*D2 + 7*D3 + 5*D4 + 2*D5 + 3*D6 + 8*D7 + 6*D8 + 2*D9 + 6*D10 + 1*D11 + 2*D12 + 6*D13 + 4*D14 #< K,
	8*E1 + 6*E2 + 7*E3 + 5*E4 + 2*E5 + 3*E6 + 8*E7 + 6*E8 + 2*E9 + 6*E10 + 1*E11 + 2*E12 + 6*E13 + 4*E14 #< K,

	% K es el es el valor a minimizar, el tiempo que puede tardar en 
	% hacer las tareas. El maximo(todas en un PC) seria 66
	K in 0..66,
        
	labeling([ min(K) ],[K|L]),

        PC1 = [ A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14 ],
        PC2 = [ B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14 ],
        PC3 = [ C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14 ],
        PC4 = [ D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14 ],
        PC5 = [ E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14 ],

	obtainTime(PC1,T1), obtainTime(PC2,T2), obtainTime(PC3,T3), obtainTime(PC4,T4), obtainTime(PC5,T5),
	T is max(T1,max(T2,max(T3,max(T4,T5)))),

        write('Tareas PC1 : '),write(PC1),nl,
        write('Tareas PC2 : '),write(PC2),nl,
        write('Tareas PC3 : '),write(PC3),nl,
        write('Tareas PC4 : '),write(PC4),nl,
        write('Tareas PC5 : '),write(PC5),nl,
	write('Tiempo : '),write(T),nl,
	!.

