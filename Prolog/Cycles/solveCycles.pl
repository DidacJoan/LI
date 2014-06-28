:-include(entradaRodear1).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

writeClauses:- forbidExteriorRows, forbidExteriorCols, linkInterior, setRestrictions.

% ------------------------------------------------------------------------------------------------------------------------
% Es produeix el minim nombre de clausules per tal de posar les variables de cada linia que conecta l´interior del
% tauler amb l´exterior d´aquest. Així es possible prohibir que els cicles passin per fora del tauler.

forbidExteriorRows:- rows(NR), columns(NC),
		IMAX is NR+1,JMAX is NC+1,
		between(1,IMAX,I),
		writeClause([\+h-I-0]),
		writeClause([\+h-I-JMAX]),fail.
forbidExteriorRows.

forbidExteriorCols:- rows(NR), columns(NC),
		IMAX is NR+1,JMAX is NC+1,
		between(1,JMAX,J),
		writeClause([\+v-0-J]),
		writeClause([\+v-IMAX-J]),fail.
forbidExteriorCols.

% ----------------------------------------------------------------------------------------------------------------------
% Si una variable (linia) es certa, per tal que pertanyi a un cicle ha de cumplir que per cada extrem d´aquesta,
% exactament una de les linies que la toca ha de ser certa.

linkInterior:-	rows(NR), columns(NC),
		IMAX is NR+1, JMAX is NC+1, 
		between(1,IMAX,I), between(1,JMAX,J),
		linkVertical(I,J), linkHorizontal(I,J), fail.
linkInterior.

oneTrueImpliesAtMostOne(A,[X|L]):- forall(member(M,L), writeClause( [ \+A, \+X, \+M] )),
				   oneTrueImpliesAtMostOne(A,L).
oneTrueImpliesAtMostOne(_,[]).

linkHorizontal(I,J):-	PREVI is I-1, PREVJ is J-1, NEXTJ is J+1, 
			    
		writeClause( [ \+h-I-J , h-I-NEXTJ , v-PREVI-NEXTJ , v-I-NEXTJ ] ),
		oneTrueImpliesAtMostOne(h-I-J , [ h-I-NEXTJ , v-PREVI-NEXTJ , v-I-NEXTJ ] ),
 
		writeClause( [ \+h-I-J  , h-I-PREVJ    ,   v-I-J     , v-PREVI-J ] ),
		oneTrueImpliesAtMostOne( h-I-J , [ h-I-PREVJ , v-I-J , v-PREVI-J ] ).		
				
linkVertical(I,J):- PREVI is I-1, NEXTI is I+1, PREVJ is J-1,

		writeClause( [ \+v-I-J , v-PREVI-J , h-I-J , h-I-PREVJ ] ),
		oneTrueImpliesAtMostOne( v-I-J , [ v-PREVI-J , h-I-J , h-I-PREVJ ] ),

		writeClause( [ \+v-I-J , v-NEXTI-J ,  h-NEXTI-J , h-NEXTI-PREVJ ] ),
		oneTrueImpliesAtMostOne( v-I-J , [ v-NEXTI-J , h-NEXTI-J , h-NEXTI-PREVJ ] ).

% ----------------------------------------------------------------------------------------------------------------------
% Per cada casella amb un nombre K al seu interior, es generen les clausules que obliguin a cumplirse 
% un atMostK i un atLeastK. Es podria haver utilitzat una codificacio diferent, pero la cuadratica es una bona 
% elecció en aquest cas. Les clausules de Exactly2 i Exactly3 han sigut obtingudes a partir d´arbres de decisió, aplicant
% la negacio a les combinacions que donaven una solució incorrecta i obtenint les clausules que codifiquen les combinacions
% correctes.

setRestrictions:- num(I,J,K),
		exactlyK(I,J,K), fail.
setRestrictions.

exactlyK(I,J,0):- NEXTI is I+1, NEXTJ is J+1,

		writeClause( [ \+h-I-J     ] ),
 		writeClause( [ \+h-NEXTI-J ] ),
		writeClause( [ \+v-I-J     ] ),
		writeClause( [ \+v-I-NEXTJ ] ).

exactlyK(I,J,1):- NEXTI is I+1, NEXTJ is J+1,

		writeClause( [ \+h-I-J     , \+h-NEXTI-J ] ),
		writeClause( [ \+h-I-J     , \+v-I-J     ] ),
		writeClause( [ \+h-I-J     , \+v-I-NEXTJ ] ),
		writeClause( [ \+h-NEXTI-J , \+v-I-J     ] ),
		writeClause( [ \+h-NEXTI-J , \+v-I-NEXTJ ] ),
		writeClause( [ \+v-I-J     , \+v-I-NEXTJ ] ),

		writeClause( [ v-I-J ,  h-I-J ,  v-I-NEXTJ , h-NEXTI-J ] ).

exactlyK(I,J,2):- NEXTI is I+1, NEXTJ is J+1, 

		writeClause( [ \+h-I-J , \+h-NEXTI-J ,   v-I-J , \+v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J ,   h-NEXTI-J , \+v-I-J , \+v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J ,   h-NEXTI-J ,   v-I-J ,   v-I-NEXTJ ] ),
		writeClause( [   h-I-J ,   h-NEXTI-J , \+v-I-J ,   v-I-NEXTJ ] ),
		writeClause( [   h-I-J , \+h-NEXTI-J , \+v-I-J , \+v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J , \+h-NEXTI-J , \+v-I-J               ] ),
		writeClause( [   h-I-J ,   h-NEXTI-J ,   v-I-J               ] ),
		writeClause( [   h-I-J , \+h-NEXTI-J ,   v-I-J ,   v-I-NEXTJ ] ).
		
			
exactlyK(I,J,3):- NEXTI is I+1, NEXTJ is J+1, 

		writeClause( [ \+h-I-J , \+h-NEXTI-J , \+v-I-J , \+v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J , \+h-NEXTI-J ,   v-I-J ,   v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J ,   h-NEXTI-J , \+v-I-J ,   v-I-NEXTJ ] ),
		writeClause( [ \+h-I-J ,   h-NEXTI-J ,   v-I-J               ] ),
		writeClause( [   h-I-J ,   h-NEXTI-J                         ] ),
		writeClause( [   h-I-J , \+h-NEXTI-J ,   v-I-J               ] ),
		writeClause( [   h-I-J , \+h-NEXTI-J , \+v-I-J ,   v-I-NEXTJ ] ).


% -----------------------------------------------------------------------------------------------------------------------

writeHeaderPS:-
    writeln('%!PS'),
    writeln('matrix currentmatrix /originmat exch def'),
    writeln('/umatrix {originmat matrix concatmatrix setmatrix} def'),
    writeln('[28.3465 0 0 28.3465 10.5 100.0] umatrix').

writeGrid:-
    writeln('0.01 setlinewidth'),
    writeVertGrid,
    writeHorizGrid.

writeVertGrid:-
    rows(R), columns(C), C1 is C+1,
    between(1,R,I), between(1,C1,J), drawVertical(I,J),fail.
writeVertGrid.

writeHorizGrid:-
    rows(R), columns(C), R1 is R+1,
    between(1,R1,I), between(1,C,J), drawHorizontal(I,J),fail.
writeHorizGrid.

drawVertical(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    Y1 is Y-Size,
    write(X), write(' '), write(Y1), write(' lineto'),nl,
    writeln('stroke').

drawHorizontal(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    X1 is X+Size,
    write(X1), write(' '), write(Y), write(' lineto'),nl,
    writeln('stroke').

writeNumbers:-
    num(I,J,K),
    writeNumber(I,J,K),
    fail.
writeNumbers.

writeNumber(I,J,K):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size + 3*Size/7,
    Y is 23-(I-1)*Size - 5*Size/7,
    writeln('0.001 setlinewidth'),
    S is Size/2,
    write('/Times-Roman findfont '), write(S), writeln(' scalefont setfont'),
    write(X), write(' '), write(Y), write(' moveto ('), write(K), writeln(') show').

writeSolution([X|M]):-
    writeLine(X),
    writeSolution(M).
writeSolution([]).
    
writeLine(X):-num2var(X,h-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawHorizontal(I,J).
writeLine(X):-num2var(X,v-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawVertical(I,J).
writeLine(_).

displaySol(M):-
    tell('graph.ps'),
    writeHeaderPS,
    writeGrid,
    writeNumbers,
    writeSolution(M),
    writeln('showpage'),
    told.

% ========== No need to change the following: =====================================

main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('cat model'),
	see(model), readModel(M), seen, displaySol(M),
	halt.

var2num(T,N):- hash_term(T,Key), varNumber(Key,T,N),!.
var2num(T,N):- retract(numVars(N0)), N is N0+1, assert(numVars(N)), hash_term(T,Key),
	assert(varNumber(Key,T,N)), assert( num2var(N,T) ), !.

writeHeader:- numVars(N),numClauses(C),write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-  retract(numClauses(N)), N1 is N+1, assert(numClauses(N1)),!.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.
unix(Comando):-shell(Comando),!.
unix(_).

readModel(L):- get_code(Char), readWord(Char,W), readModel(L1), addIfPositiveInt(W,L1,L),!.
readModel([]).

addIfPositiveInt(W,L,[N|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, !.
addIfPositiveInt(_,L,L).

readWord(99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!.
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
