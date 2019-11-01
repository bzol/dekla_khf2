intersection(_,[],[]). 
intersection(L1,[H2|L2],[H2|L3]) :-
	member(H2,L1),
	intersection(L1,L2,L4).	
intersection(L1,[H2|L2],L3) :-
	intersection(L1,L2,L3).

subList([],_,[]).
subList([H1|L1],L2,[H1|L]) :-
	\+ member(H1,L2),
	subList(L1,L2,L).	
subList([H1|L1],L2,L) :-
	subList(L1,L2,L).

getCell(Mtx,X,Y,LL) :-
	length(Mtx,Len),
	Lqt2=integer(sqrt(Len)),
	rows(Mtx,X,Y,1,Lqt2,LL2),	
	flatten(LL2,LL).

rows([],_,_,_,_,[]). 
rows([H|Mtx],X,Y,X2,Lqt,[Hc|LL]) :-
	Xd = X-1,
	X2d = X2-1,
	div(Xd,Lqt)=:=div(X2d,Lqt),
	cols(H,Y,1,Lqt,Hc),
	X3=X2+1,
	rows(Mtx,X,Y,X3,Lqt,LL).
rows([H|Mtx],X,Y,X2,Lqt,LL) :-
	X3=X2+1,
	rows(Mtx,X,Y,X3,Lqt,LL).

cols([],_,_,_,[]).
cols([H|Row],Y,Y2,Lqt,[H|LL]) :-
	write(H),
	Yd is Y-1,
	Y2d is Y2-1,
	div(Yd,Lqt)=:=div(Y2d,Lqt),
	Y3=Y2+1,
	cols(Row,Y,Y3,Lqt,LL).
cols([H|Row],Y,Y2,Lqt,LL) :-
	Y3=Y2+1,
	cols(Row,Y,Y3,Lqt,LL).


parityCheck(Mtx,X,Y,Par) :-
	nth1(X,Mtx,Row),
	nth1(Y,Row,Field),
	write(Field),
	(member(e,Field)->
		length(Mtx,Len),
		oddList(Len,1,Par)
	; member(o,Field)->
		write(Field),
		length(Mtx,Len),
		evenList(Len,1,Par)
	; Par=[]
	).

evenList(0,_,[]).
evenList(Len,X,Par) :-
	0 is mod(Len,2),
	Len2 is Len-1,
	X2 is X+1,
	evenList(Len2,X2,Par).
evenList(Len,X,[X|Par]) :-
	Len2 is Len-1,
	X2 is X+1,
	evenList(Len2,X2,Par).

oddList(0,_,[]).
oddList(Len,X,[X|Par]) :-
	0 is mod(Len,2),
	Len2 is Len-1,
	X2 is X+1,
	oddList(Len2,X2,Par).
oddList(Len,X,Par) :-
	Len2 is Len-1,
	X2 is X+1,
	oddList(Len2,X2,Par).

union([],[],[]).
union([],[H2|L2],[H2|L3]) :-
	union([],L2,L3).
union([H1|L1],L2,L3) :-
	member(H1,L2),
	union(L1,L2,L3).
union([H1|L1],L2,[H1|L3]) :-
	union(L1,L2,L3).

rowCheck([],_,_,_).
rowCheck([H|Mtx],X,X2,L) :-
	write([X,X2]),
	X = X2,
	getField(H,L).
rowCheck([H|Mtx],X,X2,L) :-
	X3 is X2+1,
	rowCheck(Mtx,X,X3,L).

getField([],[]).
getField([H|Row],[V|Vals]) :-
	getVals(H,V),
	V \= [],
	getField(Row,Vals).	
getField([H|Row],Vals) :-
	getField(Row,Vals).

getVals([],[]).
getVals([H|F],X) :-
	v(X) = H,
	getVals(F,V).	
getVals([H|F],V) :-
	getVals(F,V).	

colCheck([],_,[]).
colCheck([H|Mtx],Y,[V|L]) :-
	write('t'),
	getColField(H,Y,1,V),
	V \= [],
	colCheck(Mtx,Y,L).
colCheck([H|Mtx],Y,L) :-
	colCheck(Mtx,Y,L).

getColField([H|Col],Y,Y2,V) :-
	Y = Y2,
	write('c2'),
	getVals(H,V).
getColField([H|Col],Y,Y2,V) :-
	Y3 is Y2+1,
	getColField(Col,Y,Y3,V).	

cellCheck(Mtx,X,Y,L) :-
	getCell(Mtx,X,Y,L2),
	getCellVals(L2,L).

getCellVals([],[]).
getCellVals([H|L],[X|L2]) :-
	H = v(X),
	getCellVals(L,L2).	
getCellVals([H|L],L2) :-
	getCellVals(L,L2).

genNumbers(X,[X|L]) :-
	X \= 0,
	X2 is X-1,
	genNumbers(X2,L).	
genNumbers(_,[]).

ertekek(Mtx,X-Y,L10) :-
	cellCheck(Mtx,X,Y,L1),
	rowCheck(Mtx,X,1,L2),
	colCheck(Mtx,Y,L3),
	parityCheck(Mtx,X,Y,L4),
	union(L1,L2,L5),
	union(L5,L3,L6),
	union(L6,L4,L7),
	length(Mtx,Len),
	genNumbers(Len,L8),
	subList(L8,L7,L10).
