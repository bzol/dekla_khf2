intersection(_,[],L) :-
	L =[], !.
intersection(L1,[H2|L2],[H2|L3]) :-
	member2(H2,L1),
	intersection(L1,L2,L3), !.	
intersection(L1,[H2|L2],L3) :-
	intersection(L1,L2,L3).

subList([],_,L) :-
	L = [], !.	
subList([H1|L1],L2,[H1|L]) :-
	\+ member2(H1,L2),
	subList(L1,L2,L), !.	
subList([H1|L1],L2,L) :-
	subList(L1,L2,L), !.

flatten2([],[]).
flatten2([H2|L2],L3) :-
	append(H2,L,L3),
	flatten2(L2,L).

getCell(Mtx,X,Y,Len,LL) :-
	rows(Mtx,X,Y,1,Len,LL2),	
	flatten2(LL2,LL),
	write(LL).

rows([],_,_,_,_,[]). 

rows([H|Mtx],X,Y,X2,Lqt,[Hc|LL]) :-
	Xd is X-1,
	X2d is X2-1,
	div(Xd,Lqt)=:=div(X2d,Lqt),
	cols(H,X,Y,X2,1,Lqt,Hc),
	X3 is X2+1,
	rows(Mtx,X,Y,X3,Lqt,LL), !.
rows([H|Mtx],X,Y,X2,Lqt,LL) :-
	X3 is X2+1,
	rows(Mtx,X,Y,X3,Lqt,LL).

cols([],_,_,_,_,_,[]).
cols([H|Row],X,Y,X2,Y2,Lqt,LL) :-
	write(t),
	write(X2),
	write(Y2),

	X = X2,
	Y = Y2,
	Y3 is Y2+1,
	cols(Row,X,Y,X2,Y3,Lqt,LL), !.
cols([H|Row],X,Y,X2,Y2,Lqt,[H|LL]) :-
	Yd is Y-1,
	Y2d is Y2-1,
	div(Yd,Lqt)=:=div(Y2d,Lqt),
	Y3 is Y2+1,
	cols(Row,X,Y,X2,Y3,Lqt,LL), !.
cols([H|Row],X,Y,X2,Y2,Lqt,LL) :-
	Y3 is Y2+1,
	cols(Row,X,Y,X2,Y3,Lqt,LL).

nth2(X,L2,Res) :-
	nth3(X,1,L2,Res).	

nth3(X,X2,[H|L2],Res) :-
	X = X2,
	Res = H, !.
nth3(X,X2,[H|L2],Res) :-
	X3 is X2+1,
	nth3(X,X3,L2,Res).

parityCheck(Mtx,X,Y,Len,Par) :-
	nth2(X,Mtx,Row),
	nth2(Y,Row,Field),
	Len2 is Len*Len,
	write(Field),
	(member2(e,Field)->
		oddList(Len2,1,Par)
	; member2(o,Field)->
		evenList(Len2,1,Par)
	; Par = []
	).

evenList(0,_,Par) :-
	Par = [], !.
evenList(Len,X,[X|Par]) :-
	0 is mod(X,2),
	Len2 is Len-1,
	X2 is X+1,
	evenList(Len2,X2,Par), !.
evenList(Len,X,Par) :-
	Len2 is Len-1,
	X2 is X+1,
	evenList(Len2,X2,Par), !.

oddList(0,_,Par) :-
	Par = [], !.
oddList(Len,X,[X|Par]) :-
	1 is mod(X,2),
	Len2 is Len-1,
	X2 is X+1,
	oddList(Len2,X2,Par), !.
oddList(Len,X,Par) :-
	Len2 is Len-1,
	X2 is X+1,
	oddList(Len2,X2,Par), !.

member2(X,[H|L]) :-
	X = H,
	true, !.
member2(X,[H|L]) :-
	member2(X,L), !.
member2(_,[]) :-
	false.

union([],[],L) :-
	L = [], !.
union([],[H2|L2],[H2|L3]) :-
	union([],L2,L3), !.
union([H1|L1],L2,L3) :-
	member2(H1,L2),
	union(L1,L2,L3), !.
union([H1|L1],L2,[H1|L3]) :-
	union(L1,L2,L3), !.

rowCheck([H|Mtx],X-Y,X2,L) :-
	write(a),
	X = X2,
	getField(H,Y,1,L), !.
rowCheck([H|Mtx],X-Y,X2,L) :-
	write(b),
	X3 is X2+1,
	rowCheck(Mtx,X-Y,X3,L).

getField([],_,_,[]).
getField([H|Row],Y,Y2,Vals) :-
	Y = Y2,
	Y3 = Y2+1,
	getField(Row,Y,Y3,Vals), !. 
getField([H|Row],Y,Y2,[V|Vals]) :-
	Y3 is Y2+1,
	getVals(H,V),
	V \= [],
	getField(Row,Y,Y2,Vals).	
getField([H|Row],Y,Y2,Vals) :-
	Y3 is Y2+1,
	getField(Row,Y,Y3,Vals).

getVals([],[]).
getVals([H|F],X) :-
	v(X) = H,
	getVals(F,V).	
getVals([H|F],V) :-
	getVals(F,V).	

colCheck([H|Mtx],X-Y,X2,L) :-
	X = X2,
	X3 is X2+1,
	colCheck(Mtx,X-Y,X3,L), !. 
colCheck([H|Mtx],X-Y,X2,[V|L]) :-
	X3 is X2+1,
	getColField(H,Y,1,V),
	V \= [],
	colCheck(Mtx,X-Y,X3,L), !.
colCheck([H|Mtx],X-Y,X2,L) :-
	X3 is X2+1,
	colCheck(Mtx,X-Y,X3,L), !.
colCheck([],_,_,L) :-
	L = [], !.

getColField([H|Col],Y,Y2,V) :-
	Y = Y2,
	getVals(H,V), !.
getColField([H|Col],Y,Y2,V) :-
	Y3 is Y2+1,
	getColField(Col,Y,Y3,V).	

cellCheck(Mtx,X,Y,Len,L) :-
	getCell(Mtx,X,Y,Len,L2),
	getCellVals(L2,L).

getCellVals([],[]).
getCellVals([H|L],[X|L2]) :-
	member2(v(X),H),
	getCellVals(L,L2), !.	
getCellVals([H|L],L2) :-
	getCellVals(L,L2).

genNumbers(X,[X|L]) :-
	X \= 0,
	X2 is X-1,
	genNumbers(X2,L), !.	
genNumbers(_,[]).

getFieldVal(Mtx,X,Y,Len2,Val2) :-
	nth2(X,Mtx,Row),
	nth2(Y,Row,Field),
	getVals(Field,Val),
	Val \= [],
	Val2 = [Val], !.
getFieldVal(_,_,_,_,L) :-
	L=[].

ertekek(s(Len,Mtx),X-Y,L12) :-
	Len2 is Len*Len,
	getFieldVal(Mtx,X,Y,Len2,FV),
	colCheck(Mtx,X-Y,1,L1),
	rowCheck(Mtx,X-Y,1,L2),
	parityCheck(Mtx,X,Y,Len,L3),
	cellCheck(Mtx,X,Y,Len,L4),
	union(L1,L2,L6),
	union(L6,L3,L7),
	union(L7,L4,L8),
	write(L4),

	genNumbers(Len2,L10),
	intersection(L8,FV,Li),
	( FV = [] ->
		subList(L10,L8,L12)
	  %subList(L10,L8,L12)
	; Li = [] ->
		L12 = FV
	; L12 = []
	).
