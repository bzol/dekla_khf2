%subList(L1,L2,L3). L1

subList(_,[],[]). 
subList(L1,[H2|L2],[H2|L3]) :-
	member(H2,L1),
	subList(L1,L2,L3).	
subList(L1,[H2|L2],L3) :-
	subList(L1,L2,L3).

getCell(Mtx,X-Y,LL) :-
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
	write('t'),
		length(Mtx,Len),
		evenList(Len,Par)
	; member(o,Field)->
		write(Field),
		length(Mtx,Len),
		oddList(Len,Par)
	).

evenList(0,_).
evenList(Len,[H|Par]) :-
	mod(Len,2) is 0,
	Len-1=L2,
	evenList(L2,Par).
oddList(0,_).
oddList(Len,[H|Par]) :-
	mod(Len,2) is 1,
	Len-1=L2,
	oddList(L2,Par).
%these return not permitted in field 
%parityCheck()
%rowCheck 
%colCheck
%cellCheck
%union



/**
Mtx=[[1,2,3,4],[5,6,7,8],[9,a,b,c],[d,e,f,g]],

rows(Mtx,2,4,1,2,LL).

*/
