hanoi(N,ML) :- move(N,ML,1,2,3).
move(0,[],X,Y,Z).
move(N,ML,X,Y,Z) :- N >= 1,
		    N1 is N-1,
		    move(N1,ML1,X,Z,Y),
		    append(ML1,[[N,X,Y]],ML2),
		    move(N1,ML3,Z,Y,X),
		    append(ML2,ML3,ML).
