ack(0,Y,A) :- A is Y+1.
ack(X,0,A) :-
	      X1 is X-1,
	      ack(X1,1,A).
ack(X,Y,A) :-

	      X1 is X-1,
	      Y1 is Y-1,
	      ack(X,Y1,A1),
	      ack(X1,A1,A).

