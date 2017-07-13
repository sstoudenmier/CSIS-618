max([],0).
max([H|T],M) :- max(T,M1),
		(H > M1 -> M = H ; M = M1).
