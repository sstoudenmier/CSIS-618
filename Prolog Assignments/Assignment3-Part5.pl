fib(1, 1).
fib(2, 1).
fib(N, FN) :- N > 0,
	      N1 is N-2,
	      fib(N1, FN1),
	      N2 is N-1,
	      fib(N2, FN2),
	      FN is FN1 + FN2.
