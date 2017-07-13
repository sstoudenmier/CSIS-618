reverse([], []).
reverse([Head | Tail], List) :-
	reverse(Tail, Result),
	append(Result, [Head], List).
