reverse(L1, L2) :- reverse_aux(L1, [], L2).
reverse_aux([], L1, L1).
reverse_aux([X|L1], L2, L3) :- reverse_aux(L1, [X|L2], L3).
