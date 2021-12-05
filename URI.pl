inputTxt(T) :- string_chars(T, L), port(L, X).

digit(D) :- number(D), D >= 0, D =< 9.

identificatore([I|Is]) :- !, identificatore(I), !, identificatore(Is).
identificatore(I) :- non_member(I, ['/','?','#','@',':']).

identificatore-host([I|Is]) :- !, identificatore-host(I), !, identificatore-host(Is).
identificatore-host(I) :- non_member(I, ['.','/','?','#','@',':']).

query([]).
query([Q|Qs]) :- query(Q), query(Qs).
query(Q) :- Q \= '#'.

port([], 80).
port([X], X) :- digit(X), !.
port([X|Xs], R) :- digit(X), port(Xs, N), atom_concat(X, N, R).

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).









