inputTxt(T, X) :- string_chars(T, L), path(L, X).


fragment(X, Y) :- caratteri(X, Y, []).
identificatore(X, Y) :- caratteri(X, Y, ['/','?','#','@',':']).
identificatore-host(X, Y) :- caratteri(X, Y, ['.','/','?','#','@',':']).
query(X, Y) :- caratteri(X, Y, ['#']).

caratteri([],'',_).
caratteri([C|Cs], R, Filtri) :- non_member(C,Filtri), carattere(C), !, caratteri(Cs, S, Filtri), atom_concat(C, S, R).
carattere(C) :- reserved(C); unreserved(C).

port([],80).
port([D],D) :- digit(D), !.
port([D|Ds], Y) :- digit(D), !, port(Ds, R), atom_concat(D, R, Y).

digit(C) :- atom_number(C, D), D >= 0, D =< 9, !.

path([X], Y) :- identificatore(X, Y), !.
path(['/'|Xs], Z) :- path(Xs, Y), !, atom_concat('/', Y, Z).
path([X|Xs], Z) :- identificatore(X, Y), !, path(Xs, R), atom_concat(Y, R, Z).

terna(X, Y) :- gruppo(X, Y, 3). 
gruppo([],'',0).
gruppo([X|Xs], S, N) :- write(X), N > 0, digit(X), gruppo(Xs, T, P), N is P + 1, atom_concat(X,T,S).


reserved(C) :- gen-delims(C); sub-delims(C).
gen-delims(C) :- member(C, [':','/','?','#','[',']','@']).
sub-delims(C) :- member(C, ['!','$','&','\'','(',')','*','+',',',';','=']).

unreserved(C) :- digit(C); lower_az(C); upper_az(C); member(C, ['-','.','_','~']).

lower_az(C) :- char_code(C, N), N >= 97, N =< 122, !.
upper_az(C) :- char_code(C, N), N >= 65, N =< 90, !.

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).