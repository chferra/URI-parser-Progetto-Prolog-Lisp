uri(S, A).

uri_parse(L, uri(S,A)) :- string_chars(L, URI), scheme(URI, URIexcS, S), authority(URIexcS, URIexcSA, A).

scheme([':'|T], T, '') :- !.
scheme([X|Xs], Rest, Result) :- identificatore(X), !, scheme(Xs, Rest, T), atom_concat(X,T,Result).

authority([], T, '') :- !.
authority([X|Xs], Rest, Result) :- !, authority(Xs, Rest, T), atom_concat(X, T, Result).

identificatore(X) :- non_member(X, ['/','?','#','@',':']), !, carattere(X).

carattere(C) :- reserved(C); unreserved(C).

reserved(C) :- gen-delims(C); sub-delims(C).
gen-delims(C) :- member(C, [':','/','?','#','[',']','@']).
sub-delims(C) :- member(C, ['!','$','&','\'','(',')','*','+',',',';','=']).

unreserved(C) :- digit(C); lower_az(C); upper_az(C); member(C, ['-','.','_','~']).

lower_az(C) :- char_code(C, N), N >= 97, N =< 122, !.
upper_az(C) :- char_code(C, N), N >= 65, N =< 90, !.

digit(C) :- atom_number(C, D), D >= 0, D =< 9, !.

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).