uri(S, UI, H, Port, Path, Q, F).

uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, Port),
						pqf(URIexcSA, [], Path, Q, F).

scheme(X, Rest, Result) :- identificatore(X, [':'|Rest], Result).

authority(['/','/'|Xs], Rest, UI, H, P) :-
								userinfo(Xs, RestUI, UI), 
								host(RestUI, RestH, H),
								port(RestH, Rest, P), !.
								
userinfo(X, Rest, Result) :- identificatore(X, ['@'|Rest], Result), !.
userinfo(Rest, Rest, '').
								
pqf([],[],'','',''). 
pqf(['/'|Xs], Rest, P, Q, F) :- path(Xs, RestP, P),
								query(RestP, RestQ, Q),
								fragment(RestQ, Rest, F), !.

query(['?'|Xs], Rest, Result) :- caratteri(X, Rest, Result, ['#']),
								Result \= '', !.
query(Rest, Rest, '').

fragment(['#'|Xs], Rest, Result) :- caratteri(X, Rest, Result, []),
								Result \= '', !.
fragment(Rest, Rest, '').


host(H, Rest, Result) :- identificatore(H, Rest, Result). 


identificatore(X, Rest, Result) :- caratteri(X, Rest, Result, ['/','?','#','@',':']), 
									Result \= ''.
identificatore-host(X, Rest, Result) :- caratteri(X, Rest, Result, ['.','/','?','#','@',':']),
										Result \= ''.

inputTxt(T, Rest, Result) :- string_chars(T, L), port(L, Rest, Result).

port([':'|Xs], Rest, Result) :- digits(Xs, Rest, Result),
								Result \= '', !.
port(Rest, Rest, 80).

caratteri([C|Cs], Rest, Result, Filtri) :- 
								non_member(C,Filtri), !,
								carattere(C), !, 
								caratteri(Cs, Rest, R, Filtri), !,
								atom_concat(C, R, Result).
caratteri(Rest, Rest, '', _).

carattere(C) :- reserved(C); unreserved(C).

reserved(C) :- gen-delims(C); sub-delims(C).
gen-delims(C) :- member(C, [':','/','?','#','[',']','@']).
sub-delims(C) :- member(C, ['!','$','&','\'','(',')','*','+',',',';','=']).

unreserved(C) :- digit(C); lower_az(C); upper_az(C); member(C, ['-','.','_','~']).

lower_az(C) :- char_code(C, N), N >= 97, N =< 122, !.
upper_az(C) :- char_code(C, N), N >= 65, N =< 90, !.

digits([D|Ds], Rest, Result) :- digit(D), !,
								digits(Ds,Rest,R), 
								atom_concat(D,R,Result). 
digits(Rest,Rest,'').
digit(C) :- atom_number(C, D), D >= 0, D =< 9, !.

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).