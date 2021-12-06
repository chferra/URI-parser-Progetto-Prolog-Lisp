uri(S, A).

uri_parse(L, uri(S,A)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S)
						authority(URIexcS, URIexcSA, A).

scheme(X, Rest, Result) :- identificatore(X, [':'|Rest], Result).

authority(['/','/'|Xs], Rest, UI, H, P) :- 
								userinfo(Xs, ['@'|Rest_excUI], UI), !, 
								host(Rest_excUI, [':'|Rest_excUIH], H), !,
								port(Rest_excUIH, Rest, P), !.								 


identificatore(X, Rest, Result) :- caratteri(X, Rest, Result, ['/','?','#','@',':']).

caratteri([],[],'',_).
caratteri([C|Cs], Rest, Result, Filtri) :- 
								non_member(C,Filtri), !,
								carattere(C), !, 
								caratteri(Cs, Rest, R, Filtri), !,
								atom_concat(C, R, Result).
caratteri(Cs, Rest, '', _).

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