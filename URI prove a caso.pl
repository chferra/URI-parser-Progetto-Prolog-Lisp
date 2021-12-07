uri(S, A, UI, H, P).

uri_parse(L, uri(S, A, UI, H, P)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, P).

scheme(X, Rest, Result) :- identificatore(X, [':'|Rest], Result).

authority(['/','/'|Xs], Rest, UI, H, P) :-
								userinfo(Xs, ['@'|Rest_excUI], UI), !, 
								host(Rest_excUI, [':'|Rest_excUIH], H), !,
								port(Rest_excUIH, Rest, P), !.
authority(['/','/'|Xs], Rest, UI, H, 80) :-
								userinfo(Xs, ['@'|Rest_excUI], UI), !, 
								host(Rest_excUI, Rest, H).	
authority(['/','/'|Xs], Rest, '', H, P) :- 
								host(Xs, [':'|Rest_excH], H), !,
								port(Rest_excH, Rest, P), !.									
authority(['/','/'|Xs], Rest, '', H, 80) :-
								host(Xs, Rest, H).

userinfo(X, Rest, Result) :- identificatore(X, Rest, Result).

%host(['.'|Hs], Rest, Result) :- identificatore(Hs, S, P), !, host(S, Rest, T), 
host(H, Rest, Result) :- !, identificatore(H, ExcI, R). %, !,
						%host(ExcI, Rest, S), !,
						%atom_concat(R,S,Result).

identificatore(X, Rest, Result) :- caratteri(X, Rest, Result, ['/','?','#','@',':']), 
									Result \= ''.
identificatore-host(X, Rest, Result) :- caratteri(X, Rest, Result, ['.','/','?','#','@',':']),
										Result \= ''.

inputTxt(T, Rest, Result) :- string_chars(T, L), port(L, Rest, Result).

port(D, Rest, Result) :- digits(D, Rest, Result), Result \= '', !.
port(X, Rest, 80).

%caratteri([],[],'',_).
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