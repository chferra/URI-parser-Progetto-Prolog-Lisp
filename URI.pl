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
pqf(['/'|Xs], Rest, P, Q, F) :- 
								path(Xs, RestP, P),
								query(RestP, RestQ, Q),
								fragment(RestQ, Rest, F), !.

inputTxt(T, Rest, Result) :- string_chars(T, L), path(L, Rest, Result).


path(Xs, Rest, Result) :- identificatore(Xs, RestI, I), !,
							identificatori(RestI, Rest, Is), !, 
							atom_concat(I, Is, Result).
path(Rest, Rest, '').

identificatori(['/'|Xs], Rest, Result) :- identificatore(Xs, RestI, I), !,
									identificatori(RestI, Rest, Is), !,
									atom_concat('/', I, SlashI),
									atom_concat(SlashI, Is, Result).	
identificatori(Rest, Rest, '').						
											
query(['?'|Xs], Rest, Result) :- caratteri(Xs, Rest, Result, ['#']),
								Result \= '', !.
query(Rest, Rest, '').

fragment(['#'|Xs], Rest, Result) :- caratteri(Xs, Rest, Result, []),
								Result \= '', !.
fragment(Rest, Rest, '').

host(H, Rest, Result) :- identificatore-host(H, Rest, Result). 
									
identificatore(X, Rest, Result) :- caratteri(X, Rest, Result, ['/','?','#','@',':']), 
									Result \= ''.
									
identificatore-host(X, Rest, Result) :- caratteri(X, Rest, Result, ['.','/','?','#','@',':']),
										Result \= ''.



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

%%% 
%%% Display URI components
%%%
uri_display(L) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, Port),
						pqf(URIexcSA, [], Path, Q, F),
						write('Schema: '),
						write(S),
						nl,
						write('UserInfo: '),
						write(UI),
						nl,
						write('Host: '),
						write(H),
						nl,
						write('Port: '),
						write(Port),
						nl,
						write('Path: '),
						write(Path),
						nl,
						write('Query: '),
						write(Q),
						nl,
						write('Fragment: '),
						write(F),
						nl.

uri_display(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, Port),
						pqf(URIexcSA, [], Path, Q, F),
						write('Schema: '),
						write(S),
						nl,
						write('UserInfo: '),
						write(UI),
						nl,
						write('Host: '),
						write(H),
						nl,
						write('Port: '),
						write(Port),
						nl,
						write('Path: '),
						write(Path),
						nl,
						write('Query: '),
						write(Q),
						nl,
						write('Fragment: '),
						write(F),
						nl.
						