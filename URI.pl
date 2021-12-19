uri(S, UI, H, Port, Path, Q, F).

uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI),
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, Port),
						pqf(URIexcSA, [], Path, Q, F), !.
uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						pqf(['/'|URIexcS], [], Path, Q, F), !.
uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						pqf(URIexcS, [], Path, Q, F), !.	
uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :- 
						string_chars(L, URI), 
						scheme(URI, URIexcS, S),
						scheme-syntax(URIexcS, [], UI, H, Path), !.
						
scheme-syntax(X, Rest, UI, Host, _) :- userinfo(X, ['@'|RestUI], UI), 
									host(RestUI, Rest, Host).
scheme-syntax(X, Rest, UI, _, _) :- userinfo(X, Rest, UI).
scheme-syntax(X, Rest, _, UI, _) :- host(X, Rest, UI).
 
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
							atom_concat('/', I, SlashI),
							atom_concat(SlashI, Is, Result).
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

host(H, Rest, Result) :- countGroup(H, Rest, Result, 4), !.
host(H, Rest, Result) :- identificatore-host(H, RestI, I), !,
                            identificatori_host(RestI, Rest, Is), !,
						    atom_concat('', I, DotI),
							atom_concat(DotI, Is, Result).
				
host(Rest, Rest, '').


ip(X, Rest, Result) :- countGroup(X, Rest, Result, 4).
id44Test(T, Rest, Result) :- string_chars(T, L),
							id44(L, Rest, Result).

countGroup(['.'|X], Rest, Result, Ngr) :- write(X),
							digits(X, Rest0, R0),							
							between(R0), !,
							countGroup(Rest0, Rest, Ds, N),
							atom_concat('.', R0, R0p),
							atom_concat(R0p, Ds, Result),
							Ngr is N + 1,
							Ngr > 0, !.
							
countGroup(X, Rest, Result, Ngr) :- digits(X, Rest0, R0),
                            between(R0), !,
							countGroup(Rest0, Rest, Ds, N),
							Ngr is N + 1,
							atom_concat(R0, Ds, Result),
							Ngr > 0, !.

countGroup(Rest, Rest, '', 0).



between(X) :- atom_number(X, D),
              D < 255,
			  D >= 0.

identificatori_host(['.'|Xs], Rest, Result) :- identificatore-host(Xs, RestI, I), !,
									identificatori_host(RestI, Rest, Is), !,
									atom_concat('.', I, DotI),
									atom_concat(DotI, Is, Result).
identificatori_host(Rest, Rest, '').									
									
identificatore(X, Rest, Result) :- caratteri(X, Rest, Result, ['/','?','#','@',':']), 
									Result \= ''.
									
identificatore-host(X, Rest, Result) :- caratteri(X, Rest, Result, ['.','/','?','#','@',':']),
										Result \= ''.

								
id44_01(['.'|X], Rest, Result) :- write('CC> '), write(X), nl,
                                 X = [], !,
								 Rest = [],
							     atom_concat('.', '', Result).

id44(X, Rest, Result) :- caratteriAN(X, Rest0, Result0), !,
write('AA> '), write(Rest0), write(' -- '),	write(Result0), nl,
								id44_01(Rest0, Rest, Result1), !,
write('BB> '), write(Rest), write(' -- '),	write(Result1), nl,
								atom_concat(Result0, Result1, Result),
								string_length(Result, Ln),
								Ln =< 44, !,
								Rest = [], !.							
								
									
id44(Rest, Rest, '').
id44_01(Rest, Rest, '').

port([':'|Xs], Rest, Result) :- digits(Xs, Rest, Ds),
								atom_number(Ds, Result),
								Result \= '', !.
port(Rest, Rest, 80).


caratteriAN([C|Cs], Rest, Result):- carattereAN(C), !,
								caratteriAN(Cs, Rest, R), !,
								atom_concat(C, R, Result).
caratteriAN(Rest, Rest, '').

carattereAN(C) :- digit(C); lower_az(C); upper_az(C).				

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

uri_display(L) :- 
						string_chars(L, URI), 
						uri_parse(L, uri(S, UI, H, Port, Path, Q, F)),
						my_write(['Schema: ', S]),
						my_write(['UserInfo: ', UI]),
						my_write(['Host: ', H]),
						my_write(['Port: ', Port]),
						my_write(['Path: ', Path]),
						my_write(['Query: ', Q]),
						my_write(['Fragment: ', F]).

uri_display(L, Stream) :- 
						current_output(O),
						open(Stream, append, StreamId),
						set_output(StreamId),
						uri_parse(L, uri(S, UI, H, Port, Path, Q, F)),
						writeln('URI display:'),
						my_write(['\tSchema: ', S]),
						my_write(['\tUserInfo: ', UI]),
						my_write(['\tHost: ', H]),
						my_write(['\tPort: ', Port]),
						my_write(['\tPath: ', Path]),
						my_write(['\tQuery: ', Q]),
						my_write(['\tFragment: ', F]),
						set_output(O),
						close(StreamId).

my_write(Terms) :- 	atomics_to_string(Terms, Res),
					writeln(Res).

						