%%%% -*- Mode: Prolog -*-

%%%% uri_parse.pl

%uri(S, UI, H, Port, Path, Q, F).
%inputTxt(T, Rest, Result) :- string_chars(T, L), path(L, Rest, Result).

uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :-
						string_chars(L, URI),
						scheme(URI, URIexcS, S),
						downcase_atom(S, FixedS),
						member(FixedS,
						       ['mailto', 'tel', 'fax', 'news', 'zos']), !,
						scheme-syntax(URIexcS, S, UI, H, Port, Path, Q, F), !.

uri_parse(L, uri(S, UI, H, Port, Path, Q, F)) :-
						string_chars(L, URI),
						scheme(URI, URIexcS, S),
						authority(URIexcS, URIexcSA, UI, H, Port),
						pqf(URIexcSA, false, Path, Q, F), !.

scheme-syntax(X, Spattern, UI, H, 80, [], [], []) :-
						downcase_atom(Spattern, 'mailto'),
						userinfo(X, false, ['@'|RestUI], UI),
						host(RestUI, false, [], H).

scheme-syntax(X, Spattern, UI, [], 80, [], [], []) :-
						downcase_atom(Spattern, 'mailto'),
						userinfo(X, true, [], UI).
scheme-syntax(X, Spattern, UI, [], 80, [], [], []) :-
						downcase_atom(Spattern, 'tel'),
						userinfo(X, true, [], UI).
scheme-syntax(X, Spattern, UI, [], 80, [], [], []) :-
						downcase_atom(Spattern, 'fax'),
						userinfo(X, true, [], UI).
scheme-syntax(X, Spattern, [], H, 80, [], [], []) :-
						downcase_atom(Spattern, 'news'),
						host(X, true, [], H).
scheme-syntax(X, Spattern, UI, H, Port, Path, Q, F) :-
						downcase_atom(Spattern, 'zos'),
						authority(X, URIexcA, UI, H, Port),
						pqf(URIexcA, true, Path, Q, F), !.

scheme(X, Rest, Result) :-
						identificatore(X, [':'|Rest], Result).

authority(['/','/'|Xs], Rest, UI, H, P) :-
						userinfo(Xs, true, ['@'|RestUI], UI),
						host(RestUI, false, RestH, H),
						port(RestH, Rest, P), !.
authority(['/','/'|Xs], Rest, UI, H, 80) :-
						userinfo(Xs, true, ['@'|RestUI], UI),
						host(RestUI, false, Rest, H).
authority(['/','/'|Xs], Rest, [], H, P) :-
						host(Xs, false, RestH, H),
						port(RestH, Rest, P), !.
						
authority(Rest, Rest, [], [], 80).

userinfo(X, _, Rest, Result) :-
						identificatore(X, Rest, Result), !.
userinfo(Rest, true, Rest, []).

pqf([], _, [],[],[]).
pqf(['/'|Xs], Zos, P, Q, F) :-
						path(Xs, Zos, RestP, P), !,
						query(RestP, RestQ, Q), !,
						fragment(RestQ, [], F).

path(X, false, Rest, Result) :-
						identificatore(X, RestI, I),
						identificatori(RestI, Rest, Is), !,
						atomic_concat(I, Is, Result).

path(X, true, Rest, Result) :-
						id44(X, ['('|Rest44], Res44),
						id8(Rest44, [')'|Rest], Res8), !,
						atomic_concat('(', Res8, RRes8),
						atomic_concat(RRes8, ')', RRes8R),
						atomic_concat(Res44, RRes8R, Result).
path(X, true, Rest, Result) :-
						id44(X, Rest, Result), !.				
path(X, false, Rest, Result) :-
						identificatore(X, Rest, Result), !.
path(Rest, false, Rest, []).

identificatori(['/'|Xs], Rest, Result) :-
						identificatore(Xs, RestI, I),
						identificatori(RestI, Rest, Is), !,
						atomic_concat('/', I, SlashI),
						atomic_concat(SlashI, Is, Result).
identificatori(['/'|Xs], Rest, Result) :-
						identificatore(Xs, Rest, X),
						atomic_concat('/', X, Result), !.

query(['?'|Xs], Rest, Result) :-
						caratteri(Xs, Rest, Result, ['#']), !.
query(Rest, Rest, []).

fragment(['#'|Xs], Rest, Result) :-
						caratteri(Xs, Rest, Result, []), !.
fragment(Rest, Rest, []).

host(H, _, Rest, Result) :-
						countGroup(H, Rest, Result, 4).
host(H, _, Rest, Result) :-
						identificatore_host(H, RestI, I),
						identificatori_host(RestI, Rest, Is), !,
						atomic_concat(I, Is, Result).
host(X, _, Rest, Result) :-
						identificatore_host(X, Rest, Result), !.
host(Rest, true, Rest, []).

ip(X, Rest, Result) :-
						countGroup(X, Rest, Result, 4).

countGroup(['.'|X], Rest, Result, Ngr) :-
						digits(X, RestD, D),
						atom_number(D, Num),
						between(0, 255, Num), !,
						countGroup(RestD, Rest, Ds, N),
						atomic_concat('.', D, DotD),
						atomic_concat(DotD, Ds, Result),
						Ngr is N + 1,
						Ngr > 0, !.

countGroup(X, Rest, Result, Ngr) :-
						digits(X, RestD, D),
						atom_number(D, Num),
						between(0, 255, Num), !,
						countGroup(RestD, Rest, Ds, N),
						Ngr is N + 1,
						atomic_concat(D, Ds, Result),
						Ngr > 0, !.

countGroup(X, Rest, Result, 0) :-
						digits(X, Rest, D),
						atom_number(D, Result),
						between(0, 255, Result), !.

identificatori_host(['.'|Xs], Rest, Result) :-
						identificatore_host(Xs, RestI, I),
						identificatori_host(RestI, Rest, Is), !,
						atomic_concat('.', I, DotI),
						atomic_concat(DotI, Is, Result).
identificatori_host(['.'|Xs], Rest, Result) :-
						identificatore_host(Xs, Rest, I),
						atomic_concat('.', I, Result), !.

identificatore(X, Rest, Result) :-
						caratteri(X, Rest, Result, ['/','?','#','@',':']).

identificatore_host(X, Rest, Result) :-
						caratteri(X, Rest, Result, ['.','/','?','#','@',':']).

id44tail(['.'|Xs], Rest, Result) :-
						id44tail(Xs, Rest, Res),
						atomic_concat('.', Res, Result).
id44tail(X, Rest, Result) :-
						caratteriAN(X, RestCs, Cs),
						id44tail(RestCs, Rest, Res), !,
						atomic_concat(Cs, Res, Result).
id44tail(X, Rest, Result) :-
						caratteriAN(X, Rest, Result), !.

id44([X|Xs], Rest, Result) :-
						carattereAlfabetico(X), !,
						id44tail([X|Xs], Rest, Result), !,
						string_length(Result, Ln),
						between(1, 44, Ln).

id8([X|Xs], Rest, Result) :-
						carattereAlfabetico(X),
						caratteriAN([X|Xs], Rest, Result),
						string_length(Result, Ln),
						between(1, 8, Ln).

port([':'|Xs], Rest, Result) :-
						digits(Xs, Rest, Ds),
						atom_number(Ds, Result).
port(Rest, Rest, 80).

caratteriAN([C|Cs], Rest, Result):-
						carattereAN(C),
						caratteriAN(Cs, Rest, R), !,
						atomic_concat(C, R, Result).
caratteriAN([C|Rest], Rest, C) :- carattereAN(C), !.

carattereAN(C) :-
						digit(C), !.
carattereAN(C) :-
						carattereAlfabetico(C), !.

caratteri([' '|Cs], Rest, Result, Filtri) :-
						caratteri(Cs, Rest, R, Filtri), !,
						atomic_concat('%20', R, Result).

caratteri([C|Cs], Rest, Result, Filtri) :-
						non_member(C,Filtri),
						caratteri(Cs, Rest, R, Filtri), !,
						atomic_concat(C, R, Result).
caratteri([' '|Cs], Cs, '%20', _) :- !.
caratteri([C|Rest], Rest, C, Filtri) :-
						non_member(C,Filtri), !.

carattereAlfabetico(C) :- lower_az(C), !.
carattereAlfabetico(C) :- upper_az(C), !.

lower_az(C) :-
						char_code(C, N),
						N >= 97, N =< 122, !.

upper_az(C) :-
						char_code(C, N),
						N >= 65,
						N =< 90, !.

digits([D|Ds], Rest, Result) :-
						digit(D),
						digits(Ds, Rest, R), !,
						atomic_concat(D, R, Result).
digits([D|Rest], Rest, D) :- digit(D), !.

digit(C) :-
						atom_number(C, D),
						between(0, 9, D), !.

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).

uri_display(uri(S, UI, H, Port, Path, Q, F)) :-
						multi_writeln(['Schema: ', S]),
						multi_writeln(['UserInfo: ', UI]),
						multi_writeln(['Host: ', H]),
						multi_writeln(['Port: ', Port]),
						multi_writeln(['Path: ', Path]),
						multi_writeln(['Query: ', Q]),
						multi_writeln(['Fragment: ', F, '\n']).

uri_display(uri(S, UI, H, Port, Path, Q, F), StreamAlias) :-
						writeln(StreamAlias, 'URI display:'),
						multi_writeln(StreamAlias, ['\tSchema: ', S]),
						multi_writeln(StreamAlias, ['\tUserInfo: ', UI]),
						multi_writeln(StreamAlias, ['\tHost: ', H]),
						multi_writeln(StreamAlias, ['\tPort: ', Port]),
						multi_writeln(StreamAlias, ['\tPath: ', Path]),
						multi_writeln(StreamAlias, ['\tQuery: ', Q]),
						multi_writeln(StreamAlias, ['\tFragment: ', F, '\n']).

multi_writeln([T]) :-
						writeln(T), !.

multi_writeln([T | Terms]) :-
						write(T),
						multi_writeln(Terms).
						
multi_writeln(StreamAlias, [T]) :-
						writeln(StreamAlias, T), !.		
						
multi_writeln(StreamAlias, [T | Terms]) :-
						write(StreamAlias, T),
						multi_writeln(StreamAlias, Terms).

%%%% end of file -- uri_parse.pl