inputTxt(T) :- string_chars(T, L), fragment(L, X).

%digit(D) :- number(D), D >= 0, D =< 9.
%digit(D) :- char_code(D,N), digit(N).

identificatore([I|Is]) :- !, identificatore(I), !, identificatore(Is).
identificatore(I) :- non_member(I, ['/','?','#','@',':']).

identificatore-host([I|Is]) :- !, identificatore-host(I), !, identificatore-host(Is).
identificatore-host(I) :- non_member(I, ['.','/','?','#','@',':']).

query([],'').
query([Q|Qs],R) :- Q \= '#', query(Qs,T), atom_concat(Q, T, R).

allowedSymFragment(['?','/',':','@','-','.','_','~','!','$','&','\'','(',')','*','+',',',';','=']).
fragment(F) :- allowedSymFragment(L), member(F, L), !.
fragment(F) :- string_codes(F, R), R >= 65, R =< 90, !.
fragment(F) :- string_codes(F, R), R >= 97, R =< 122, !.
fragment(F) :- digit(F), !.
fragment([],'').
fragment([F|Fs], R) :- fragment(F), !, fragment(Fs, S), atom_concat(F, S, R).



port([CT|Cs]) -->
	digit(CT), !,
	port(Cs).

port([]) -->
	[80].

caratteri([CT|Cs]) --> 
		carattere(CT), !,
		caratteri(Cs).
		
caratteri([]) --> [].

carattere(C) -->
		unreserved(C);
		reserved(C).

unreserved(C) --> 
	digit(C);
	upper_az(C);
	lower_az(C);
	symb(C).

reserved(C) -->
	gen-delims(C);
	sub-delims(C).
	
digit(C) -->
	[C],
	{ number(C), C >= 0, C =< 9
	}.

symb(C) -->
	[C],
	{ member(C, ['-','.','_','~'])
	}.
	
lower_az(C) -->
	[C],
	{ char_code(C, N), N >= 97, N =< 122
	}.
	
upper_az(C) -->
	[C],
	{ char_code(C, N), N >= 65, N =< 90
	}.
	
gen-delims(C) -->
	[C],
	{ member(C, [':','/','?','#','[',']','@'])
	}.

sub-delims(C) -->
	[C],
	{ member(C, ['!','$','&','\'','(',')','*','+',',',';','='])
	}.	



%carattere(C) :- char_code(C, R), R >= 65, R <= 90, !.
%carattere(C) :- char_code(C, R), R >= 97, R <= 122, !.
%carattere(C) :- digit(C), !.



port([], 80).
port([X], X) :- digit(X), !.
port([X|Xs], R) :- digit(X), port(Xs, N), atom_concat(X, N, R).

non_member(X, [X|_]) :- !, fail.
non_member(X, [_|Xs]) :- !, non_member(X, Xs).
non_member(_, []).






