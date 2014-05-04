%Example of random Connect6 player
%reading stdin and writing stdout.
%(c) S. Zidek 2013

%"Compilation":
%	 swipl -q -g prolog -o xdumbo00 -c c6-dumb.pl

%CHANGELOG:
%v1: removed gtrace & starting whole game, i.e. with FIRST/START message
%v2: fixed error in get_coords - Sy2 contained semicolon
%v3: uncommented entry point
%v4: fixed asserting first stone
%v5: adapted to new SWI Prolog

:- dynamic stone/3. %db tvorena 3prvkovou entici stone
:- dynamic startStone/2. % pocatenci tahy
:- dynamic stonesPlayed/1. % pocet kamenu na desce
:- dynamic t_timeout/1.

% velikost plochy
board_size(19).

% odehrane kameny 
stonesPlayed(0). % max 361

timeout :-
  t_timeout(DT) -> (
    get_time(TS),
    TS >= DT -> true ; false
  ) ; false.

timeout(T) :-
  (retract(t_timeout(_)) -> true ; true ),
  get_time(TS),
  DT is TS + T,
  assert(t_timeout(DT)).

%Reads line from stdin, terminates on LF or EOF.
%nacte radek po znaku dokud neni konec souboru nebo radku vraci seznam nactenych symbolu
read_line(L) :-
	get_char(C), 
	(isEOFEOL(C), L = [], !; 
		read_line(LL), atom_codes(C,[Cd]), 
		[Cd|LL] = L). 

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file, halt; 
	(char_code(C,Code), Code==10).

% ziskani x a y hodnoty ze vstupniho seznamu
get_coords1(L, X, Y) :-
	atom_codes(';', [SemiColon]), % dekodovani 
	bagof(Isc, nth0(Isc, L, SemiColon), [Isc]), % bagof hledani Isc v nejake bd a ulozeni do [Isc] 
												% nth0(index, [], element) true pokud je element indexovan v []
	string_to_list(S, L), % prevod retezce na seznam

	atom_codes(',', [Comma]), % dekodovani carky
	bagof(Ic, nth0(Ic, L, Comma), [Icom]), 
	sub_string(S, 0, Icom, _, Sx), % hledani podretezce
	Sybeg is Icom+1, 
	Sylen is Isc-Icom-1,
	sub_string(S, Sybeg, Sylen, _, Sy), % podretezec( z S, od , delka , za , podretezec)

	string_to_list(Sx, Cx),
	string_to_list(Sy, Cy),

	number_codes(X, Cx), % prirazeni hodnoty symbolu
	number_codes(Y, Cy).

get_coords(L, X1, Y1, X2, Y2) :-
	atom_codes(';', [SC]), 
	bagof(Isc, nth0(Isc, L, SC), SemiColons),
	SemiColons = [SC1, _SC2],
	string_to_list(S, L),

	sub_string(S, 0, SC1, _, S1),
	string_to_list(S1, L1),
	atom_codes(',', [Comma]),
	bagof(Ic1, nth0(Ic1, L1, Comma), [Icom1]),
	sub_string(S1, 0, Icom1, _, Sx1), 
	Sy1beg is Icom1+1,
	sub_string(S1, Sy1beg, _, 0, Sy1),

	S2beg is SC1+1,
	sub_string(S, S2beg, _, 0, S2),
	string_to_list(S2, L2),
	bagof(Ic2, nth0(Ic2, L2, Comma), [Icom2]),
	sub_string(S2, 0, Icom2, _, Sx2), 
	Sy2beg is Icom2+1,
	sub_string(S2, Sy2beg, _, 1, Sy2),

	string_to_list(Sx1, Cx1),
	string_to_list(Sy1, Cy1),
	string_to_list(Sx2, Cx2),
	string_to_list(Sy2, Cy2),

	number_codes(X1, Cx1),
	number_codes(Y1, Cy1),
	number_codes(X2, Cx2),
	number_codes(Y2, Cy2).

write_stones(X1, Y1, X2, Y2) :-
	number_codes(X1, Sx1),
	number_codes(Y1, Sy1),
	number_codes(X2, Sx2),
	number_codes(Y2, Sy2),

	append(["STONES:", Sx1, ",", Sy1, ";", Sx2, ",", Sy2, ";"], L), % vypis
	put_line(L).

% zmena celkoveho poctu odehranych kamenu
updateStoneCount(N) :-
	stonesPlayed(X),
	XX = X + N,
	assert(stonesPlayed(XX)),
	retract(stonesPlayed(X)).

% TESTOVANI KONCE HRY - smerove fce
checkDown(P, N, X, Y, O) :- 
	(
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		YY is Y+1,
		checkDown(P, NN , X, YY, OO),
		O is OO+1
	); O is 0.

checkTop(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		YY is Y-1,
		checkTop(P, NN, X, YY, OO),
		O is OO+1
	); O is 0.

checkRight(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		XX is X+1,
		checkRight(P, NN, XX, Y, OO),
		O is OO+1
	); O is 0.

checkLeft(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		XX is X-1,
		checkLeft(P, NN, XX, Y, OO),
		O is OO+1
	); O is 0.

checkDownRight(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		YY is Y+1,
		XX is X+1,
		checkDownRight(P, NN, XX, YY, OO),
		O is OO+1
	); O is 0. 

checkTopLeft(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		XX is X-1,
		YY is Y-1,
		checkTopLeft(P, NN, XX, YY, OO),
		O is OO+1
	); O is 0.

checkDownLeft(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		XX is X-1,
		YY is Y+1,
		checkDownLeft(P, NN, XX, YY, OO),
		O is OO+1
	); O is 0.

checkTopRight(P, N, X, Y, O) :- 
	(	
		N < 7,
		stone(P, X, Y),
		NN is N+1,
		YY is Y-1,
		XX is X+1,
		checkTopRight(P, NN, XX, YY, OO),
		O is OO+1
	); O is 0. 

checkAll(P, X, Y) :-
	(
		checkDown(P, 0, X, Y, O1),
		checkTop(P, O1, X, Y, O2),
		O2 > 5
	);(
		checkRight(P, 0, X, Y, O1),
		checkLeft(P, O1, X, Y, O2),
		O2 > 5
	);(
		checkDownRight(P, 0, X, Y, O1),
		checkTopLeft(P, O1, X, Y, O2),
		O2 > 5
	);(
		checkDownLeft(P, 0, X, Y, O1),
		checkTopRight(P, O1, X, Y, O2),
		O2 > 5
	) 
	.

% TESTOVANI END

%Generovani typu startu
genStartStones(R) :-
	( R = 1,
		assert(startStone(11,13)),
		assert(startStone(13,13)),
		assert(startStone(12,12))
		);
	( R = 2,
		assert(startStone(7,13)),
		assert(startStone(7,15)),
		assert(startStone(9,13))
		);
	( R = 3,
		assert(startStone(7,7)),
		assert(startStone(11,7)),
		assert(startStone(11,11)),
		assert(startStone(9,9))
		);
	( R = 4,
		assert(startStone(15,13)),
		assert(startStone(12,10)),
		assert(startStone(14,12)),
		assert(startStone(16,14)),
		assert(startStone(13,11)),
		assert(startStone(11,9))
		).

% nahodna pozice kamene
gen_pos(S, X, Y) :-
	X is random(S) + 1,
	Y is random(S) + 1.

% nahodna volna pozice kamene 
gen_random_free(X, Y) :-
	board_size(S),
	%TODO: check if board is full
	repeat, % odsud opakuj
	gen_pos(S, X, Y), % vygeneruj pozici
	\+ stone(_, X, Y). % test jestli je pozice obsazena

predefMove1(X, Y) :-
	startStone(X, Y),
	retract(startStone(X, Y)).

% tah pro first
move1(X, Y) :-
	(predefMove1(X, Y);
	gen_random_free(X, Y)),
	assert(stone(0, X, Y)), % pridej do db
	updateStoneCount(1).

moveMinmax(X1,Y1,X2,Y2) :- 
	resolve_strategy(OffensiveStrategy, Xc, Yc),
	get_minimax_range(Xc, Yc, 2, Rx1, Ry1, Rx2, Ry2),
	minmax(0, 1, 0, Xc, Yc, OffensiveStrategy, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2),

	assert(stone(0,X1,Y1)),
	assert(stone(0,X2,Y2)),
	updateStoneCount(2)
	.

% tah pro stone
move(X1, Y1, X2, Y2) :-
	(startStone(_,_),
		move1(X1, Y1),
		move1(X2, Y2));
	moveMinmax(X1,Y1,X2,Y2).

% vypis formatovane lajny ze seznamu symbolu
put_line(L) :-
	writef('%s\n', [L]).
	
% start 
start :-
	read_line(L), % nacti lajnu
	(
		atom_codes('FIRST:', AC), % dekoduj zpravu do AC
		append(AC, CS, L), % spoj 2 ratezce a vrat do L
		get_coords1(CS, X, Y), % ziskej koordinaty 
		assert(stone(1, X, Y)), % a pridej do db
		updateStoneCount(1),
		move(X1, Y1, X2, Y2), % proved tah
		write_stones(X1, Y1, X2, Y2) % vypis
	;
		L = "START;", % nacetl jsem L takze zacinam
		move1(X,Y),
		number_codes(X, Sx),
		number_codes(Y, Sy),
		append(["FIRST:", Sx, ",", Sy, ";"], LL),
		put_line(LL) % vypisu
	;
		halt % jinak koncim
	),
	play. % pokracuj hranim
		
play :-
	read_line(L), % nacti radek
	(
		atom_codes('QUIT;', AC), % je konec? 
		L == AC, halt % je konec
	;	
		atom_codes('STONES:', AC), 						% dosel tah soupere STONES do AC
		append(AC, CS, L), 								% udelej mi z toho retezec
		get_coords(CS, X1o, Y1o, X2o, Y2o), 			% koordinaty kamenu
		assert(stone(1, X1o, Y1o)), 					% pridej do db
		assert(stone(1, X2o, Y2o)),
		updateStoneCount(2),
		(retract(startStone(X1o,Y1o));!),
		(retract(startStone(X2o,Y2o));!),
		move(X1, Y1, X2, Y2), 							% hraj
		write_stones(X1, Y1, X2, Y2), 					% vypis
		play 											% a zas znovu
	;
		halt
	).


% ziskej rozmery oblasti
% D je vzdalenost resp. polomer
get_minimax_range(X, Y, D, Rx1, Ry1, Rx2, Ry2) :-
	(
		(% osetreni X-oveho okraje
			X - D < 1,
			Rx1 is 1,
			Rx2 is 2 * D + 1
		) ; (
			X > 19 - D,
			Rx1 is 19 - 2 * D - 1,
			Rx2 is 19
		) ; (
			Rx1 is X - 2,
			Rx2 is X + 2
		)
	) , (
		(% osetreni Y-oveho okraje
			Y - D < 3,
			Ry1 is 1,
			Ry2 is 2 * D + 1
		) ; (
			X > 19 - D,
			Ry1 is 19 - 2 * D - 1,
			Ry2 is 19
		) ; (
			Ry1 is Y - 2,
			Ry2 is Y + 2
		)
	).


% prohleda celou desku a hleda pro daneho hrace maximalni delku rady
% TODO navratova hodnota souradnice maxima
%find_max_in_board(P, X, Y, CMAX, MAX, Xs, Ys) :-
%	checkDown(P, 0, X, Y, D), 
%	checkDownRight(P, 0, X, Y, DR), 
%	checkRight(P, 0, X, Y, R), 
%	checkTopRight(P, 0, X, Y, TR),
%	max_list([CMAX, D, DR, R, TR], MMAX),
%	(
%		(
%			X = 19,
%			Y = 19,
%			MAX is CMAX,
%			Xs is 0,
%			Ys is 0
%		);(
%			(
%				X < 19,
%				Y = 19,
%				XX is X+1,
%				YY is 1,
%				find_max_in_board(P, XX, YY, MMAX, MAX, Xs, Ys)
%			);(
%				XX is X,
%				YY is Y+1,
%				find_max_in_board(P, XX, YY, MMAX, MAX, Xs, Ys)
%			),
%			(
%				(
%					MMAX > CMAX,
%					Xs is X,
%					Ys is Y
%				);!
%			)
%		)
%	).

find_max_in_board(P,Xs,Ys,CMAX,MAX, Xm,Ym,Xo,Yo) :-
	checkDown(P, 0, Xs, Ys, D), 
	checkDownRight(P, 0, Xs, Ys, DR), 
	checkRight(P, 0, Xs, Ys, R), 
	checkTopRight(P, 0, Xs, Ys, TR),
	max_list([CMAX, D, DR, R, TR], MMAX),
	( 
		Ys < 19,
		(
			MMAX >= CMAX,
			find_max_in_board(P,Xs,Ys+1,CMAX,MAX,Xm,Ym,Xo,Yo)
		) ;
		(
			find_max_in_board(P,Xs,Ys+1,MMAX,MAX,Xs,Ys,Xo,Yo)
		)
	);
	( 
		Ys = 19,
		Xs < 19,
		(
			MMAX >= CMAX,
			find_max_in_board(P,Xs+1,1,CMAX,MAX,Xm,Ym,Xo,Yo)
		) ;
		(
			find_max_in_board(P,Xs+1,1,MMAX,MAX,Xs,Ys,Xo,Yo)
		)
	),
	( % dohledano
		Xs = 19,
		Ys = 19,
		( 
			MMAX >= CMAX,
			MAX is MMAX,
			Xo is Xs,
			Yo is Ys	
		); (
			MAX is CMAX,
			Xo is Xm,
			Yo is Ym
		)
	);!.

% minmax
minmax(P1, P2,N, X, Y, Strategy, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2) :-
	% writef('rozmer oblasti pro (%d) X=%d Y=%d    je %d,%d : %d,%d\n', [P, X, Y, Rx1, Ry1, Rx2, Ry2]),
	
	(
		N < 3,
		(Strategy = 1,
			minmax(P2, P1,N + 1, X, Y, 0, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2) 				% P1 and P2 swapped, Strategy negated
		); (
			minmax(P2, P1,N + 1, X, Y, 1, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2)
		)
	);!,
	
	true.


% volba strategie na zaklade stavu desky (kdo ma navrh)
% vede-li souper, zvoli se obrana strategie, jinak utocna
resolve_strategy(OffensiveStrategy, X, Y) :-
	%find_max_in_board(P,Xs,Ys,CMAX,MAX, Xm,Ym,Xo,Yo)
	find_max_in_board(0, 1, 1, 0, 0, DEF_MAX, 0, 0, Xd, Yd),
	find_max_in_board(1, 1, 1, 0, 0, OFF_MAX, 0, 0, Xo, Yo),
	(
		(
			DEF_MAX > OFF_MAX,
			OffensiveStrategy is 0,
			X is Xd,
			Y is Yd

		) ; (
			OffensiveStrategy is 1,
			X is Xo,
			Y is Yo
		)
	).


%entry point
prolog :-
	prompt(_, ''),


	R is random(4) + 1,
	genStartStones(R),
	start.
	

	%number_codes(B, Sn),
	%put_line(Sn).





%% tento soubor můete ve svých projektech libovolně pouít
%% PS: minulý rok bylo jedním studentem vytvořeno grafické rozhraní,
%%     ke staení na https://gist.github.com/izidormatusov/5114798
%% PS2: zejména při pouívání různých knihoven si dávejte dobrý pozor,
%%     zda je poadovaná funkce dostupná na referenčním serveru
%% M. Hyr, 21.3.2014
