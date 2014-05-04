% Authors: 
% Marek Surovic, xsurov03
% Petr Stodulka, xstodu05
% Igor Pavlu, xpavlu06
% Miroslav Paulik, xpauli00

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
  DT is TS + T - 0.1,
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
		% assert(startStone(7,15)),
		assert(startStone(7,17)),
		% assert(startStone(9,13))
		assert(startStone(7,14))
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
	retract(startStone(X, Y)),
	\+ stone(_, X ,Y).

% tah pro first
move1(X, Y) :-
	(
		predefMove1(X, Y);
		find_max_in_board(1, 1, 1, 1,1,19,19, 0, _, X, Y)
	),
	assert(stone(0, X, Y)), % pridej do db
	updateStoneCount(1).

moveMinmax(X1,Y1,X2,Y2) :- 
	resolve_strategy(OffensiveStrategy, Xc, Yc),
	get_minimax_range(Xc, Yc, 19, Rx1, Ry1, Rx2, Ry2),
	minmax(0, 1, Xc, Yc, OffensiveStrategy, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2),

	assert(stone(0,X1,Y1)),
	assert(stone(0,X2,Y2)),
	updateStoneCount(2)
	.

% tah pro stone
move(X1, Y1, X2, Y2) :-
	(
		startStone(_,_),
		move1(X1, Y1),
		move1(X2, Y2)
	); (
		moveMinmax(X1,Y1,X2,Y2)
	
	).

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
		%print2,
		move(X1, Y1, X2, Y2), 							% hraj
		write_stones(X1, Y1, X2, Y2), 					% vypis
		%print2,
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

checkCLosedRow(X, Y, N, DX, DY, NN, Xo, Yo) :-
	(
		XX is X + DY * N,
		YY is Y + DX * N,
		XX > 0,
		XX < 20,
		YY > 0,
		YY < 20,
		\+ stone(_, XX ,YY),
		NN is N,
		Xo is XX,
		Yo is YY
	);(
		XX is X - DY,
		YY is Y - DX,
		XX > 0,
		XX < 20,
		YY > 0,
		YY < 20,
		\+ stone(_, XX ,YY),
		NN is N,
		Xo is XX,
		Yo is YY
	);(
		NN is 0,
		Xo is 0,
		Yo is 0
	).


find_max_in_board(P,Xs,Ys,BXs,BYs,BXe,BYe,CMAX,MAX, Xm,Ym) :-
	(
		(
			stone(P,Xs,Ys),

			checkDown(P, 0, Xs, Ys, D), 
			checkCLosedRow(Xs,Ys, D, 1, 0, ND, Xn1, Yn1),

			checkDownRight(P, 0, Xs, Ys, DR), 
			checkCLosedRow(Xs,Ys, DR, 1, 1, NDR, Xn2, Yn2),

			checkRight(P, 0, Xs, Ys, R), 
			checkCLosedRow(Xs,Ys, R, 0, 1, NR, Xn3, Yn3),

			checkTopRight(P, 0, Xs, Ys, TR),
			checkCLosedRow(Xs,Ys, TR, -1, 1, NTR, Xn4, Yn4),

			max_list([CMAX, ND, NDR, NR, NTR], MMAX)
		) ; 
		(
			MMAX is CMAX,
			ND is 0,
			NDR is 0,
			NR is 0,
			NTR is 0,
			Xn1 is BXs,
			Xn2 is BXs,
			Xn3 is BXs,
			Xn4 is BXs,
			Yn1 is BYs,
			Yn2 is BYs,
			Yn3 is BYs,
			Yn4 is BYs
		)
	),
	XXs is Xs + 1,
	YYs is Ys+1,
	(
		( % dohledano
			Xs = BXe,
			Ys = BYe,
			NEW_MAX is 0,
			NEW_Xm is BXs,
			NEW_Ym is BYs
		);( 
			Ys < BYe,
			find_max_in_board(P,Xs,YYs,BXs,BYs,BXe,BYe,MMAX,NEW_MAX,NEW_Xm,NEW_Ym)
		);
		( 
			Ys = BYe,
			Xs < BXe,
			find_max_in_board(P,XXs,BYs,BXs,BYs,BXe,BYe,MMAX,NEW_MAX,NEW_Xm,NEW_Ym)
		)
	),
	(
		(
			NEW_MAX > MMAX,
			MAX is NEW_MAX,
			Xm is NEW_Xm,
			Ym is NEW_Ym
		);(
			MMAX = ND,
			MAX is MMAX,
			Xm is Xn1,
			Ym is Yn1
		);(
			MMAX = NDR,
			MAX is MMAX,
			Xm is Xn2,
			Ym is Yn2
		);(
			MMAX = NR,
			MAX is MMAX,
			Xm is Xn3,
			Ym is Yn3
		);(
			MMAX = NTR,
			MAX is MMAX,
			Xm is Xn4,
			Ym is Yn4
		);(
			MAX is MMAX,
			Xm is BXs,
			Ym is BYs
		)
	)
	;!.



minmax(P1, P2, X, Y, Strategy, Rx1, Ry1, Rx2, Ry2, X1,Y1,X2,Y2) :-
	
	% NextStrategy = 1 - Strategy, 			% Zmena strategie 
	assert(stone(P1, X, Y)),
	(
		(
			Strategy = 1,
			find_max_in_board(P2, 1, 1, Rx1, Ry1, Rx2, Ry2, 0, _, Xd, Yd),
			assert(stone(P1,Xd, Yd))
		); (
			find_max_in_board(P1, 1, 1, Rx1, Ry1, Rx2, Ry2, 0, _, Xd, Yd),
			assert(stone(P1,Xd, Yd))
		)
	),
	
	retract(stone(P1, X, Y)),
	retract(stone(P1, Xd, Yd)),
	X1 is X,
	Y1 is Y,
	X2 is Xd,
	Y2 is Yd
	.


% volba strategie na zaklade stavu desky (kdo ma navrh)
% vede-li souper, zvoli se obrana strategie, jinak utocna
resolve_strategy(OffensiveStrategy, X, Y) :-
	find_max_in_board(0, 1, 1,1,1,19,19, 0, DEF_MAX, Xd, Yd),
	find_max_in_board(1, 1, 1,1,1,19,19, 0, OFF_MAX, Xo, Yo),
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

print2 :-
    findall((A,B,C), stone(A,B,C), Facts),
    maplist(writeln, Facts).


%entry point
prolog :-
	prompt(_, ''),


	R is random(4) + 1,
	genStartStones(R),
	start.
