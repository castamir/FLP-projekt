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

% velikost plochy
board_size(19).

% POCATECNICH 9 KAMENU
%     0
%   0   0
% 0   0   0
%   0   0
%     0

startStone(9,11).
startStone(10,10).
startStone(10,12).
startStone(11,9).
startStone(11,11).
startStone(11,13).
startStone(12,10).
startStone(12,12).
startStone(13,11).

% POCATECNI KAMENY END

% odehrane kameny 
stonesPlayed(0). % max 361

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
checkDown(P, N, X, Y) :- 
	(	
		number_codes(N, Sn),
		put_line(Sn),
		N < 6,
		X < 20,
		stone(P, X, Y),
		NN is N + 1,
		checkDown(P, NN, X + 1, Y),
		N is NN
	);N is N.

checkTop(P, N, X, Y) :- 
	(	
		N < 6,
		X > 0,
		stone(P, X, Y),
		NN is N + 1,
		checkTop(P, NN, X - 1, Y),
		N is NN
	);!.

checkRight(P, N, X, Y) :- 
	(	
		N < 6,
		Y < 20,
		stone(P, X, Y),
		NN is N + 1,
		checkRight(P, NN, X, Y + 1),
		N is NN
	);!.

checkLeft(P, N, X, Y) :- 
	(	
		N < 6,
		Y > 0,
		stone(P, X, Y),
		NN is N + 1,
		checkLeft(P, NN, X, Y - 1),
		N is NN
	);!.

checkDownRight(P, N, X, Y) :- 
	(	
		N < 6,
		X < 20,
		Y < 20,
		stone(P, X, Y),
		NN is N + 1,
		checkDownRight(P, NN, X + 1, Y + 1),
		N is NN
	);!.

checkTopLeft(P, N, X, Y) :- 
	(	
		N < 6,
		X > 0,
		Y > 0,
		stone(P, X, Y),
		NN is N + 1,
		checkTopLeft(P, NN, X - 1, Y - 1),
		N is NN
	);!.

checkDownLeft(P, N, X, Y) :- 
	(	
		N < 6,
		X < 20,
		Y > 0,
		stone(P, X, Y),
		NN is N + 1,
		checkDownLeft(P, NN, X + 1, Y - 1),
		N is NN
	);!.

checkTopRight(P, N, X, Y) :- 
	(	
		N < 6,
		X > 0,
		Y < 20,
		stone(P, X, Y),
		NN is N + 1,
		checkTopRight(P, NN, X - 1, Y + 1),
		N is NN
	);!.

checkAll(P, X, Y) :-
	(
		N is 1,
		checkDown(P, N, X, Y),
		checkTop(P, N, X, Y),
		N = 6
	) ;
	(
		N is 1,
		checkRight(P, N, X, Y),
		checkLeft(P, N, X, Y),
		N = 6
	) ;
	(
		N is 1,
		checkDownRight(P, N, X, Y),
		checkTopLeft(P, N, X, Y),
		N = 6

	) ;
	(
		N is 1,
		checkDownLeft(P, N, X, Y),
		checkTopRight(P, N, X, Y),
		N = 6
	).

% TESTOVANI END

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
	Z = 1,
	updateStoneCount(Z).

% tah pro stone
move(X1, Y1, X2, Y2) :-
	move1(X1, Y1),
	move1(X2, Y2).

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
		atom_codes('STONES:', AC), % dosel tah soupere STONES do AC
		append(AC, CS, L), % udelej mi z toho retezec
		get_coords(CS, X1o, Y1o, X2o, Y2o), % koordinaty kamenu
		assert(stone(1, X1o, Y1o)), % pridej do db
		assert(stone(1, X2o, Y2o)), 
		(checkAll(1, X1o, Y1o), LL = "QUIT;", put_line(LL), halt;!),
		(checkAll(1, X2o, Y2o), LL = "QUIT;", put_line(LL), halt;!),
		updateStoneCount(2),
		(retract(startStone(X1o,Y1o));!),
		(retract(startStone(X2o,Y2o));!),
		move(X1, Y1, X2, Y2), % hraj
		write_stones(X1, Y1, X2, Y2), % vypis
		(checkAll(0, X1, Y1), LL = "QUIT;", put_line(LL), halt;!),
		(checkAll(0, X2, Y2), LL = "QUIT;", put_line(LL), halt;!),
		play % a zas znovu
	;
		halt
	).

%entry point
prolog :-
	prompt(_, ''),
	start.





%% tento soubor můete ve svých projektech libovolně pouít
%% PS: minulý rok bylo jedním studentem vytvořeno grafické rozhraní,
%%     ke staení na https://gist.github.com/izidormatusov/5114798
%% PS2: zejména při pouívání různých knihoven si dávejte dobrý pozor,
%%     zda je poadovaná funkce dostupná na referenčním serveru
%% M. Hyr, 21.3.2014