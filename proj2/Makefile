######################################################
## Project: Simple Grep
## Authors:
##   xsurov03 - Marek Surovic
##   xstodu05 - Petr Stodulka
##   xpavlu06 - Igor Pavlu
##   xpauli00 - Miroslav Paulik
######################################################

SOURCE_FILES=DFA.hs Interpreter.hs MIN.hs NFA.hs Parser.hs xsurov03.hs
SRC=connect6
NAME=xsurov03

$(NAME):
	swipl -q -g prolog -o $(NAME) -c $(SRC).pl

clean:
	rm -f *.hi *.o *$(NAME).tgz $(NAME) *.exe

pack: clean
	tar -vzcf flp-fun-xsurov03.tgz $(SOURCE_FILES) Makefile README rozdeleni

