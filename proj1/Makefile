######################################################
## Project: Simple Grep
## Authors:
##   xsurov03 - Marek Surovic
##   xstodu05 - Petr Stodulka
##   xpavlu06 - Igor Pavlu
##   xpauli00 - Miroslav Paulik
######################################################

SOURCE_FILES=DFA.hs Interpreter.hs MIN.hs NFA.hs Parser.hs xsurov03.hs
NAME=xsurov03

$(NAME):
	ghc $(NAME).hs

clean:
	rm -f *.hi *.o *$(NAME).tgz $(NAME) 

pack: clean
	tar -vzcf flp-fun-xsurov03.tgz $(SOURCE_FILES) Makefile README rozdeleni

