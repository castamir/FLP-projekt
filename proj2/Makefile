######################################################
## Project: Simple Grep
## Authors:
##   xsurov03 - Marek Surovic
##   xstodu05 - Petr Stodulka
##   xpavlu06 - Igor Pavlu
##   xpauli00 - Miroslav Paulik
######################################################

SOURCE_FILES=connect6.pl
SRC=connect6
NAME=xsurov03

$(NAME):
	swipl -q -g prolog -o $(NAME) -c $(SRC).pl

clean:
	rm -f *.hi *.o *$(NAME).tgz $(NAME) *.exe

pack: clean
	tar -vzcf flp-log-xsurov03.tgz $(SOURCE_FILES) Makefile README rozdeleni

