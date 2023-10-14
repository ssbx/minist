SHELL = /bin/sh
BISON = bison
FLEX  = flex

.SUFFIXES:
.SUFFIXES: .c .o

CFLAGS    = -Wall -Wextra
ALLCFLAGS = -I. $(CFLAGS)
OBJECTS   = parse.tab.o scan.yy.o main.o


run: minist-compile
	./minist-compile files/Behavior.st

minist-compile: $(OBJECTS)
	$(CC) -o minist-compile $(OBJECTS)

clean:
	$(RM) -f parse.tab.h parse.tab.c scan.yy.h scan.yy.c *.o minist-compile


scan.yy.c scan.yy.y: scan.l parse.tab.h
	$(FLEX) --outfile=scan.yy.c --header-file=scan.yy.h scan.l

parse.tab.c parse.tab.h: parse.y nodes.h
	$(BISON) --color -Wall -Wother -Wcounterexamples -d parse.y


.c.o:
	$(CC) $(CPPFLAGS) -c $(ALLCFLAGS) $<

# cc -MM *.c
main.o: main.c scan.yy.h parse.tab.h nodes.h
parse.tab.o: parse.tab.c nodes.h parse.tab.h
scan.yy.o: scan.yy.c parse.tab.h
