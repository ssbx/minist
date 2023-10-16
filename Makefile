# the default target
run:

# install configuration
prefix     = $(HOME)
bindir     = $(prefix)/bin
libexecdir = $(prefix)/libexec

SHELL = /bin/sh
BISON = bison
FLEX  = flex
CC    = cc
RM    = rm -f

.SUFFIXES:
.SUFFIXES: .c .o

# optional flags that can be altered by the user
CFLAGS    = -Wall -Wextra

# The used cflags containings our mandatory cflags
ALLCFLAGS =
ALLCFLAGS = -I. $(CFLAGS)

# all objects. Clear guard and definition
OBJECTS =
OBJECTS = parse.tab.o scan.yy.o main.o

.PHONY: run clean

run: minist-img-gen
	./minist-img-gen files/Test.st

minist-img-gen: $(OBJECTS)
	$(CC) -o minist-img-gen $(OBJECTS)

clean:
	$(RM) parse.tab.h parse.tab.c scan.yy.h scan.yy.c *.o minist-img-gen

scan.yy.c scan.yy.y: scan.l parse.tab.h
	$(FLEX) --outfile=scan.yy.c --header-file=scan.yy.h scan.l

parse.tab.c parse.tab.h: parse.y nodes.h
	$(BISON) --color -Wall -Wother -Wcounterexamples -d parse.y

.c.o:
	$(CC) $(CPPFLAGS) -c $(ALLCFLAGS) $<

# Do not edit bellow >>>. Delete the last target definition lines and run
# $ cc -MM *.c >> Makefile
# >>>
main.o: main.c scan.yy.h parse.tab.h nodes.h
parse.tab.o: parse.tab.c nodes.h parse.tab.h
scan.yy.o: scan.yy.c parse.tab.h

