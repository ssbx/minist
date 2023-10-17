# the default target
all::

# install configuration
prefix     = $(HOME)
bindir     = $(prefix)/bin
libexecdir = $(prefix)/libexec

SHELL = /bin/sh
PROG  = minist-img-gen
BISON = bison
FLEX  = flex
CC    = cc
RM    = rm -f

.SUFFIXES:
.SUFFIXES: .c .o

# optional CFLAGS that can be altered by the user
CFLAGS    = -Wall -Wextra
ALLCFLAGS =
ALLCFLAGS = -I. $(CFLAGS)

# optional BISONFLAGS that can be altered by the user
BISONFLAGS = --color -Wall -Wother -Wcounterexamples
ALLBISONFLAGS =
ALLBISONFLAGS = $(BISONFLAGS)

# optional FLEXFLAGS that can be altered by the user
FLEXFLAGS =
ALLFLEXFLAGS =
ALLFLEXFLAGS = --outfile=scanner.yy.c --header-file=scanner.yy.h $(FLEXFLAGS)

# all objects. Clear guard and definition
OBJECTS =
OBJECTS = parser.tab.o scanner.yy.o compiler.o pprinter.o $(PROG).o

# cleanfiles
CLEANFILES =
CLEANFILES = $(PROG) *.o parser.tab.h parser.tab.c scanner.yy.h scanner.yy.c

include depends.mk

.PHONY: run clean depends

all:: run

run: $(PROG)
	./$(PROG) files/Test.st

$(PROG): $(OBJECTS)
	$(CC) -o $(PROG) $(OBJECTS)

clean:
	$(RM) $(CLEANFILES)

scanner.yy.c scanner.yy.h: scanner.l parser.tab.h
	$(FLEX) $(ALLFLEXFLAGS) scanner.l

parser.tab.c parser.tab.h: parser.y nodes.h
	$(BISON) $(ALLBISONFLAGS) -d parser.y

.c.o:
	$(CC) $(CPPFLAGS) -c $(ALLCFLAGS) $<

depends: scanner.yy.h parser.tab.h
	$(CC) -MM *.c > depends.mk

