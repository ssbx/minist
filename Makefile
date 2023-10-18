.POSIX:
.SUFFIXES: .c .o

prefix     = $(HOME)
bindir     = $(prefix)/bin

SHELL = /bin/sh
BISON = bison
FLEX  = flex
RM    = rm -f

CFLAGS     = -I. -std=c99 -g -Wall -Wextra -Wpedantic
BISONFLAGS = --color -Wall -Wother -Wcounterexamples
FLEXFLAGS  = --outfile=scanner.yy.c --header-file=scanner.yy.h

OBJS  = parser.tab.o scanner.yy.o compiler.o pprinter.o bytecodes.o \
        minist-img-gen.o

minist-img-gen: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o minist-img-gen

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

parser.tab.c parser.tab.h: parser.y nodes.h
	$(BISON) $(BISONFLAGS) -d parser.y

scanner.yy.c scanner.yy.h: scanner.l parser.tab.h
	$(FLEX) $(FLEXFLAGS) scanner.l

include depends.mk

.PHONY: run clean depends check
run: minist-img-gen
	./minist-img-gen files/Test.st

clean:
	$(RM) minist-img-gen *.o parser.tab.* scanner.yy.*

depends: scanner.yy.h parser.tab.h
	$(CC) -MM *.c > depends.mk

check: minist-img-gen
	./tests/run.sh

