all::

.POSIX:
.SUFFIXES: .c .o

prefix     = $(HOME)
bindir     = $(prefix)/bin

SHELL = /bin/sh
BISON = bison
FLEX  = flex
RM    = rm -f

CFLAGS     = -std=c99 -g -Wall -Wextra -Wpedantic
BISONFLAGS = --color -Wall -Wother -Wcounterexamples
FLEXFLAGS  = --outfile=scan.yy.c --header-file=scan.yy.h

OBJS  = parse.tab.o scan.yy.o comp.o pprint.o bytecodes.o utils.o \
        mem.o minist-img-gen.o

minist-img-gen: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o minist-img-gen

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

parse.tab.c parse.tab.h: parse.y nodes.h
	$(BISON) $(BISONFLAGS) -d parse.y

scan.yy.c scan.yy.h: scan.l parse.tab.h
	$(FLEX) $(FLEXFLAGS) scan.l

include depends.mk

.PHONY: t run clean depends check all install uninstall
all:: run

run: minist-img-gen
	./minist-img-gen files/test.st

clean:
	$(RM) minist-img-gen *.o parse.tab.* scan.yy.* tests/*.expect.out

depends: scan.yy.h parse.tab.h
	$(CC) -MM *.c > depends.mk

check: minist-img-gen
	./tests/run.sh

install: minist-img-gen
	install -m755 minist-img-gen $(bindir)/minist-img-gen
uninstall:
	rm -f $(bindir)/minist-img-gen
