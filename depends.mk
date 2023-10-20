bytecodes.o: bytecodes.c bytecodes.h parse.tab.h
comp.o: comp.c comp.h nodes.h utils.h bytecodes.h parse.tab.h
mem.o: mem.c mem.h
minist-img-gen.o: minist-img-gen.c parse.tab.h comp.h nodes.h pprint.h \
 scan.yy.h
parse.tab.o: parse.tab.c nodes.h utils.h parse.tab.h
pprint.o: pprint.c pprint.h nodes.h bytecodes.h
scan.yy.o: scan.yy.c parse.tab.h
utils.o: utils.c utils.h
