bytecodes.o: bytecodes.c bytecodes.h parser.tab.h
compiler.o: compiler.c compiler.h nodes.h bytecodes.h parser.tab.h
minist-img-gen.o: minist-img-gen.c parser.tab.h compiler.h nodes.h \
 pprinter.h scanner.yy.h
parser.tab.o: parser.tab.c nodes.h parser.tab.h
pprinter.o: pprinter.c pprinter.h nodes.h bytecodes.h
scanner.yy.o: scanner.yy.c parser.tab.h
