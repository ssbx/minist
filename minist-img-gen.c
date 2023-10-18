#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "parse.tab.h"
#include "comp.h"
#include "pprint.h"
#include "scan.yy.h"
#include "nodes.h"

extern struct ClassFile *parsed_file;
char* fname;

void yyerror(char *err) {
    fprintf(stderr, "%s:%i %s\n", fname, yylineno, err);
}

/* yywrap, called by the lexer on EOF. If returns 0, asume yyin point to
another file. condition remains inchanged (no revert to initial) */
int yywrap() {
    return 1;
}

int
main(int argc, char * argv[])
{
    int   opt_parseOnly = 0;

    if (argc < 1) {
        printf( "nedd file to read\n");
        return 1;
    }

    if (strcmp(argv[1], "-p") == 0) {
        opt_parseOnly = 1;
        if (argc < 2) {
            printf( "nedd file to read\n");
            return 1;
        }
        fname = argv[2];
    } else {
        fname = argv[1];
    }


    FILE *f = fopen(fname, "r");
    if (!f) {
        perror(argv[1]);
        return 1;
    }
    fname = argv[1];

    yyin = f;

    int err = yyparse();

    fclose(f);

    if (err) {
        return 1;
    }

    if (!opt_parseOnly)
        compiler_compile(parsed_file);
    pprinter_print(parsed_file);

    return 0;
}

