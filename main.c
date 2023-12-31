#include <stdio.h>
#include "scan.yy.h"
#include "parse.tab.h"
#include "nodes.h"

extern struct ClassFile *parsed_file;

char *fname;

void release(struct ClassHeader *h) {
     free(h->super);
     free(h->className);
     free(h->instsVarNames);
     free(h->classVarNames);
     free(h->poolDict);
     free(h->category);
     free(h);
}

void yyerror(char *err) {
    fprintf(stderr, "%s:%i %s\n", fname, yylineno, err);
}

/* yywrap, called by the lexer on EOF. If returns 0, asume yyin point to
another file. condition remains inchanged (no revert to initial) */
int yywrap() {
    return 1;
}

void print_eval(struct ExprUnit *e);
void print_cas(struct Cascade *c) {
    printf( " ; ");
    struct BinaryMsg *b;
    struct KeywordMsg *k;
    switch (c->type) {
        case CASCADE_UNARY:
            print_eval(c->unary.msg);
            break;
        case CASCADE_BINARY:
            b = c->binary.msg;
            printf( " %c ", b->op);
            print_eval(b->arg);
            break;
        case CASCADE_KEYWORD:
            k = c->keyword.msg;
            while(k) {
                printf( " %s ", k->key);
                print_eval(k->arg);
                k = k->next;
            }
            break;
    }
}
void print_exp(struct ExprUnit* exp) {
    while (exp) {
        if (exp->returns) printf( " ^ ");
        else if (exp->assignsTo)  {
            print_eval(exp->assignsTo);
            printf( " := ");
        }
        print_eval(exp);
        printf( " .");
        exp = exp->next;
    }
}

int is_const = 1;

void print_eval(struct ExprUnit *e) {
    struct KeywordMsg *kmsg;
    struct Cascade *cas;
    struct ExprUnit *eu;
    struct Temp *tps;
    struct BlockArg *ba;
    switch (e->type) {
        case ST_ID:
            printf( " %s ", e->id.name);
            break;
        case ST_UNARY:
            printf( "(");
            print_eval(e->unary.receiver);
            struct UnaryMsg *umsg = e->unary.msgs;
            while (umsg) {
                printf( " %s", umsg->msg->id.name);
                umsg = umsg->next;
            }
            cas = e->cascade;
            while (cas) {
                print_cas(cas);
                cas = cas->next;
            }
            printf( ")");
            break;
        case ST_BINARY:
            printf( "(");
            print_eval(e->binary.receiver);
            struct BinaryMsg *bmsg = e->binary.msgs;
            while(bmsg) {
                printf( " %c ", bmsg->op);
                print_eval(bmsg->arg);
                bmsg = bmsg->next;
            }
            cas = e->cascade;
            while (cas) {
                print_cas(cas);
                cas = cas->next;
            }

            printf( ")");
            break;
        case ST_KEYWORD:
            printf( "(");
            print_eval(e->keyword.receiver);
            kmsg = e->keyword.msgs;
            while (kmsg) {
                printf( " %s ", kmsg->key);
                print_eval(kmsg->arg);
                kmsg = kmsg->next;
            }
            cas = e->cascade;
            while (cas) {
                print_cas(cas);
                cas = cas->next;
            }

            printf( ")");
            break;
        case ST_BLOCK:
            printf( "[");
            if (e->block.args) {
                ba = e->block.args;
                while (ba) {
                    printf( " %s", ba->name);
                    ba = ba->next;
                }
                printf( " |");
            }
            if (e->block.temps) {
                printf( " | ");
                tps = e->block.temps;
                while(tps) {
                    print_eval(tps->name);
                    tps = tps->next;
                }
                printf( " | ");
            }
            if (e->block.exprs)
                print_exp(e->block.exprs);
            printf( "]");
            break;
        case ST_STRING:
            printf( "'%s'", e->string.value);
            break;
        case ST_CHAR:
            printf( "%s", e->character.value);
            break;
        case ST_INT:
            printf( " %s ", e->integer.value);
            break;
        case ST_ARRAY:
            printf( "(");

            eu = e->array.head;
            while (eu) {
                print_eval(eu);
                printf( " ");
                eu = eu->next;
            }
            printf( ")");
            break;

        case ST_ARRAYCONST:
            printf( "#");

            if (e->array.head) print_eval(e->array.head);
            break;

    }
}

void print_method(struct Method* m) {
    printf( "\n\n");
    struct KeywordMsg *kw;
    switch(m->def->type) {
        case ST_UNARY:
            printf( "%s\n", m->def->unary->id.name);
            break;
        case ST_BINARY:
            printf( "%c %s\n", m->def->binary, m->def->arg->id.name);
            break;
        case ST_KEYWORD:
            kw = m->def->keys;
            while (kw) {
                printf( "%s %s ", kw->key, kw->arg->id.name);
                kw = kw->next;
            }
            printf( "\n");
            break;
    }
    if (m->prim) {
        printf( "    <primitive: %s>\n", m->prim->integer.value);
    }
    if (m->temps) {
        struct Temp *t = m->temps;
        printf("    |");
        while(t) {
            printf( " %s", t->name->id.name);
            t = t->next;
        }
        printf(" |\n");
    }
    if (m->exprs) {
        struct ExprUnit *exp = m->exprs;
        while (exp) {
            printf( "    ");
            if (exp->returns) printf( "^ ");
            else if (exp->assignsTo)  {
                print_eval(exp->assignsTo);
                printf( " := ");
            }
            print_eval(exp);
            printf( " .\n");
            exp = exp->next;
        }
    }
    printf( " ! ");
}

int main(int argc, char * argv[])
{
    if (argc < 1) {
        printf( "nedd file to read\n");
        return 1;
    }

    FILE *f = fopen(argv[1], "r");
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

    struct ClassHeader *h = parsed_file->header;
    printf( "-----\n");
    printf( "%s subclass: %s\n", h->super->id.name, h->className->symbol.value);
    printf( "\tinstanceVariableNames: '%s'\n", h->instsVarNames);
    printf( "\tclassVariableNames: '%s'\n", h->classVarNames);
    printf( "\tpooldictionary: '%s'\n", h->poolDict);
    printf( "\tcategory: '%s'!\n", h->category);

    printf( "\n");
    struct ClassComment *c = parsed_file->comment;
    printf( "%s comment: '%s'!\n", c->className->id.name, c->comment);

    printf( "\n");
    struct MethodCategory *m = parsed_file->categories;
    while (m) {
        printf( "!%s methodsFor: '%s'!\n", m->classname->id.name, m->name);
        printf( "\n");
        struct Method *met = m->methods;
        while(met) {
            print_method(met);
            met = met->next;
        }
        m = m->next;
        printf( "!\n\n");
    }
    if (parsed_file->classHeader) {
        printf( "\"= = = == = \"!\n");
        struct ClassClassHeader *cch = parsed_file->classHeader;
        printf( "%s class instanceVariableNames: '%s'!\n\n",
                cch->className->id.name, cch->instVarNames);
        struct MethodCategory * mmm = parsed_file->classCategories;
        while (mmm) {
            printf( "!%s class methodsFor: '%s'!\n\n", mmm->classname->id.name, mmm->name);
            struct Method* mmmet = mmm->methods;
            while (mmmet) {
                print_method(mmmet);
                mmmet = mmmet->next;
            }
            printf( "!\n");
            mmm = mmm->next;
        }
    }
    printf( "\n");
    return 0;
}

