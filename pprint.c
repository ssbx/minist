#include "pprint.h"
#include "bytecodes.h"
#include <stdio.h>



/*
static char * str_from_enum(int e) {
    switch (e) {
        case ST_ID: return "ST_ID";
        case ST_UNIT: return "ST_UNIT";
        case ST_BINARY: return "ST_BINARY";
        case ST_UNARY: return "ST_UNARY";
        case ST_KEYWORD: return "ST_KEYWORD";
        case ST_BLOCK: return "ST_BLOCK";
        case ST_STRING: return "ST_STRING";
        case ST_CHAR: return "ST_CHAR";
        case ST_INT: return "ST_INT";
        case ST_SYMBOL: return "ST_SYMBOL";
        case ST_ARRAY: return "ST_ARRAY";
        case ST_ARRAYCONST: return "ST_ARRAYCONST";
    }
    return "NOT KNOWN";
}
*/

static void print_eval(struct ExprUnit *e);
static void print_cas(struct Cascade *c) {
    printf( " ; ");
    struct BinaryMsg *b;
    struct KeywordMsg *k;
    switch (c->type) {
        case CASCADE_UNARY:
            printf(" %s ", c->u.unary);
            break;
        case CASCADE_BINARY:
            b = c->u.binary;
            printf( " %c ", b->op);
            print_eval(b->arg);
            break;
        case CASCADE_KEYWORD:
            k = c->u.keyword;
            while(k) {
                printf( " %s ", k->key);
                print_eval(k->arg);
                k = k->next;
            }
            break;
    }
}

/* what ois this?? TODO remove print_exp */
static void print_exp(struct ExprUnit* exp) {
    while (exp) {
        if (exp->returns) printf( " ^ ");
        if (exp->assignsTo)  {
            struct ExprUnit *assign = exp->assignsTo;
            while (assign) {
                print_eval(assign);
                printf( " := ");
                assign = assign->next;
            }
        }
        print_eval(exp);
        printf( " .");
        exp = exp->next;
    }
}

int is_const = 1;

static void print_eval(struct ExprUnit *e) {
    struct KeywordMsg *kmsg;
    struct Cascade *cas;
    struct ExprUnit *eu;
    struct Temp *tps;
    struct BlockArg *ba;
    switch (e->type) {
        case ST_ID:
            printf( " %s ", e->u.id.name);
            break;
        case ST_UNARY:
            printf( "(");
            print_eval(e->u.unary.receiver);
            struct UnaryMsg *umsg = e->u.unary.msgs;
            while (umsg) {
                printf( " %s", umsg->msg->u.id.name);
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
            print_eval(e->u.binary.receiver);
            struct BinaryMsg *bmsg = e->u.binary.msgs;
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
            print_eval(e->u.keyword.receiver);
            kmsg = e->u.keyword.msgs;
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
            if (e->u.block.args) {
                ba = e->u.block.args;
                while (ba) {
                    printf( " %s", ba->name);
                    ba = ba->next;
                }
                printf( " |");
            }
            if (e->u.block.temps) {
                printf( " | ");
                tps = e->u.block.temps;
                while(tps) {
                    printf(" %s ", tps->name);
                    tps = tps->next;
                }
                printf( " | ");
            }
            if (e->u.block.exprs)
                print_exp(e->u.block.exprs);
            printf( "]");
            break;
        case ST_STRING:
            printf( "'%s'", e->u.string.value);
            break;
        case ST_CHAR:
            printf( "%s", e->u.character.value);
            break;
        case ST_INT:
            printf( " %s ", e->u.integer.value);
            break;
        case ST_ARRAY:
            printf( "(");

            eu = e->u.array.head;
            while (eu) {
                print_eval(eu);
                printf( " ");
                eu = eu->next;
            }
            printf( ")");
            break;

        case ST_ARRAYCONST:
            printf( "#");

            if (e->u.array.head) print_eval(e->u.array.head);
            break;

    }
}

static void print_method(struct Method* m) {
    printf( "\n\n");
    struct KeywordMsg *kw;
    switch(m->def->type) {
        case ST_UNARY:
            printf( "%s\n", m->def->u.unary.name);
            break;
        case ST_BINARY:
            printf( "%c %s\n", m->def->u.binary.op, m->def->u.binary.arg);
            break;
        case ST_KEYWORD:
            kw = m->def->u.keyword.keys;
            while (kw) {
                printf( "%s %s ", kw->key, kw->arg->u.id.name);
                kw = kw->next;
            }
            printf( "\n");
            break;
    }

    if (m->bytecodes) {
        printf("    \"\n");
        for (int i = 0; i < m->bytecount; i++) {
            printf("    <%i> %s\n", m->bytecodes[i], bytecodes_getBytecodeDescription(m->bytecodes[i]));
        }
        printf("    \"\n");
    }


    if (m->prim) {
        printf( "    <primitive: %s>\n", m->prim->u.integer.value);
    }

    if (m->temps) {
        struct Temp *t = m->temps;
        printf("    |");
        while(t) {
            printf( " %s", t->name);
            t = t->next;
        }
        printf(" |\n");
    }

    if (m->exprs) {
        struct ExprUnit *exp = m->exprs;
        while (exp) {
            printf( "    ");
            if (exp->returns) printf( "^ ");
            if (exp->assignsTo)  {
                struct ExprUnit *assign = exp->assignsTo;
                while (assign) {
                    print_eval(assign);
                    printf( " := ");
                    assign = assign->next;
                }
            }
            print_eval(exp);
            printf( " .\n");
            exp = exp->next;
        }
    }
    printf( " ! ");
}

void pprinter_print(struct ClassFile *cf) {
    struct ClassHeader *h = cf->header;
    printf( "-----\n");
    printf( "%s subclass: %s\n", h->super, h->className);
    printf( "\tinstanceVariableNames: '");
    int i;
    for (i=0; i < h->instsVarNamesCount; i++) {
        printf(" %s ", h->instsVarNames[i]);
    }
    printf( "'\n");

    printf( "\tclassVariableNames: '");
    for (i=0; i< h->classVarNamesCount; i++) {
        printf(" %s ", h->classVarNames[i]);
    }
    printf("'\n");

    printf( "\tpoolDictionaries: '");
    for (i=0; i < h->poolDictsCount; i++) {
        printf(" %s ", h->poolDicts[i]);
    }
    printf("'\n");
    printf( "\tcategory: '%s'!\n", h->category);

    printf( "\n");
    printf( "%s comment: '%s'!\n", cf->header->className, cf->comment);

    printf( "\n");
    struct MethodCategory *m = cf->categories;
    while (m) {
        printf( "!%s methodsFor: '%s'!\n", m->classname->u.id.name, m->name);
        printf( "\n");
        struct Method *met = m->methods;
        while(met) {
            print_method(met);
            met = met->next;
        }
        m = m->next;
        printf( "!\n\n");
    }
    if (cf->classHeader) {
        printf( "\"= = = == = \"!\n");
        struct ClassClassHeader *cch = cf->classHeader;
        printf( "%s class instanceVariableNames: '", cch->className);
        for (int x= 0; x < cch->instVarNamesCount; x++) {
            printf(" %s ", cch->instVarNames[x]);
        }
        printf( "' !\n");

        struct MethodCategory * mmm = cf->classCategories;
        while (mmm) {
            printf( "!%s class methodsFor: '%s'!\n\n", mmm->classname->u.id.name, mmm->name);
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
}
