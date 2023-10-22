#include "pprint.h"
#include "bytecodes.h"
#include <stdio.h>

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

/* TODO this share the same thing as the method body printing:
 * - temps
 * - expressions with assignment
 * - returns
 */
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
                printf( " %s", umsg->msg);
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
        case ST_SYMBOL:
            printf(" #%s", e->u.symbol.value);
            break;
        default:
            printf("EEEEEEEEEEEEEEEeeeeeeeeeerrror\n");
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

    if (m->bytecodes) {
        printf("    \"\n");
        int needsExtent = 0;
        unsigned char ext;
        for (int i = 0; i < m->bytecount; i++) {
            if (needsExtent) {
                printf("    <%i, %i> %s\n", ext,
                        m->bytecodes[i],
                        bytecodes_getBytecodeDescription(ext));
                needsExtent = 0;
                ext = 0;
                continue;
            }
            if (bytecodes_needsExtent(m->bytecodes[i])) {
                needsExtent = 1;
                ext = m->bytecodes[i];
            } else {
                printf("    <%i> %s\n",
                        m->bytecodes[i],
                        bytecodes_getBytecodeDescription(m->bytecodes[i]));
            }
        }
        if (m->literal_frame) {
            printf("    Literal Frame:\n");
            for (int i = 0; i < m->literal_frame_count; i++) {
                printf("        %s\n", m->literal_frame[i]);
            }
        }
        printf("    \"\n");
    }

    printf( " ! ");
}

void pprinter_print(struct ClassFile *cf) {
    struct ClassHeader *h = cf->header;
    printf( "-----\n");
    printf( "%s subclass: #%s\n", h->super, h->className);
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
        printf( "!%s methodsFor: '%s'!\n", m->classname, m->name);
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
            printf( "!%s class methodsFor: '%s'!\n\n", mmm->classname, mmm->name);
            struct Method* mmmet = mmm->methods;
            while (mmmet) {
                print_method(mmmet);
                mmmet = mmmet->next;
            }
            printf( "!\n");
            mmm = mmm->next;
        }
    }
    if (cf->init) {
        struct ExprUnit *receiver = cf->init->u.unary.receiver;
        struct UnaryMsg *msg = cf->init->u.unary.msgs;
        printf("\n\n%s %s!\n", receiver->u.id.name, msg->msg);
    }

    printf( "\n");
}
