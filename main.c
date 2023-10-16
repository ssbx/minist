#include <stdio.h>
#include <assert.h>
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

char * str_from_enum(int e) {
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

void yyerror(char *err) {
    fprintf(stderr, "%s:%i %s\n", fname, yylineno, err);
}

/* yywrap, called by the lexer on EOF. If returns 0, asume yyin point to
another file. condition remains inchanged (no revert to initial) */
int yywrap() {
    return 1;
}

void compile_method(struct ClassHeader*, struct Method*);
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

void pretty_print() {
    struct ClassHeader *h = parsed_file->header;
    printf( "-----\n");
    printf( "%s subclass: %s\n", h->super->id.name, h->className->symbol.value);
    printf( "\tinstanceVariableNames: '");
    int i;
    for (i=0; i < h->instsVarNamesCount; i++) {
        printf(" %s ", h->instsVarNames[i]);
    }
    printf( "'\n");
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
            compile_method(h, met);
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
}

int
main(int argc, char * argv[])
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

    pretty_print();
    return 0;
}

void
print_expr_bytes(struct ExprUnit *e)
{
    printf("have one of type %s\n", str_from_enum(e->type));
    printf("push stack 0 is %s\n", e->binary.receiver->id.name);
    printf("psuh stack 1 is %s\n", e->binary.msgs->arg->id.name);
    printf("send op is %c\n", e->binary.msgs->op);
    struct BinaryMsg *m = e->binary.msgs->next;
    if (m) {
        printf("push stack %s\n", m->arg->integer.value);
        printf("send op is %c\n", m->op);
    }
    if (e->returns) {
        printf("returns top of the stack\n");
    }
}

int get_temps(struct Method* m, char*** dest) {
    char **temps = NULL;
    int numTmps = 0;
    if (m->def->type == ST_KEYWORD) {
        struct KeywordMsg *kw = m->def->keys;
        while (kw) {
            numTmps++;
            kw = kw->next;
        }
    }

    if (m->temps) {
        struct Temp *t = m->temps;
        while (t) {
            numTmps++;
            t = t->next;
        }
    }
    if (numTmps)
        temps = malloc(sizeof(char**) * numTmps);

    int tmpid = 0;
    if (m->def->type == ST_KEYWORD) {
        struct KeywordMsg *kw = m->def->keys;
        while (kw) {
            temps[tmpid++] = kw->arg->id.name;
            kw = kw->next;
        }
    }
    if (m->temps) {
        struct Temp *t = m->temps;
        while (t) {
            temps[tmpid++] = t->name->id.name;
            t = t->next;
        }
    }
    *dest = temps;
    return numTmps;
}

int get_instvars(struct ClassHeader *h, char ***v) {
    /* todo recursive super too */
    *v = h->instsVarNames;
    return h->instsVarNamesCount;
}

struct MethodCompileInfo {
    char **temps;
    int    numTemps;
    char **instvars;
    int    numInstVars;
};

enum {
    PUSH_RCVR,
    PUSH_TEMP,
    PUSH_CONSTANT,
    SEND_BIN_MSG,
};

int getCodeFor(int type, int val) {
    switch (type) {
        case PUSH_RCVR:
            assert(val < 16);
            return val;
        case SEND_BIN_MSG:
            if (val == '+') return 176;
            if (val == '-') return 177;
            if (val == '<') return 178;
            if (val == '>') return 178;
            if (val == LESS_OR_EQUAL) return 180;
            if (val == GREATER_OR_EQUAL) return 181;
            if (val == '=') return 182;
            if (val == NOT_EQUAL) return 183;
            if (val == '*') return 184;
            if (val == '/') return 185;
            if (val == MODULO) return 186;
            break;
        case PUSH_CONSTANT:
            /*
             * TODO
            if (val == RECEIVE ) return 112;
            if (val == ST_TRUE) return 113;
            if (val == ST_FALSE) return 114;
            if (val == ST_NIL) return 115;
             */
            if (val == -1) return 116;
            if (val == 0) return 117;
            if (val == 1) return 118;
            if (val == 2) return 119;
            break;
        case PUSH_TEMP:
            assert(val < 16);
            return val + 16;
            break;
    }
    return 0;
}

void decode_expr(struct ExprUnit* e, struct MethodCompileInfo *info)
{
    struct BinaryMsg *binmsg;
    int code;
    switch (e->type) {
        case ST_ID:
            for (int i = 0; i < info->numTemps; i++) {
                if (strcmp(e->id.name, info->temps[i]) == 0) {
                    code = getCodeFor(PUSH_TEMP, i);
                    printf("<%i> pushTemp: %i (%s)\n", code, i, info->temps[i]);
                    return;
                }
            }
            for (int i = 0; i < info->numInstVars; i++) {
                if (strcmp(e->id.name, info->instvars[i]) == 0) {
                    code = getCodeFor(PUSH_RCVR, i);
                    printf("<%i> pushRcvr: %i (%s)\n", code, i, info->instvars[i]);
                    return;
                }
            }
            break;
        case ST_INT:
            printf("pushConstant: %s\n", e->integer.value);
            break;
        case ST_CHAR:
            /* WTF */
            code = getCodeFor(PUSH_CONSTANT, atoi(e->string.value));
            printf("<%i> pushConstant: %s\n", code, e->string.value);
            break;

        case ST_BINARY:
            decode_expr(e->binary.receiver, info);
            binmsg = e->binary.msgs;
            while (binmsg) {
                decode_expr(binmsg->arg, info);
                code = getCodeFor(SEND_BIN_MSG, binmsg->op);
                printf("<%i> send: %c\n", code, binmsg->op);
                binmsg = binmsg->next;
            }
            break;
    }
    if (e->returns) printf("<124> returnTop\n");
}

void compile_method(struct ClassHeader*h, struct Method* m) {

    printf("\n");
    struct MethodCompileInfo i;
    i.numTemps = get_temps(m, &i.temps);
    i.numInstVars= get_instvars(h, &i.instvars);
    struct ExprUnit *exp = m->exprs;
    while (exp) {
        decode_expr(exp, &i);
        exp = exp->next;
    }
    printf("TODO create the header and then, create a valid compiled method\n");
    printf("TODO create my own thing: primBytecode followed by the primitiveIndex\n");
    printf("flag value 0-4 args, return self 5, return an instance var 6, 7 header extention with num of args and primitive index\n");
}

