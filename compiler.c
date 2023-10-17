#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "compiler.h"
#include "parser.tab.h"

#define INITIAL_BUFF_SIZE 50

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

static int getTemps(struct Method*, char***);
static int getInstvars(struct ClassHeader*, char ***);
static unsigned char getCodeFor(int , int );
static void decodeExpr(struct Method*, struct ExprUnit* e, struct MethodCompileInfo info);
static void compileMethod(struct Method*, struct MethodCompileInfo);
static void addByteToMethod(unsigned char, struct Method*);

void compiler_compile(struct ClassFile *cf) {
    struct MethodCategory   *cat;
    struct Method           *method;
    struct MethodCompileInfo info;

    info.numInstVars = getInstvars(cf->header, &info.instvars);
    info.temps = NULL;
    info.numTemps = 0;

    cat = cf->categories;
    while (cat) {
        method = cat->methods;
        while (method) {
            compileMethod(method, info);
            method = method->next;
        }
        cat = cat->next;
    }
}

void compileMethod(struct Method* method, struct MethodCompileInfo info)
{
    method->bytecodes = malloc(sizeof(char) * INITIAL_BUFF_SIZE);
    method->bytecount = 0;
    method->buffsize  = INITIAL_BUFF_SIZE;
    assert(method->bytecodes);

    if (info.temps) free(info.temps);
    info.temps    = NULL;
    info.numTemps = 0;
    info.numTemps = getTemps(method, &info.temps);

    struct ExprUnit *expr = method->exprs;
    while (expr) {
        decodeExpr(method, expr, info);
        expr = expr->next;
    }
}

static void addByteToMethod(unsigned char byte, struct Method * method)
{
    if (method->bytecount == method->buffsize) {
        method->bytecodes = realloc(
                method->bytecodes, sizeof(char) * method->buffsize * 2);
    }

    method->bytecodes[method->bytecount++] = byte;
}


void decodeExpr(struct Method *method, struct ExprUnit* e, struct MethodCompileInfo info)
{
    struct BinaryMsg *binmsg;
    unsigned char code;
    switch (e->type) {
        case ST_ID:
            for (int i = 0; i < info.numTemps; i++) {
                if (strcmp(e->id.name, info.temps[i]) == 0) {
                    code = getCodeFor(PUSH_TEMP, i);
                    addByteToMethod(code, method);
                    return;
                }
            }
            for (int i = 0; i < info.numInstVars; i++) {
                if (strcmp(e->id.name, info.instvars[i]) == 0) {
                    code = getCodeFor(PUSH_RCVR, i);
                    addByteToMethod(code, method);
                    return;
                }
            }
            printf("%s %i error\n", __FILE__, __LINE__);
            break;
        case ST_INT:
            printf("wtf pushConstant: %s\n", e->integer.value);
            break;
        case ST_CHAR:
            /* WTF */
            code = getCodeFor(PUSH_CONSTANT, atoi(e->string.value));
            addByteToMethod(code, method);
            break;

        case ST_BINARY:
            decodeExpr(method, e->binary.receiver, info);
            binmsg = e->binary.msgs;
            while (binmsg) {
                decodeExpr(method, binmsg->arg, info);
                code = getCodeFor(SEND_BIN_MSG, binmsg->op);
                addByteToMethod(code, method);
                binmsg = binmsg->next;
            }
            break;
    }
    if (e->returns) addByteToMethod(124, method);
}

static int getTemps(struct Method* m, char*** dest) {
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

static int getInstvars(struct ClassHeader *h, char ***v) {
    /* todo recursive super too */
    *v = h->instsVarNames;
    return h->instsVarNamesCount;
}

static unsigned char getCodeFor(int type, int val) {
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

/*

void decodeExpr(struct ExprUnit* e, struct MethodCompileInfo info)
{
    struct BinaryMsg *binmsg;
    int code;
    switch (e->type) {
        case ST_ID:
            for (int i = 0; i < info.numTemps; i++) {
                if (strcmp(e->id.name, info.temps[i]) == 0) {
                    code = getCodeFor(PUSH_TEMP, i);
                    printf("<%i> pushTemp: %i (%s)\n", code, i, info.temps[i]);
                    return;
                }
            }
            for (int i = 0; i < info.numInstVars; i++) {
                if (strcmp(e->id.name, info.instvars[i]) == 0) {
                    code = getCodeFor(PUSH_RCVR, i);
                    printf("<%i> pushRcvr: %i (%s)\n", code, i, info.instvars[i]);
                    return;
                }
            }
            printf("%s %i error\n", __FILE__, __LINE__);
            break;
        case ST_INT:
            printf("pushConstant: %s\n", e->integer.value);
            break;
        case ST_CHAR:
            // WTF
            code = getCodeFor(PUSH_CONSTANT, atoi(e->string.value));
            printf("<%i> pushConstant: %s\n", code, e->string.value);
            break;

        case ST_BINARY:
            decodeExpr(e->binary.receiver, info);
            binmsg = e->binary.msgs;
            while (binmsg) {
                decodeExpr(binmsg->arg, info);
                code = getCodeFor(SEND_BIN_MSG, binmsg->op);
                printf("<%i> send: %c\n", code, binmsg->op);
                binmsg = binmsg->next;
            }
            break;
    }
    if (e->returns) printf("<124> returnTop\n");
}

*/
