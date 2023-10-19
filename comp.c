#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "comp.h"
#include "bytecodes.h"
#include "parse.tab.h"

#define INITIAL_BUFF_SIZE 50

struct MethodCompileInfo {
    char **temps;
    int    numTemps;
    char **instvars;
    int    numInstVars;
};
enum {
    VALIS_NOTFOUND,
    VALIS_RCVR,
    VALIS_TEMP
};
struct Val {
    int type;
    int index;
};
static int  getTemps(struct Method*, char***);
static int  getInstvars(struct ClassHeader*, char ***);
static void decodeExpr(struct Method*, struct ExprUnit* e, struct MethodCompileInfo info);
static void compileMethod(struct Method*, struct MethodCompileInfo);
static void addByteToMethod(unsigned char, struct Method*);


void compiler_compile(struct ClassFile *cf)
{
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

static void compileMethod(
        struct Method           *method,
        struct MethodCompileInfo info)
{
    if (method->bytecodes) free(method->bytecodes);
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
    /* if no return, add bytecode 120 */
    expr = method->exprs;
    while (expr->next) expr = expr->next;
    if (!expr->returns) {
        addByteToMethod(120, method);
    }
}

struct Val
getVal(char* varname, struct MethodCompileInfo info) {
    struct Val p;
    p.type = VALIS_NOTFOUND;
    p.index = 0;
    for (int i = 0; i < info.numTemps; i++) {
        if (strcmp(varname, info.temps[i]) == 0) {
            p.index = i;
            p.type = VALIS_TEMP;
            return p;
        }
    }

    for (int i = 0; i < info.numInstVars; i++) {
        if (strcmp(varname, info.instvars[i]) == 0) {
            p.index = i;
            p.type = VALIS_RCVR;
            return p;
        }
    }

    return p;
}

static void decodeExpr(
        struct Method           *method,
        struct ExprUnit         *expr,
        struct MethodCompileInfo info)
{
    struct BinaryMsg *binmsg;
    struct Val val;
    unsigned char code;
    switch (expr->type) {
        case ST_ID:
            val = getVal(expr->u.id.name, info);
            if (val.type == VALIS_NOTFOUND) {
                fprintf(stderr, "can not find %s\n", expr->u.id.name);
                exit(1);
            } else if (val.type == VALIS_TEMP) {
                code = bytecodes_getCodeFor(PUSH_TEMP, val.index);
                addByteToMethod(code, method);
            } else {
                code = bytecodes_getCodeFor(PUSH_RCVR, val.index);
                addByteToMethod(code, method);
            }

            break;
        case ST_INT:
            code = bytecodes_getCodeFor(PUSH_CONSTANT, atoi(expr->u.string.value));
            addByteToMethod(code, method);
            break;
        case ST_CHAR:
            code = bytecodes_getCodeFor(PUSH_CONSTANT, atoi(expr->u.string.value));
            addByteToMethod(code, method);
            break;

        case ST_BINARY:
            decodeExpr(method, expr->u.binary.receiver, info);
            binmsg = expr->u.binary.msgs;
            while (binmsg) {
                decodeExpr(method, binmsg->arg, info);
                code = bytecodes_getCodeFor(SEND_BIN_MSG, binmsg->op);
                addByteToMethod(code, method);
                binmsg = binmsg->next;
            }
            break;
    }

    if (expr->assignsTo) {
        struct ExprUnit *e = expr->assignsTo;
        while (e) {
            val = getVal(e->u.id.name, info);
            if (val.type == VALIS_NOTFOUND) {
                fprintf(stderr, "can not find %s\n", expr->u.id.name);
                exit(1);
            }
            if (val.type == VALIS_TEMP) {
                code = bytecodes_getCodeFor(POP_STORE_TEMP, val.index);
                addByteToMethod(code, method);
                // if e->next
                code = bytecodes_getCodeFor(PUSH_TEMP, val.index);
            } else { // VALIS_RCVR
                code = bytecodes_getCodeFor(POP_STORE_RCVR, val.index);
                addByteToMethod(code, method);
                // if e->next
                code = bytecodes_getCodeFor(PUSH_RCVR, val.index);
            }
            if (e->next)
                addByteToMethod(code, method);
            e = e->next;
        }
    }

    if (expr->returns) addByteToMethod(124, method);
}

static int getTemps(struct Method* m, char*** dest) {
    char **temps = NULL;
    int numTmps = 0;

    if (m->def->type == ST_KEYWORD) {
        struct KeywordMsg *kw = m->def->u.keyword.keys;
        while (kw) {
            numTmps++;
            kw = kw->next;
        }
    } else if (m->def->type == ST_BINARY) {
        numTmps++;
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
        struct KeywordMsg *kw = m->def->u.keyword.keys;
        while (kw) {
            temps[tmpid++] = kw->arg->u.id.name;
            kw = kw->next;
        }
    } else if (m->def->type == ST_BINARY) {
        temps[tmpid++] = m->def->u.binary.arg;
    }

    if (m->temps) {
        struct Temp *t = m->temps;
        while (t) {
            temps[tmpid++] = t->name;
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

static void addByteToMethod(
        unsigned char   byte,
        struct Method *method)
{
    if (method->bytecount == method->buffsize) {
        method->bytecodes = realloc(
                method->bytecodes, sizeof(char) * method->buffsize * 2);
    }

    method->bytecodes[method->bytecount++] = byte;
}

