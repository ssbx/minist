#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "comp.h"
#include "utils.h"
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
    VALIS_ASSOC,
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
static int getLiteralIndex(char*, struct Method*);
enum {LIT_SELECTOR, LIT_ASSOC};
static int addLiteralToMethod(int,char*,struct Method*);
static int addLiteralKwToMethod(struct KeywordMsg *msgs, struct Method *method);

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

    method->literal_frame = malloc(sizeof(char*) * INITIAL_BUFF_SIZE);
    method->literal_frame_size = INITIAL_BUFF_SIZE;
    method->literal_frame_count = 0;
    assert(method->literal_frame);

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
getVal(char* varname, struct MethodCompileInfo info, struct Method* m) {
    struct Val p;
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

    /* if not found must be an assoc */

    p.type = VALIS_ASSOC;
    p.index = addLiteralToMethod(LIT_ASSOC, varname, m);
    return p;
}

static void decodeExpr(
        struct Method           *method,
        struct ExprUnit         *expr,
        struct MethodCompileInfo info)
{
    int kargc;
    struct UnaryMsg   *umsg;
    struct KeywordMsg *kmsg;
    int literal_index;
    struct BinaryMsg *binmsg;
    struct Val val;
    unsigned char code;
    switch (expr->type) {
        case ST_ID:
            val = getVal(expr->u.id.name, info, method);
            if (val.type == VALIS_ASSOC) {
                code = bytecodes_getCodeFor(PUSH_LITERAL_VARIABLE, val.index);
                addByteToMethod(code, method);
            } else if (val.type == VALIS_TEMP) {
                code = bytecodes_getCodeFor(PUSH_TEMP, val.index);
                addByteToMethod(code, method);
            } else if (val.type == VALIS_RCVR) {
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
        case ST_UNARY:
            decodeExpr(method, expr->u.unary.receiver, info);

            umsg = expr->u.unary.msgs;
            while (umsg) {
                literal_index = addLiteralToMethod(LIT_SELECTOR, umsg->msg, method);
                code = bytecodes_getCodeFor(SEND_LITERAL_NOARG, literal_index);
                addByteToMethod(code, method);
                umsg = umsg->next;
            }

            break;
        case ST_KEYWORD:
            literal_index = addLiteralKwToMethod(expr->u.keyword.msgs, method);

            decodeExpr(method, expr->u.keyword.receiver, info);

            kargc = 0;
            kmsg = expr->u.keyword.msgs;
            while (kmsg) {
                kargc++;
                decodeExpr(method, kmsg->arg, info);
                kmsg =kmsg->next;
            }
            if (kargc == 1) {
                code = bytecodes_getCodeFor(SEND_LITERAL_1ARG, literal_index);
                addByteToMethod(code, method);
            } else if (kargc == 2) {
                code = bytecodes_getCodeFor(SEND_LITERAL_2ARG, literal_index);
                addByteToMethod(code, method);
            }
            break;
    }

    if (expr->assignsTo) {
        struct ExprUnit *e = expr->assignsTo;
        while (e) {
            val = getVal(e->u.id.name, info, method);
            if (val.type == VALIS_ASSOC) {
                fprintf(stderr, "can not find %s\n", expr->u.id.name);
                exit(1);
            }
            if (val.type == VALIS_TEMP) {
                code = bytecodes_getCodeFor(POP_STORE_TEMP, val.index);
                addByteToMethod(code, method);
                // if e->next TODO
                code = bytecodes_getCodeFor(PUSH_TEMP, val.index);
            } else { // VALIS_RCVR
                code = bytecodes_getCodeFor(POP_STORE_RCVR, val.index);
                addByteToMethod(code, method);
                // if e->next TODO
                code = bytecodes_getCodeFor(PUSH_RCVR, val.index);
            }
            if (e->next)
                addByteToMethod(code, method);
            e = e->next;
        }
    }

    if (expr->returns) addByteToMethod(124, method);
}

static int addLiteralKwToMethod(struct KeywordMsg *msgs, struct Method *method)
{
    struct KeywordMsg *m = msgs;
    int slen = 0;
    char *str;
    while(m) {
        slen += strlen(m->key);
        m = m->next;
    }
    str = malloc(sizeof(char) * slen + 1);
    m = msgs;
    int index = 0;
    while (m) {
        memcpy(&str[index], m->key, strlen(m->key));
        index += strlen(m->key);
        str[index] = '\0';
        m = m->next;
    }
    str[index] = '\0';

    int litindex = addLiteralToMethod(LIT_SELECTOR, str, method);
    free(str);
    return litindex;
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
    /* TODO recursive super too */
    *v = h->instsVarNames;
    return h->instsVarNamesCount;
}

static int
addLiteralToMethod(int type, char* literal, struct Method* method)
{
    static char fmt[] = "#%s -> %s";
    int ret;
    if (method->literal_frame_count == method->literal_frame_size) {
        method->literal_frame =
            malloc(sizeof(char*) * 2 * method->literal_frame_size);
        assert(method->literal_frame);
        method->literal_frame_size *= 2;
    }

    ret = method->literal_frame_count;
    if (type == LIT_SELECTOR) {
        for (int i = 0; i < method->literal_frame_count; i++) {
            /* allready present */
            if (strcmp(method->literal_frame[i], literal) == 0)
                return i;
        }
        method->literal_frame[method->literal_frame_count++] = strdup2(literal);
    } else if (type == LIT_ASSOC) {
        int strsize = strlen(literal) * 2 + strlen(fmt) + 1;
        char *str = malloc(sizeof(char) * strsize);
        snprintf(str, strsize, fmt, literal, literal);
        for (int i = 0; i < method->literal_frame_count; i++) {
            /* allready present */
            if (strcmp(method->literal_frame[i], str) == 0) {
                free(str);
                return i;
            }
        }
        method->literal_frame[method->literal_frame_count++] = str;
    }
    return ret;
}

static int getLiteralIndex(char*lit, struct Method*m)
{
    for (int i = 0; i < m->literal_frame_count; i++) {
        if (strcmp(lit, m->literal_frame[i]) == 0) return i;
    }
    assert(0 == 1);
    return -1;
}

static void addByteToMethod(
        unsigned char   byte,
        struct Method *method)
{
    if (method->bytecount == method->buffsize) {
        method->bytecodes = realloc(
                method->bytecodes, sizeof(char) * method->buffsize * 2);
        assert(method->bytecodes);
    }

    method->bytecodes[method->bytecount++] = byte;
}

