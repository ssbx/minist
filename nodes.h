/*
 * nodes.h
 *
 * This file contains all structures used by the parser the generate a tree of
 * nodes representing a smalltalk 80 class file. Notable users of the generated
 * tree, are compiler and pprinter.
 *
 */
#ifndef PARSER_NODES
#define PARSER_NODES
struct ClassHeader;
struct ClassComment;
struct MethodCategory;
struct Method;
struct Expression;
struct KeywordMethod;
struct MethodDef;
struct ExprUnit;
struct Temp;
struct UnaryMsg;
struct BinaryMsg;
struct Cascade;


/*
 * Represent one file of a smalltalk class.
 */
struct ClassFile {
    struct ClassHeader      *header;
    struct MethodCategory   *categories;
    struct ClassClassHeader *classHeader;
    struct MethodCategory   *classCategories;
    char                    *comment;
};


/*
 * Object subclass: #Rectangle
 *    instanceVariableNames: 'origin corner '
 *    classVariableNames: ''
 *    poolDictionaries: ''
 *    category: 'Graphics-Primitives'!
 */
struct ClassHeader {
    char  *super;
    char  *className;
    int    instsVarNamesCount;
    char **instsVarNames;
    int    classVarNamesCount;
    char **classVarNames;
    int    poolDictsCount;
    char **poolDicts;
    char  *category;
};


/*
 * Rectangle comment: 'comment'!
 */
struct ClassComment {
    char *className;
    char *str;
};


/*
 * !Rectangle methodsFor: 'accessing'!
 */
struct MethodCategory {
    struct MethodCategory *next;
    char                  *name;
    struct ExprUnit       *classname; // wtf should be string
    struct Method         *methods;
};


/*
 * Rectangle class
 *    instanceVariableNames: '' !
 */
struct ClassClassHeader {
    char  *className;
    int    instVarNamesCount;
    char **instVarNames;
};


/*
 * Represent the entire method:
 *
 * myMethod             -> def
 *      | a b c |       -> temps
 *      <primitive: 1>  -> prim
 *      ^ a             -> exprs
 */
struct Method {
    struct MethodDef *def;
    struct Temp      *temps;
    struct ExprUnit  *prim;
    struct ExprUnit  *exprs;

    int            buffsize;
    int            bytecount;
    unsigned char *bytecodes;

    struct Method *next;
};


/*
 * Either unary, binary or keyword. XXX Does not look nice (union?).
 */
struct MethodDef {
    int type;
    struct ExprUnit *unary;

    int  binary;
    struct ExprUnit *arg;

    struct KeywordMsg *keys;
};


/*
 * Temporary variables:
 *
 *  | var1 var2 var3 |
 */
struct Temp {
    struct Temp     *next;
    struct ExprUnit *name; // XXX should be simple *char
};


/* XXX Should be used as a kind of ExprUnit that can be returned or assigned.
 * XXX See ExprUnit comments
struct Expression {
    int    type;
    struct Expression *next;
    struct ExprUnit* eval;
    int    returns;
    struct ExprUnit* assignsTo;
};
*/


/*
 * The big thing. XXX This needs to be clarified.
 */
enum {
    ST_ID,
    ST_UNIT,
    ST_UNARY,
    ST_BINARY,
    ST_KEYWORD,
    ST_BLOCK,
    ST_STRING,
    ST_CHAR,
    ST_INT,
    ST_SYMBOL,
    ST_ARRAY,
    ST_ARRAYCONST
};

struct ExprUnit {
    int type;
    union {

        struct {
            char *name;
        } id; /* XXX does an id needs to be a ExprUnit? */

        struct {
            int op;
            struct ExprUnit *receiver;
            struct BinaryMsg *msgs;
        } binary;

        struct {
            struct ExprUnit *receiver;
            struct UnaryMsg *msgs;
        } unary;
        struct {
            struct ExprUnit *receiver;
            struct KeywordMsg *msgs;
        } keyword;
        struct {
            struct BlockArg *args;
            struct Temp     *temps;
            struct ExprUnit *exprs;
        } block;
        struct {
            char *value;
        } string;
        struct {
            char *value;
        } character;
        struct {
            char *value;
        } integer;
        struct {
            char *value;
        } symbol;
        struct {
            struct ExprUnit *head;
        } array;
    };
    struct Cascade  *cascade;   /* XXX does this belong here? */
    struct ExprUnit *next;      /* XXX does an ExprUnit have next? */
    int              returns;   /* XXX sould be in Expression */
    struct ExprUnit *assignsTo; /* XXX should be in Expression */
};


/*
 * Chain of messages of type binary/unary
 */
struct BinaryMsg {
    int op;
    struct ExprUnit  *arg;
    struct BinaryMsg *next;
};
struct UnaryMsg {
    struct ExprUnit *msg;
    struct UnaryMsg *next;
};


/*
 * A Single keyword message
 */
struct KeywordMsg {
    struct KeywordMsg *next;
    char              *key;
    struct ExprUnit   *arg;
};


/*
 * Cascades
 *
 * myObj msg1;
 *       msg2;
 *       msg3.
 */
enum {
    CASCADE_UNARY,
    CASCADE_BINARY,
    CASCADE_KEYWORD,
};

struct Cascade {
    int type;
    union {
        struct ExprUnit* msg;
    } unary;
    union {
        struct BinaryMsg *msg;
    } binary;
    union {
        struct KeywordMsg *msg;
    } keyword;
    struct Cascade *next;
};


/*
 * Block arguments. XXX Might share something with struct Temp?
 * [ :arg1 :arg2 | body... ]
 */
struct BlockArg {
    char            *name;
    struct BlockArg *next;
};

#endif // PARSER_NODES
