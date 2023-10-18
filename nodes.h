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
struct ClassFile;
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

void nodes_freeClassFile(struct ClassFile*);

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
 * Either unary, binary or keyword.
 */
struct DefUnary   { char *name; };
struct DefBinary  { int op; char *arg; };
struct DefKeyword { struct KeywordMsg *keys; };
union MethodDefType {
    struct DefUnary   unary;
    struct DefBinary  binary;
    struct DefKeyword keyword;
};
struct MethodDef {
    int type;
    union MethodDefType u;
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

struct EUnitId      { char *name; };
struct EUnitBinary  { int op; struct ExprUnit *receiver; struct BinaryMsg *msgs;};
struct EUnitUnary   { struct ExprUnit *receiver; struct UnaryMsg *msgs; };
struct EUnitKeyword { struct ExprUnit *receiver; struct KeywordMsg *msgs; };
struct EUnitBlock   { struct BlockArg *args; struct Temp *temps; struct ExprUnit *exprs; };
struct EUnitString  { char* value; };
struct EUnitCharacter { char *value; };
struct EUnitInteger { char *value; };
struct EUnitSymbol  { char *value; };
struct EUnitArray   { struct ExprUnit *head; };
union ExprUnitType {
    struct EUnitId        id;
    struct EUnitBinary    binary;
    struct EUnitUnary     unary;
    struct EUnitKeyword   keyword;
    struct EUnitBlock     block;
    struct EUnitString    string;
    struct EUnitCharacter character;
    struct EUnitInteger   integer;
    struct EUnitSymbol    symbol;
    struct EUnitArray     array;
};
struct ExprUnit {
    int                type;
    union ExprUnitType u;
    struct Cascade    *cascade;   /* XXX does this belong here? */
    struct ExprUnit   *next;      /* XXX does an ExprUnit have next? */
    int                returns;   /* XXX sould be in Expression */
    struct ExprUnit   *assignsTo; /* XXX should be in Expression */
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
