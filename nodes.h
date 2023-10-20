/*
 * nodes.h
 *
 * This file contains all structures used by the parser the generate a tree of
 * nodes representing a smalltalk 80 class file. Used  by the comp.c and
 * pprint.c.
 *
 * See struct ExprUnit bellow.
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

/*
 * Represent one file of a smalltalk class.
 */
struct ClassFile {
    struct ClassHeader      *header;
    struct MethodCategory   *categories;
    struct ClassClassHeader *classHeader;
    struct MethodCategory   *classCategories;
    char                    *comment;
    struct ExprUnit         *init;
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
    char                  *classname;
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

    char         **literal_frame;
    int            literal_frame_size;
    int            literal_frame_count;

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
    char        *name;
    struct Temp *next;
};

/*
 * struct ExprUnit: the big thing. This is where parse.y create a tree of
 * expressions, taking care of the presedence. comp.c and pprint.c just have
 * to follow a first expression, and "unroll" their arguments.
 *
 * Some ExprUnit can be evaluated:
 *  - EUnitBinary
 *  - EUnitUnary
 *  - EUnitKeyword
 *
 * All ExprUnits can act as receiver or argument values.
 *
 * Note that parse.y have filled structures with msgs receiver arguments etc
 * that follow the smalltak precedence rule.
 *
 */
enum {
    ST_ID,
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
struct EUnitBinary  { struct ExprUnit *receiver; struct BinaryMsg *msgs; int op; };
struct EUnitUnary   { struct ExprUnit *receiver; struct UnaryMsg *msgs; };
struct EUnitKeyword { struct ExprUnit *receiver; struct KeywordMsg *msgs; };

struct EUnitId      { char *name; };
struct EUnitString  { char *value; };
struct EUnitChar    { char *value; };
struct EUnitInteger { char *value; };
struct EUnitSymbol  { char *value; };
struct EUnitBlock   { struct BlockArg *args; struct Temp *temps; struct ExprUnit *exprs; };
struct EUnitArray   { struct ExprUnit *head; };
union ExprUnitType {
    struct EUnitId      id;
    struct EUnitBinary  binary;
    struct EUnitUnary   unary;
    struct EUnitKeyword keyword;
    struct EUnitBlock   block;
    struct EUnitString  string;
    struct EUnitChar    character;
    struct EUnitInteger integer;
    struct EUnitSymbol  symbol;
    struct EUnitArray   array;
};

/*
 * struct ExprUnit the main node
 */
struct ExprUnit {
    int                type;
    union ExprUnitType u;
    struct ExprUnit   *assignsTo; /* z := := x := y := myExprUnit */
    struct Cascade    *cascade;   /* myExprUnit msg;  andOther: arg ; + 1 */
    struct ExprUnit   *next;
    int                returns;   /* ^ */
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
    char            *msg;
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
union CascadeType {
    char              *unary;
    struct BinaryMsg  *binary;
    struct KeywordMsg *keyword;
};
struct Cascade {
    int type;
    union  CascadeType u;
    struct Cascade *next;
};


/*
 * [ :arg1 :arg2 | body... ]
 */
struct BlockArg {
    char            *name;
    struct BlockArg *next;
};

#endif // PARSER_NODES
