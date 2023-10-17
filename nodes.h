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

struct ClassHeader {
    char *super;
    char *className;
    int    instsVarNamesCount;
    char **instsVarNames;
    int    classVarNamesCount;
    char **classVarNames;
    int    poolDictsCount;
    char **poolDicts;
    char *category;
};

struct ClassComment {
    char *className;
    char *str;
};

struct ClassClassHeader {
    char  *className;
    int    instVarNamesCount;
    char **instVarNames;
};

struct Temp {
    struct Temp *next;
    struct ExprUnit *name; // TODO must be simple *char
};

struct MethodCategory {
    struct MethodCategory *next;
    char* name;
    struct ExprUnit* classname; // wtf should be string
    struct Method* methods;
};

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

struct MethodDef {
    int type;
    struct ExprUnit *unary;

    int  binary;
    struct ExprUnit *arg;

    struct KeywordMsg *keys;
};

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

struct BinaryMsg {
    int op;
    struct ExprUnit* arg;
    struct BinaryMsg *next;
};

struct UnaryMsg {
    struct ExprUnit *msg;
    struct UnaryMsg *next;
};

struct Expression {
    int    type;
    struct Expression *next;
    struct ExprUnit* eval;
    int    returns;
    struct ExprUnit* assignsTo;
};
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
    struct Cascade *cascade;
    union {

        struct {
            char *name;
        } id;

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
    struct ExprUnit *next;
    int    returns;
    struct ExprUnit *assignsTo;
};

struct BlockArg {
    struct BlockArg *next;
    char *name;
};

struct KeywordMsg {
    struct KeywordMsg *next;
    char *key;
    struct ExprUnit *arg; /* todo expr */
};

#endif // PARSER_NODES
