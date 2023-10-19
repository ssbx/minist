%{

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "nodes.h"

extern int yylineno;
int  yylex();
void yyerror(const char *s); /* defined in main.c */
extern char* yytext;
extern char* st_string;
extern int st_op;
extern char *fname;

char *strdup2(const char *str) {
    size_t len = strlen(str) + 1;
    char  *new = malloc(len);
    memcpy(new, str, len);
    return new;
}

struct ClassFile* parsed_file = NULL;

void freeExprUnit(struct ExprUnit *expr);
void checkStr(char*, char*);

void addAssignsTo(struct ExprUnit *exp, struct ExprUnit *id);
struct ExprUnit* allocExprUnit(int);
struct ExprUnit* mkArray(struct ExprUnit*);
struct ExprUnit* mkArrayConst(struct ExprUnit*);
struct ExprUnit* mkSymbolExpr(char*);
struct ExprUnit* mkIntExpr(char*);
struct ExprUnit* mkCharExpr(char*);
struct ExprUnit* mkStringExpr(char *);
struct ExprUnit* mkBlockExpr(struct BlockArg*, struct Temp*, struct ExprUnit*);
struct ExprUnit* mkKeywordExpr(struct ExprUnit*, struct KeywordMsg*);
struct ExprUnit* mkBinaryExpr(struct ExprUnit*, struct BinaryMsg*);
struct ExprUnit* mkUnaryExpr(struct ExprUnit*, struct UnaryMsg*);
struct ExprUnit* mkIdExpr(char*);
struct BlockArg*   mkBlockArg(char*);
struct KeywordMsg* mkKeywordMsg(char*, struct ExprUnit*);
struct BinaryMsg*  mkBinaryMsg(int, struct ExprUnit*);
struct UnaryMsg*   mkUnaryMsg(struct ExprUnit*);
struct Temp*       mkTemp(struct ExprUnit*);
void addBlockArg(struct BlockArg*, struct BlockArg*);
void addExpr(struct ExprUnit*, struct ExprUnit*);
void addKeywordMsg(struct KeywordMsg*, struct KeywordMsg*);
void addBinaryMsg(struct BinaryMsg*, struct BinaryMsg*);
void addUnaryMsg(struct ExprUnit*, struct UnaryMsg*);
void addKeywordCascade(struct ExprUnit*, struct KeywordMsg*);
void addBinaryCascade(struct ExprUnit*, struct BinaryMsg*);
void addUnaryCascade(struct ExprUnit*, struct ExprUnit*);
struct MethodDef* mkUnaryMethodDef(struct ExprUnit*);
struct MethodDef* mkBinaryMethodDef(int, struct ExprUnit*);
struct MethodDef* mkKeywordMethodDef(struct KeywordMsg*);
struct Method* mkMethod(struct MethodDef*, struct ExprUnit*,struct Temp*, struct ExprUnit*);
struct ClassComment* mkComment(struct ExprUnit*, struct ExprUnit*);
struct ClassClassHeader* mkClassClassHeader(struct ExprUnit*,struct ExprUnit*);
struct MethodCategory* mkMethodCategory(
                            struct ExprUnit*,struct ExprUnit*, struct Method*);
struct ClassFile* mkFile(struct ClassHeader*, struct ClassComment*,
     struct MethodCategory*, struct ClassClassHeader*, struct MethodCategory*);
struct ClassHeader* mkClassHeader(struct ExprUnit*,struct ExprUnit*,
      struct ExprUnit*, struct ExprUnit*, struct ExprUnit*,  struct ExprUnit*);
%}

%define parse.error detailed
%union {
    char                  *str;
    struct ClassFile      *file;
    struct ClassHeader    *header;
    struct ClassClassHeader *ccheader;
    struct ClassComment   *comment;
    struct MethodCategory *category;
    struct Method         *method;
    struct MethodDef      *message;
    struct ExprUnit       *uexpr;
    struct Expression     *expression;
    struct Temp           *temp;
    struct BinaryMsg      *binmsg;
    struct KeywordMsg     *keymsg;
    struct BlockArg       *blockarg;
    int                    binary;
}

%token ST_ASSIGN
%token LESS_OR_EQUAL GREATER_OR_EQUAL NOT_EQUAL MODULO
%token OBJECT_EQUALS OBJECT_NOT_EQUALS
%token IDENTIFIER STRING KEYWORD SYMCONST COLONVAR CHARCONST INTEGER
%token EOL CLASS

%type <file> file
%type <header> clsheader
%type <ccheader> clsclsheader
%type <comment> classcomment
%type <message> message
%type <category> category categories clscategories clscategory
%type <method> method methods
%type <temp> temps tempids
%type <uexpr> exprs expro expr2 expr1 unaryexpr msgexpr keyexpr id unit prim prim2
%type <uexpr> binexpr block string charconst integer symconst literals
%type <uexpr> primitive arrayconst
%type <uexpr> arrayelement arrayelements array
%type <blockarg> blockargs
%type <binmsg> binmsg
%type <keymsg> keymsg methodkeymsg
%type <binary> binsel binops
%type <binary> ','  '+'  '-'  '*'  '/'  '<'  LESS_OR_EQUAL  '>'
%type <binary> GREATER_OR_EQUAL  '='  NOT_EQUAL  MODULO  '@'  OBJECT_EQUALS
%type <binary> OBJECT_NOT_EQUALS '&' '|'
%type <str> colonvar keysel

%%

/******************************************************************************
 * The file from class declaration, to method cathegories
 * file
 *****************************************************************************/
file: clsheader classcomment            {$$ = mkFile($1,$2,NULL,NULL,NULL);}
    | clsheader classcomment categories {$$ = mkFile($1,$2,$3,NULL,NULL);}
    | clsheader classcomment categories '!' {$$ = mkFile($1,$2,$3,NULL,NULL);}
    | clsheader classcomment categories '!' clsclsheader
            { $$ = mkFile($1, $2, $3, $5, NULL); }
    | clsheader classcomment categories '!' clsclsheader clscategories
            { $$ = mkFile($1, $2, $3, $5, $6); }

/******************************************************************************
 * file -> clsheader
 *****************************************************************************/
clsheader: id keysel symconst
            keysel string
            keysel string
            keysel string
            keysel string  '!' {
        checkStr($2,  "subclass:");
        checkStr($4,  "instanceVariableNames:");
        checkStr($6,  "classVariableNames:");
        checkStr($8,  "poolDictionaries:");
        checkStr($10, "category:");
        free($2);
        free($4);
        free($6);
        free($8);
        free($10);
        $$ = mkClassHeader($1, $3, $5, $7, $9, $11);}

/******************************************************************************
 * file -> classcomment
 *****************************************************************************/
classcomment: id keysel string '!' {
            checkStr($2, "comment:");
            free($2);
            $$ = mkComment($1, $3); }

/******************************************************************************
 * file -> categories
 *****************************************************************************/
categories: category
          | categories category   {
                struct MethodCategory *last = $1;
                while (last->next)
                    last = last->next;
                last->next = $2;
                $$ = $1;}

category: '!' id keysel string '!' methods '!' {
                checkStr($3, "methodsFor:");
                free($3);
                $$ = mkMethodCategory($2, $4, $6); }
        | '!' id keysel string '!' '!' {
                checkStr($3, "methodsFor:");
                free($3);
                $$ = mkMethodCategory($2, $4, NULL); }

/******************************************************************************
 * file -> clsclsheader
 *****************************************************************************/
clsclsheader: id id keysel string '!' {
        checkStr($3,"instanceVariableNames:");
        free($3);
        checkStr($2->u.id.name, "class");
        freeExprUnit($2);
        $$ = mkClassClassHeader($1, $4);}

/******************************************************************************
 * file -> clscategories
 *****************************************************************************/
clscategories: clscategory
             | clscategories clscategory {
                struct MethodCategory *last = $1;
                while (last->next)
                    last = last->next;
                last->next = $2;
                $$ = $1;}
clscategory: '!' id id keysel string '!' methods '!' {
                assert($3->type == ST_ID);
                checkStr($3->u.id.name, "class");
                checkStr($4, "methodsFor:");
                $$ = mkMethodCategory($2, $5, $7); }
           | '!' id id keysel string '!' '!' {
                assert($3->type == ST_ID);
                checkStr($3->u.id.name, "class");
                checkStr($4, "methodsFor:");
                $$ = mkMethodCategory($2, $5, NULL); }

/******************************************************************************
 * file -> category -> methods
 *****************************************************************************/
methods: method
       | methods method {
            struct Method *last = $1;
            while (last->next)
                last = last->next;
            last->next = $2;
            $$ = $1;}

/******************************************************************************
 * The METHOD part
 *****************************************************************************/
method: message       '!'                 { $$ = mkMethod($1, NULL, NULL, NULL); }
      | message exprs '!'                 { $$ = mkMethod($1, NULL, NULL, $2); }
      | message temps exprs '!'           { $$ = mkMethod($1, NULL,   $2, $3); }
      | message primitive exprs '!'       { $$ = mkMethod($1, $2,   NULL, $3); }
      | message primitive temps exprs '!' { $$ = mkMethod($1, $2,     $3, $4); }

primitive: '<' keysel integer '>' { $$ = $3; }
/******************************************************************************
 * The method message definition
 *****************************************************************************/
message: id            { $$ = mkUnaryMethodDef($1); }
       | binsel id     { $$ = mkBinaryMethodDef($1, $2); }
       | methodkeymsg  { $$ = mkKeywordMethodDef($1); }

methodkeymsg: keysel id        { $$ = mkKeywordMsg($1, $2); }
      | methodkeymsg keysel id {
            struct KeywordMsg *new = mkKeywordMsg($2, $3);
            struct KeywordMsg *last = $1;
            while (last->next)
                last = last->next;
            last->next = new;
            $$ = $1;}

/******************************************************************************
 * The temporaries variables
 *****************************************************************************/
temps: '|' tempids '|'  { $$ = $2; }
tempids: id             { $$ = mkTemp($1); }
       | tempids id     { struct Temp *new = mkTemp($2);
                          struct Temp *last = $$;
                          while (last->next)
                              last = last->next;
                          last->next = new;
                          $$ = $1;}

/******************************************************************************
 ******************************************************************************
 *****  Expressions are the body of the method, and the main thing ************
 ******************************************************************************
 *****************************************************************************/
exprs: expr1            { $$ = $1; }
     | '^' expr1        { $$ = $2; $$->returns = 1; }
     | exprs '.'        { $$ = $1; }
     | exprs '.' expr1  { addExpr($1, $3);                  $$ = $1; }
     | exprs '^' expr1  { addExpr($1, $3); $3->returns = 1; $$ = $1; } /* end of block */

/******************************************************************************
 * add assignment to expr2
 *****************************************************************************/
expr1: expr2 | expro

expro: id ST_ASSIGN expr2 { $$ = $3; $3->assignsTo = $1; }
     | id ST_ASSIGN expro { $$ = $3; addAssignsTo($$, $1); }

/******************************************************************************
 * add cascade to unary/bin/keyword
 *****************************************************************************/
msgexpr: unaryexpr | binexpr | keyexpr
expr2: unit             { $$ = $1; }
     | msgexpr          { $$ = $1; }
     | expr2 ';' id     { addUnaryCascade($1, $3);   $$ = $1; }
     | expr2 ';' binmsg { addBinaryCascade($1, $3);  $$ = $1; }
     | expr2 ';' keymsg { addKeywordCascade($1, $3); $$ = $1; }

/******************************************************************************
 * unarymessage
 *****************************************************************************/
unaryexpr: unit      id { $$ = mkUnaryExpr($1, mkUnaryMsg($2)); }
         | unaryexpr id { addUnaryMsg($1, mkUnaryMsg($2)); $$ = $1; }

/******************************************************************************
 * binarymessage
 *****************************************************************************/
prim: unit | unaryexpr
binexpr: prim binmsg       { $$ = mkBinaryExpr($1, $2); }
binmsg: binsel prim        { $$ = mkBinaryMsg($1, $2); }
      | binmsg binsel prim { addBinaryMsg($1, mkBinaryMsg($2, $3)); $$ = $1; }

/******************************************************************************
 * keywordmessage
 *****************************************************************************/
prim2: unit | binexpr | unaryexpr
keyexpr: prim2 keymsg       { $$ = mkKeywordExpr($1,$2); }
keymsg: keysel prim2        { $$ = mkKeywordMsg($1, $2); }
      | keymsg keysel prim2 { addKeywordMsg($1,mkKeywordMsg($2, $3)); $$ = $1;}

/******************************************************************************
 * block
 *****************************************************************************/
block: '[' ']'                           { $$ = mkBlockExpr(NULL, NULL, NULL);}
     | '[' exprs ']'                     { $$ = mkBlockExpr(NULL, NULL, $2);}
     | '[' temps exprs ']'               { $$ = mkBlockExpr(NULL, $2,   $3);}
     | '[' blockargs ']'                 { $$ = mkBlockExpr($2,   NULL, NULL);}
     | '[' blockargs '|' exprs ']'       { $$ = mkBlockExpr($2,   NULL, $4);}
     | '[' blockargs '|' temps exprs ']' { $$ = mkBlockExpr($2,   $4,   $5);}

blockargs: colonvar           { $$ = mkBlockArg($1); }
         | blockargs colonvar { addBlockArg($1, mkBlockArg($2)); $$ = $1; }

/******************************************************************************
 * const arrays
 *****************************************************************************/
arrayconst: '#' array { $$ = mkArrayConst($2); }
array:  '(' arrayelements ')' { $$ = mkArray($2); }
     |  '(' ')'               { $$ = NULL; }

arrayelement: literals | array
arrayelements:
        arrayelement          { $$ = $1; }
      | arrayelements arrayelement    {
            struct ExprUnit *last = $1;
            while (last->next) last = last->next;
            last->next = $2;
            $$ = $1; }

/* todo: arrayconstructor arrayconst bytearray array numbr arraysym exp
         binding eval */
unit: id | literals | block | '(' expr1 ')' { $$ = $2;}

literals: integer | string | charconst | symconst | arrayconst

integer:   INTEGER    { $$ = mkIntExpr(strdup2(yytext)); }
string:    STRING     { $$ = mkStringExpr(st_string); }
charconst: CHARCONST  { $$ = mkCharExpr(strdup2(yytext)); }
symconst:  SYMCONST   { $$ = mkSymbolExpr(strdup2(yytext)); }
id:        IDENTIFIER { $$ = mkIdExpr(strdup2(yytext)); }
keysel:    KEYWORD    { $$ = strdup2(yytext); }
colonvar:  COLONVAR   { $$ = strdup2(yytext); }

binsel: binops { $$ = st_op; };
binops: ',' | '-' | '*' | '/' | '<' | LESS_OR_EQUAL | '>' |
        GREATER_OR_EQUAL | '=' | NOT_EQUAL | MODULO | '@' | OBJECT_EQUALS |
        OBJECT_NOT_EQUALS | '+' | '&' | '|'

%%
void
checkStr(char *a, char *b)
{
    if (strcmp(a,b) != 0) {
        fprintf(stderr, "%s: expected: %s but have %s near %i\n",
                                    fname, b, a, yylineno);
        exit(2);
    }
}

void
addAssignsTo(struct ExprUnit *exp, struct ExprUnit *id)
{
    struct ExprUnit *last = exp->assignsTo;
    while (last->next) last = last->next;
    last->next = id;
}

struct ExprUnit*
allocExprUnit(int type)
{
    struct ExprUnit *t = malloc(sizeof(struct ExprUnit));
    t->type = type;
    t->cascade = NULL;
    t->next = NULL;
    t->assignsTo = NULL;
    t->returns = 0;
    return t; }

struct ExprUnit*
mkArrayConst(struct ExprUnit* head)
{
    struct ExprUnit *t = allocExprUnit(ST_ARRAYCONST);
    t->u.array.head = head;
    return t;
}

struct ExprUnit*
mkArray(struct ExprUnit* head)
{
    struct ExprUnit *t = allocExprUnit(ST_ARRAY);
    t->u.array.head = head;
    return t;
}

struct ExprUnit*
mkSymbolExpr(char*v)
{
    struct ExprUnit *t = allocExprUnit(ST_SYMBOL);
    t->u.symbol.value = v;
    return t;
}

struct ExprUnit*
mkIntExpr(char*v)
{
    struct ExprUnit *t = allocExprUnit(ST_CHAR);
    t->u.integer.value = v;
    return t;
}

struct ExprUnit*
mkCharExpr(char*v)
{
    struct ExprUnit *t = allocExprUnit(ST_CHAR);
    t->u.character.value = v;
    return t;
}

struct ExprUnit*
mkStringExpr(char *v)
{
    struct ExprUnit *t = allocExprUnit(ST_STRING);
    t->u.string.value = v;
    return t;
}

struct BlockArg*
mkBlockArg(char *value)
{
    struct BlockArg *t = malloc(sizeof(struct BlockArg));
    t->next = NULL;
    t->name = value;
    return t;
}

void
addBlockArg(
        struct BlockArg *v,
        struct BlockArg *new)
{
    struct BlockArg *last = v;
    while (last->next) last = last->next;
    last->next = new;
}

struct ExprUnit*
mkBlockExpr(
        struct BlockArg *args,
        struct Temp     *tmps,
        struct ExprUnit *exprs)
{
    struct ExprUnit *t = allocExprUnit(ST_BLOCK);
    t->u.block.args  = args;
    t->u.block.temps = tmps;
    t->u.block.exprs = exprs;
    return t;
}

void
addExpr(
        struct ExprUnit *e,
        struct ExprUnit *next)
{
    struct ExprUnit *last = e;
    while (last->next) last = last->next;
    last->next = next;
}

void
addKeywordMsg(
        struct KeywordMsg *m,
        struct KeywordMsg *new)
{
   struct KeywordMsg *last = m;
   while (last->next) last = last->next;
   last->next = new;
}

void
addBinaryMsg(
        struct BinaryMsg *m,
        struct BinaryMsg *new)
{
   struct BinaryMsg *last = m;
   while (last->next) last = last->next;
   last->next = new;
}

void
addUnaryMsg(
        struct ExprUnit *u,
        struct UnaryMsg *new)
{
    if (! u->u.unary.msgs) {
        u->u.unary.msgs = new;
        return;
    }
    struct UnaryMsg *last = u->u.unary.msgs;
    while(last->next) last = last->next;
    last->next = new;
}

struct KeywordMsg*
mkKeywordMsg(
        char            *keysel,
        struct ExprUnit *val)
{
    struct KeywordMsg *t = malloc(sizeof(struct KeywordMsg));
    t->next = NULL;
    t->key = keysel;
    t->arg = val;
    return t;
}

void
addKeywordCascade(
        struct ExprUnit   *e,
        struct KeywordMsg *m)
{
    struct Cascade *t = malloc(sizeof(struct Cascade));
    t->type = CASCADE_KEYWORD;
    t->next = NULL;
    t->u.keyword = m;
    if (!e->cascade)
        e->cascade = t;
    else {
        struct Cascade *last = e->cascade;
        while (last->next) last = last->next;
        last->next = t;
    }
}

void
addBinaryCascade(
        struct ExprUnit  *ev,
        struct BinaryMsg *bin)
{
    struct Cascade *t = malloc(sizeof(struct Cascade));
    t->type = CASCADE_BINARY;
    t->next = NULL;
    t->u.binary = bin;
    if (!ev->cascade)
        ev->cascade = t;
    else {
        struct Cascade *last = ev->cascade;
        while (last->next) last = last->next;
        last->next = t;
    }
}

void
addUnaryCascade(
        struct ExprUnit *ev,
        struct ExprUnit *unary)
{
    struct Cascade *t = malloc(sizeof(struct Cascade));
    t->type = CASCADE_UNARY;
    t->next = NULL;
    assert(unary->type == ST_ID);
    t->u.unary = unary->u.id.name;
    freeExprUnit(unary);
    if (! ev->cascade)
        ev->cascade = t;
    else {
        struct Cascade *last = ev->cascade;
        while (last->next) last = last->next;
        last->next = t;
    }
}

struct BinaryMsg*
mkBinaryMsg(
        int              op,
        struct ExprUnit *e)
{
    struct BinaryMsg *t = malloc(sizeof(struct BinaryMsg));
    t->op = op;
    t->arg = e;
    t->next = NULL;
    return t;
}

struct ExprUnit*
mkIdExpr(char*v)
{
    struct ExprUnit *t = allocExprUnit(ST_ID);
    t->u.id.name = v;
    return t;
}

struct UnaryMsg*
mkUnaryMsg(struct ExprUnit *v)
{
    struct UnaryMsg *t = malloc(sizeof(struct UnaryMsg));
    t->msg = v;
    t->next = NULL;
    return t;
}

struct Temp*
mkTemp(struct ExprUnit *v)
{
    struct Temp *t = malloc(sizeof(struct Temp));
    t->next = NULL;
    assert(v->type == ST_ID);
    t->name = strdup2(v->u.id.name);
    freeExprUnit(v);
    return t;
}

struct ExprUnit*
mkKeywordExpr(
        struct ExprUnit   *receiver,
        struct KeywordMsg *args)
{
    struct ExprUnit *e = allocExprUnit(ST_KEYWORD);
    e->u.keyword.receiver = receiver;
    e->u.keyword.msgs = args;
    return e;
}

struct ExprUnit*
mkBinaryExpr(
        struct ExprUnit  *receiver,
        struct BinaryMsg *msg)
{
    struct ExprUnit *e = allocExprUnit(ST_BINARY);
    e->u.binary.receiver = receiver;
    e->u.binary.msgs = msg;
    return e;
}

struct ExprUnit*
mkUnaryExpr(
        struct ExprUnit *receiver,
        struct UnaryMsg *msg)
{
    struct ExprUnit *e = allocExprUnit(ST_UNARY);
    e->u.unary.receiver = receiver;
    e->u.unary.msgs     = msg;
    return e;
}

struct MethodDef*
mkUnaryMethodDef(struct ExprUnit* msg)
{
    struct MethodDef *d = malloc(sizeof(struct MethodDef));
    d->type = ST_UNARY;
    assert(msg->type == ST_ID);
    d->u.unary.name = strdup2(msg->u.id.name);
    freeExprUnit(msg);
    return d;
}

struct MethodDef*
mkBinaryMethodDef(
        int              c,
        struct ExprUnit *arg)
{
    struct MethodDef *d = malloc(sizeof(struct MethodDef));
    d->type = ST_BINARY;
    d->u.binary.op = c;
    assert(arg->type == ST_ID);
    d->u.binary.arg = strdup2(arg->u.id.name);
    freeExprUnit(arg);
    return d;
}

struct MethodDef*
mkKeywordMethodDef(struct KeywordMsg *keys)
{
    struct MethodDef *d = malloc(sizeof(struct MethodDef));
    d->type = ST_KEYWORD;
    d->u.keyword.keys = keys;
    return d;
}

struct MethodCategory*
mkMethodCategory(
        struct ExprUnit *classname,
        struct ExprUnit *category,
        struct Method   *methods)
{
    struct MethodCategory *c = malloc(sizeof(struct MethodCategory));
    c->classname = classname; /* XX todo extract str */

    assert(category->type == ST_STRING);
    c->name = strdup2(category->u.string.value);
    freeExprUnit(category);

    c->methods = methods;
    c->next = NULL;

    return c;
}
void freeExprUnit(struct ExprUnit *expr)
{
    switch (expr->type) {
        case ST_ID:
            free(expr->u.id.name);
            free(expr);
            break;
        case ST_STRING:
            free(expr->u.string.value);
            free(expr);
            break;
        case ST_SYMBOL:
            free(expr->u.symbol.value);
            free(expr);
            break;
    }
}

struct ClassFile*
mkFile(
        struct ClassHeader      *head,
        struct ClassComment     *comment,
        struct MethodCategory   *category,
        struct ClassClassHeader *clshead,
        struct MethodCategory   *clscategories)
{
    struct ClassFile *f = malloc(sizeof(struct ClassFile));
    f->header = head;

    assert(strcmp(comment->className, &head->className[1]) == 0);
    f->comment = comment->str;
    free(comment);
    f->categories = category;
    f->classHeader = clshead;
    f->classCategories = clscategories;
    parsed_file = f;
    return f;
}

int tokenizeStString(char *str, char ***dst)
{
    int tsize = 0;
    int tnum  = 0;
    char **tokens = NULL;
    char  *tok;
    tok = strtok(str, " ");
    while (tok) {
        if (tsize == tnum) {
            tokens = realloc(tokens, sizeof(char*) + 10);
            tsize += 10;
        }
        tokens[tnum++] = strdup2(tok);
        tok = strtok(NULL, " ");
    }
    *dst = tokens;
    return tnum;

}

struct ClassHeader*
mkClassHeader(
        struct ExprUnit *super,
        struct ExprUnit *classname,
        struct ExprUnit *instvars,
        struct ExprUnit *classvars,
        struct ExprUnit *pooldict,
        struct ExprUnit *category)
{
    struct ClassHeader *h = malloc(sizeof(struct ClassHeader));

    assert((super->type    == ST_ID) && (!super->next));
    h->super = strdup2(super->u.id.name);
    freeExprUnit(super);

    assert(classname->type == ST_SYMBOL);
    h->className = strdup2(classname->u.symbol.value);
    freeExprUnit(classname);

    assert(instvars->type  == ST_STRING);
    h->instsVarNamesCount = tokenizeStString(instvars->u.string.value, &h->instsVarNames);
    freeExprUnit(instvars);

    assert(classvars->type == ST_STRING);
    h->classVarNamesCount = tokenizeStString(classvars->u.string.value, &h->classVarNames);
    freeExprUnit(classvars);

    assert(pooldict->type  == ST_STRING);
    h->poolDictsCount = tokenizeStString(pooldict->u.string.value, &h->poolDicts);
    freeExprUnit(pooldict);

    assert(category->type  == ST_STRING);
    h->category = strdup2(category->u.string.value);
    freeExprUnit(category);
    return h;
}

struct ClassComment*
mkComment(
        struct ExprUnit *className,
        struct ExprUnit *comment)
{
    struct ClassComment *c = malloc(sizeof(struct ClassComment));

    assert(className->type == ST_ID);
    c->className = strdup2(className->u.id.name);
    freeExprUnit(className);

    assert(comment->type == ST_STRING);
    c->str = strdup2(comment->u.string.value);
    freeExprUnit(comment);
    return c;
}

struct Method*
mkMethod(
        struct MethodDef *def,
        struct ExprUnit  *prim,
        struct Temp      *t,
        struct ExprUnit  *exprs)
{
    struct Method *m = malloc(sizeof(struct Method));
    m->next = NULL;
    m->buffsize  = 0;
    m->bytecount = 0;
    m->bytecodes = NULL;
    m->buffsize = 0;
    m->prim = prim;
    m->temps = t;
    m->exprs = exprs;
    m->def = def;
    return m;
}

struct ClassClassHeader*
mkClassClassHeader(
        struct ExprUnit *name,
        struct ExprUnit *instvars)
{
    struct ClassClassHeader *c = malloc(sizeof(struct ClassClassHeader));
    assert(name->type == ST_ID);
    c->className = strdup2(name->u.id.name);
    freeExprUnit(name);

    assert(instvars->type == ST_STRING);
    c->instVarNamesCount = tokenizeStString(instvars->u.string.value, &c->instVarNames);
    freeExprUnit(instvars);

    return c;
}

