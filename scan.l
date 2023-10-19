%{

#include "parse.tab.h"

char *st_string = NULL;
int   st_string_max = 100;
int   st_op;

%}
%option yylineno
%option never-interactive

ID    [a-zA-Z][a-zA-Z0-9]*
KW    {ID}":"

%%

{ID}       { return IDENTIFIER; }
{KW}       { return KEYWORD;  }
{KW}+      { return ARRAYSYM; }

":"{ID}    { return COLONVAR; }

"#"{ID}    { return SYMCONST; }
"#"({KW}+) { return SYMCONST; }

"-"?[0-9]+ { return INTEGER; }
"$"[!-~]   { return CHARCONST; }

"^"  { return '^'; }
":=" { return ST_ASSIGN; }
"_"  { return ST_ASSIGN; }
"#"  { return '#'; }
"("  { return '('; }
")"  { return ')'; }
"["  { return '['; }
"]"  { return ']'; }
"!"  { return '!'; }
"."  { return '.'; }
";"  { return ';'; }

"|"  { st_op = '|'; return '|'; }
"&"  { st_op = '&'; return '&'; }
","  { st_op = ','; return ','; }
"+"  { st_op = '+'; return '+'; }
"-"  { st_op = '-'; return '-'; }
"*"  { st_op = '*'; return '*'; }
"/"  { st_op = '/'; return '/'; }
"<"  { st_op = '<'; return '<'; }
"<=" { st_op = LESS_OR_EQUAL; return LESS_OR_EQUAL; }
">"  { st_op = '>'; return '>'; }
">=" { st_op = GREATER_OR_EQUAL; return GREATER_OR_EQUAL; }
"="  { st_op = '='; return '='; }
"~=" { st_op = NOT_EQUAL; return NOT_EQUAL; }
"//" { st_op = MODULO; return MODULO; }
"@"  { st_op = '@'; return '@'; }
"==" { st_op = OBJECT_EQUALS; return OBJECT_EQUALS; }
"~~" { st_op = OBJECT_NOT_EQUALS; return OBJECT_NOT_EQUALS; }

"'"  {
    int stmax = st_string_max;
    st_string = malloc(stmax);

    int pos = 0;
    int c1;
    int c2;
    for (;;) {
        if (pos == stmax) {
            stmax *= 2;
            st_string = realloc(st_string, stmax);
        }
        c1 = input();
        if (c1 != '\'') {
            // not ' continue
            st_string[pos++] = c1;
            continue;
        }

        c2 = input();
        if (c2 == '\'') {
            // agin ' c1 was an escape thing we want c2 in
            st_string[pos++] = c2;
        } else {
            // c1 was the end of string c2 is something else
            st_string[pos] = '\0';
            // put the character c2 back in string
            unput(c2);
            break;
        }
    }
    return STRING;
}

"\"" {
    /* heat all comments */
    int c;
    do {
        c = input();
    } while((c != '"') && (c != EOF));
}

\n { /* without it yylineno never changes */ }

[[:space:]] {}


%%


