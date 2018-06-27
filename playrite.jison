
%lex

%options case-insensitive

digits                  (-?\d+(\.\d+)?(e[-+]\d+)?|&[HO][0-9a-f]+)&?
string                  ["].*?["]
var_name                [$%](\w|[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FFF\uF900-\uFAFF！？])+
ident                   (\w|[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FFF\uF900-\uFAFF！？])+

%x COMMENT STRING SKIP_EOS

%%

<COMMENT>\r?\n          {this.popState(); return 'EOS';}
<COMMENT>[^\r\n]*         {  return 'COMMENT' }
//["]                     {this.pushState('STRING');}
//<STRING>[^"]*           { return 'STRING';}
//<STRING>["]             { this.popState(); }
<SKIP_EOS>\r?\n           { this.popState(); }
\r?\n                   return 'EOS';
"!!"                    { this.pushState('COMMENT'); }

\\\\                      { this.pushState('SKIP_EOS') }

{string}                    return 'STRING';
{digits}                  return 'DIGITS';
{var_name}                return 'VAR_NAME';

// keywords
"end"\s+"if"\b            return 'END_IF';
"end"\s+"sub"\b           return 'END_SUB';
"end"\s+"function"\b      return 'END_FUNCTION';
"end"\s+"select"\b        return 'END_SELECT';
"exit"\s+"sub"\b          return 'EXIT_SUB';
"exit"\s+"function"\b     return 'EXIT_FUNCTION';
"exit"\s+"foreach"\b      return 'EXIT_FOREACH';
"exit"\s+"for"\b          return 'EXIT_FOR';
"exit"\s+"do"\b           return 'EXIT_DO';
"exit"\b                  return 'EXIT';
"select"\s+"case"\b       return 'SELECT_CASE';
"case"\s+"else"\b         return 'CASE_ELSE';
"case"\b                  return 'CASE';
"elseif"\b                return 'ELSE_IF';
"else"\b                  return 'ELSE';
"if"\b                    return 'IF';
"call"\b                  return 'CALL';
"foreach"\b               return 'FOR_EACH';
"for"\b                   return 'FOR';
"to"\b                    return 'TO';
"in"\b                    return 'IN';
"next"\b                  return 'NEXT';
"do"\b                    return 'DO';
"while"\b                 return 'WHILE';
"until"\b                 return 'UNTIL';
"loop"\b                  return 'LOOP';
"true"\b                  return 'TRUE';
"false"\b                 return 'FALSE';
"nil"\b                   return 'NIL';
"sub"\b                   return 'SUB';
"function"\b              return 'FUNCTION';

// operators
","			return 'COMMA';
"("			return 'LBRACE';
")"			return 'RBRACE';
">="		return 'GE';
"<="		return 'LE';
"<>"		return 'NE';
"="			return 'EQ';
"<"			return 'LT';
">"			return 'GT';
"and"		return 'AND';
"or"		return 'OR';
"xor"		return 'XOR';
"not"		return 'NOT';
"mod"		return 'MOD';
"*"			return 'MUL';
"/"			return 'DIV';
"+"			return 'ADD';
"&"			return 'CAT';
"-"			return 'SUB';

{ident}     return 'IDENT';
\s+        /* eat up spaces */
.           { console.error('INVALID', yytext); return 'INVALID' };
<<EOF>>     return 'EOF';

/lex

%left COMMA
%left OR
%left AND
%left XOR
%nonassoc EQ NE
%nonassoc LT GT LE GE
%left ADD SUB CAT
%left MUL DIV MOD
%nonassoc NOT
%nonassoc NEG
%nonassoc FUNCALL

%start program

%%

program: statements EOF { $$ = $1; return $$; }
;

statements: statement { $$ = [$1];  }
| statements statement { $1.push($2); $$ = $1; }
;

statement: EOS             { $$ = "" }
| COMMENT EOS           { $$ = "//" + $1 }
| stmt EOS                { $$ = $1 }
| stmt COMMENT EOS      { $$ = $1 + "//" + $2 }
;

stmt: proccall
| call_stmt
| assign
| select_case
| case
| if
| elseif
| else
| for
| foreach
| do
| do_while
| do_until
| stmt_preamble
| END_IF                      { $$ = "}" }
| NEXT                        { $$ = "}"}
| LOOP                        { $$ = "}" }
| EXIT_FOR                    { $$ = "break"}
| EXIT_FOREACH                    { $$ = "break"}
| EXIT_DO                     { $$ = "break"}
| EXIT_SUB                  { $$ = "return"}
| EXIT_FUNCTION             { $$ = "return _prr_result" }
| END_SELECT                  { $$ = "}" }
| END_SUB                        { $$ = "}" }
| END_FUNCTION                  { $$ = "return _prr_result\r\n}"}
;

if: IF expr                 { $$ = "if(" + $2 + ") {" }
;
select_case: SELECT_CASE expr           { $$ = "switch(" + $2 + ") {"}
;
else: ELSE                               { $$ = "} else {" }
;
elseif: ELSE_IF expr                { $$ = "} else if(" + $2 + ") {"  }
;
case: CASE expr                           { $$ = "case " + $2 + ":" }
| CASE_ELSE                               { $$ = "default:" }
;
for: FOR VAR_NAME EQ expr TO expr { $$ = "for(let " + $2 + " = " + $4 + "; " + $2 + " <= " + $6 + "; " + $2 + "++) {" }
;
foreach: FOR_EACH VAR_NAME IN var_ref { $$ = "for(let " + $2 + " of " + $4 + ") {"}
;
do_while: DO WHILE expr           { $$ = "while(" + $3 + ") {" }
;
do_until: DO UNTIL expr           { $$ = "while(!" + $3 + ") {" }
;
do: DO                              { $$ = "while(1) {"}
;

assign: assign_left EQ expr           { $$ = $1 + " = " + $3 }
;
assign_left: var_or_func            { $$ = [$1] }
| assign_left COMMA var_or_func    { $1.push($3); $$ = $1 }
;
var_or_func: var_ref
| IDENT                             { $$ = "_prr_result" }
;

stmt_preamble: proc IDENT decl_args { $$ = "function " + $2 + "(" + $3 + ") {" }
;
proc: SUB
| FUNCTION
;

value:  LBRACE expr RBRACE      { $$ = $2 }
| STRING                          { $$ = yytext }
| DIGITS                          { $$ = yytext }
| var_ref
| TRUE                            { $$ = "true" }
| FALSE                           { $$ = "false" }
| NIL                               { $$ = "null" }
;

var_ref: VAR_NAME                        { $$ = $1 } /* simple reference */
| VAR_NAME LBRACE arglist RBRACE        { $$ = $1 + "[" + $3 + "]" } /* array reference */
;

proccall: IDENT call_args           { $$ = $1 + "(" + $2 + ")" }
;
call_stmt: CALL IDENT call_args     { $$ = $2 + "(" + $3 + ")" }
;
funcall: IDENT decl_args            { $$ = $1 + "(" + $2 + ")" }
;

decl_args:                           { $$ = "" }
| LBRACE arglist RBRACE { $$ = $2 }
;

call_args: arglist                   { $$ = $1 }
| LBRACE arglist RBRACE             { $$ = $2 }
;

arglist:                               { $$ = [] }
| arglist_m
;

arglist_m: expr                 { $$ = [$1]; }
| arglist_m COMMA expr         { $1.push($3); $$ = $1 }
;

expr: funcall
| unary
| binary
| value
;

unary: NOT expr       { $$ = ['!', $2].join("") }
| SUB expr %prec NEG      { $$ = ['-', $2].join("") }
;

binary: expr ADD expr { $$ = [$1, '+', $3].join(" ") }
| expr SUB expr       { $$ = [$1, '-', $3].join(" ") }
| expr MUL expr       { $$ = [$1, '*', $3].join(" ") }
| expr DIV expr       { $$ = [$1, '/', $3].join(" ") }
| expr MOD expr       { $$ = [$1, '%', $3].join(" ") }
| expr CAT expr       { $$ = [$1, '+', $3].join(" ") }
| expr AND expr       { $$ = [$1, '&&', $3].join(" ") }
| expr OR expr        { $$ = [$1, '||', $3].join(" ") }
| expr XOR expr       { $$ = [$1, '^', $3].join(" ") }
| expr EQ expr        { $$ = [$1, '==', $3].join(" ") }
| expr NE expr        { $$ = [$1, '!=', $3].join(" ") }
| expr LT expr        { $$ = [$1, '<', $3].join(" ") }
| expr GT expr        { $$ = [$1, '>', $3].join(" ") }
| expr LE expr        { $$ = [$1, '<=', $3].join(" ") }
| expr GE expr        { $$ = [$1, '>=', $3].join(" ") }
;
