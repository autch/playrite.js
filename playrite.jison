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

program: defs_or_stmts EOF { $$ = $1; return $$; }
;

defs_or_stmts: definition   { $$ = [$1]; }
| statement                 { $$ = [$1]; }
| defs_or_stmts definition  { $1.push($2); $$ = $1; }
| defs_or_stmts statement   { $1.push($2); $$ = $1; }
;

definition: defun
| defsub
;

eos: EOS                    { $$ = [] }
| COMMENT EOS               { $$ = ['COMMENT', $1 ]}
;

statement: eos
| stmt eos                { $$ = [$1, $2] }
;

flow_control: select_case
| if
| for
| foreach
| loops
;

stmt: flow_control
| proccall
| call_stmt
| assign
| EXIT_FOR                    { $$ = ['EXIT_FOR']}
| EXIT_FOREACH                    { $$ = ['EXIT_FOREACH']}
| EXIT_DO                     { $$ = ['EXIT_DO']}
| EXIT_SUB                  { $$ = ["EXIT_SUB"]}
| EXIT_FUNCTION             { $$ = ["EXIT_FUNCTION"] }
;

defun: FUNCTION IDENT decl_args eos suite END_FUNCTION  { $$ = ['DEFUN', $2, $3, $5 ] }
;
defsub: SUB IDENT decl_args eos suite END_SUB           { $$ = ['DEFSUB', $2, $3, $5] }
;
suite: statement            {$$ = [$1]; }
| suite statement           {$1.push($2); $$ = $1; }
;

if: IF expr eos suite elseif_list else END_IF           { $$ = ['IF', $2, $4, $5, $6 ]}
;
elseif_list: elseif         { $$ = [$1]; }
| elseif_list elseif        { $1.push($2); $$ = $1; }
;
elseif:                     { $$ = null;}
| ELSE_IF expr eos suite    { $$ = ['ELSE_IF', $2, $4] }
;
else:                       { $$ = null; }
| ELSE eos suite            { $$ = ['ELSE', $3] }
;

select_case: SELECT_CASE expr eos cases END_SELECT  { $$ = ['SELECT', $2, $4 ]; }
;
cases: case                 { $$ = [$1]; }
| cases case                { $1.push($2); $$ = $1; }
;
case: CASE expr eos suite   { $$ = ['CASE', $2, $4]; }
| CASE_ELSE eos suite       { $$ = ['CASE_ELSE', $3]; }
;

for: FOR VAR_NAME EQ expr TO expr eos suite NEXT { $$ = ['FOR', $2, $4, $6, $8]}
;
foreach: FOR_EACH VAR_NAME IN var_ref eos suite NEXT { $$ = ['FOR_EACH', $2, $4, $6]}
;
loops: DO eos suite LOOP            { $$ = ['LOOP', $3 ] }
| DO cond eos suite LOOP            { $$ = ['WHILE', $2, $4 ]}
| DO eos suite LOOP cond            { $$ = ['DO_WHILE', $5, $3 ]}
;
cond: WHILE expr                    { $$ = ['WHILE', $2] }
| UNTIL expr                        { $$ = ['UNTIL', $2] }
;

assign: assign_left EQ expr           { $$ = ['LET', $1, $3] }
;
assign_left: var_or_func            { $$ = [$1] }
| assign_left COMMA var_or_func    { $1.push($3); $$ = $1 }
;
var_or_func: var_ref
| IDENT                             { $$ = ['SET_RESULT', $1] }
;

value:  LBRACE expr RBRACE      { $$ = $2 }
| STRING                          { $$ = yytext }
| DIGITS                          { $$ = Number(yytext); }
| var_ref
| TRUE                            { $$ = 'true' }
| FALSE                           { $$ = 'false' }
| NIL                               { $$ = "null" }
;

var_ref: VAR_NAME                        { $$ = ['REF', $1] } /* simple reference */
| VAR_NAME LBRACE arglist RBRACE        { $$ = ['REF', $1, $3] } /* array reference */
;

proccall: IDENT call_args           { $$ = ['CALL', $1, $2] }
;
call_stmt: CALL IDENT call_args     { $$ = ['CALL', $2, $3] }
;
funcall: IDENT decl_args            { $$ = ['FUNCALL', $1, $2] }
;

decl_args:                           { $$ = [] }
| LBRACE arglist RBRACE              { $$ = $2 }
;

call_args: arglist                   { $$ = $1 }
| LBRACE arglist RBRACE              { $$ = $2 }
;

arglist:                               { $$ = [] }
| arglist_m
;

arglist_m: expr                      { $$ = [$1]; }
| arglist_m COMMA expr               { $1.push($3); $$ = $1 }
;

expr: funcall
| unary
| binary
| value
;

unary: NOT expr       { $$ = ['!', $2] }
| SUB expr %prec NEG      { $$ = ['-', $2] }
;

binary: expr ADD expr { $$ = ['+', $1, $3] }
| expr SUB expr       { $$ = ['-', $1, $3] }
| expr MUL expr       { $$ = ['*', $1, $3] }
| expr DIV expr       { $$ = ['/', $1, $3] }
| expr MOD expr       { $$ = ['%', $1, $3] }
| expr CAT expr       { $$ = ['+', $1, $3] }
| expr AND expr       { $$ = ['&&', $1, $3] }
| expr OR expr        { $$ = ['||', $1, $3] }
| expr XOR expr       { $$ = ['^', $1, $3] }
| expr EQ expr        { $$ = ['==', $1, $3] }
| expr NE expr        { $$ = ['!=', $1, $3] }
| expr LT expr        { $$ = ['<', $1, $3] }
| expr GT expr        { $$ = ['>', $1, $3] }
| expr LE expr        { $$ = ['<=', $1, $3] }
| expr GE expr        { $$ = ['>=', $1, $3] }
;
