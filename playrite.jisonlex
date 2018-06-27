%options case-insensitive

digits                  (-?\d+(\.\d+)?(e[-+]\d+)?|&[HO][0-9a-f]+)&?
string                  ["].*?["]
global_var              [$]{ident}
local_var               [%]{ident}
var_name                ({global_var}|{local_var})
ident                   (\w|[\u3040-\u309F\u30A0-\u30FF\u4E00-\u9FFF\uF900-\uFAFF！？])+

%x COMMENT STRING SKIP_EOS

%%

<COMMENT>\r?\n          {this.popState(); return 'EOS';}
<COMMENT>[^\r\n]*         {  return 'COMMENT' }
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
