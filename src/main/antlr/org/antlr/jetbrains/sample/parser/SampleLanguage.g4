/** A simple language for use with this sample plugin.
 *  It's C-like but without semicolons. Symbol resolution semantics are
 *  C-like: resolve symbol in current scope. If not in this scope, ask
 *  enclosing scope to resolve (recurse up tree until no more scopes or found).
 *  Forward refs allowed for functions but not variables. Globals must
 *  appear first syntactically.
 *
 *  Generate the parser via "mvn compile" from root dir of project.
 */
grammar SampleLanguage;

/** The start rule must be whatever you would normally use, such as script
 *  or compilationUnit, etc...
 */
script
	:	(function|define_behavior|statement|install_behavior)* EOF
	;

function
	:	'def' ID '(' formal_args? ')' block
	;


define_behavior
    : 'behavior' CAMELID paren_args? block ;

install_behavior
    : 'install' CAMELID paren_kwargs? ;

// argument list
formal_args : formal_arg (',' formal_arg)* ;
kwargs: kwarg (',' kwarg)* ;

// parenthesized arguments:
paren_args : '(' formal_args? ')' ;
paren_kwargs : '(' kwargs ')' ;

// non-typed arguments:
formal_arg : ID ;
kwarg : ID ':' expr ;

block
	: BLOCK_START? statement* BLOCK_END;

statement
	:	'if' '('? expr ')'? statement ('else' statement)?	# If
	|	'while' '(' expr ')' statement						# While
	|	var_def  											# Assign
	|	ID '[' expr ']' TO expr							    # ElementAssign
	|	call_expr											# CallStatement
    |   'print' '(' expr? ')'								# Print
    |   'log' expr                                          # Log
	|	'return' expr										# Return
    |   BLOCK_END                                           # NestedBlockEnd
	;

var_def : SET ID TO expr ;

expr
	:	expr operator expr									# Op
	|	'-' expr											# Negate
	|	'!' expr											# Not
	|	call_expr											# Call
	|	ID '[' expr ']'										# Index
	|	LPAREN expr RPAREN									# Parens
	|	primary												# Atom
	;

operator  : MUL|DIV|ADD|SUB|GT|GE|LT|LE|EQUAL_EQUAL|NOT_EQUAL|OR|AND|DOT ; // no implicit precedence

call_expr
	: ID LPAREN expr_list? RPAREN ;


expr_list : expr (',' expr)* ;

dictpair : expr BLOCK_START expr ;

expr_dict : dictpair (',' dictpair)* ;

primary
	:	ID													# Identifier
	|	INT													# Integer
	|	FLOAT												# Float
	|	STRING												# String
	|	array           									# ArrayItem
	|   dict                                                # DictItem
	|	'true'												# TrueLiteral
	|	'false'												# FalseLiteral
	;

array : '[' expr_list ']' ;
dict : '{' expr_dict '}' ;

// custom (custom rules also at the end?):

LPAREN : '(' ;
RPAREN : ')' ;
COLON : 'COLON' ;
COMMA : ',' ;
LBRACK : '[' ;
RBRACK : ']' ;
BLOCK_START : ':' ;
BLOCK_END : 'end' ;
IF : 'if' ;
ELSE : 'else' ;
WHILE : 'while' ;
SET : 'set' ; // Var_defContext
TO : 'to' ;
RETURN : 'return' ;
DEF : 'def' ; // FunctionContext
BEHAVIOR : 'behavior' ; // FunctionContext
PRINT : 'print' ;
LOG : 'log' ;
TYPEINT : 'int' ;
TYPEFLOAT : 'float' ;
TYPESTRING : 'string' ;
TYPEBOOLEAN : 'boolean' ;
TRUE : 'true' ;
FALSE : 'false' ;
SUB : '-' ;
BANG : '!' ;
MUL : '*' ;
DIV : '/' ;
ADD : '+' ;
LT : '<' ;
LE : '<=' ;
EQUAL_EQUAL : '==' ;
NOT_EQUAL : '!=' ;
GT : '>' ;
GE : '>=' ;
OR : '||' ;
AND : '&&' ;
DOT : ' . ' ;

COMMENT      : '---' .*? '---'    	-> channel(HIDDEN) ;
LINE_COMMENT : '--' .*? ('\n'|EOF)	-> channel(HIDDEN) ;


ID  : [a-z] [a-zA-Z0-9_]* ;
CAMELID : [A-Z] [a-zA-Z0-9_]* ;

INT : [0-9]+ ;
FLOAT
	:   '-'? INT '.' INT EXP?   // 1.35, 1.35E-9, 0.3, -4.5
	|   '-'? INT EXP            // 1e10 -3e4
	;
fragment EXP :   [Ee] [+\-]? INT ;

STRING : (STRING_BASIC|STRING_ALT|STRING_INTERPOL) ;
fragment STRING_BASIC :  '"' STRING_CONTENT '"' ; // "string"
fragment STRING_ALT :  '\'' STRING_CONTENT '\'' ; // 'string'
fragment STRING_INTERPOL :  '`' STRING_CONTENT '`' ; // `string`
fragment STRING_CONTENT : (ESC | ~["\\])*;
fragment ESC :   '\\' ["\bfnrt] ;

WS : [ \t\n\r]+ -> channel(HIDDEN) ;

/** "catch all" rule for any char not matche in a token rule of your
 *  grammar. Lexers in Intellij must return all tokens good and bad.
 *  There must be a token to cover all characters, which makes sense, for
 *  an IDE. The parser however should not see these bad tokens because
 *  it just confuses the issue. Hence, the hidden channel.
 */
ERRCHAR
	:	.	-> channel(HIDDEN)
	;