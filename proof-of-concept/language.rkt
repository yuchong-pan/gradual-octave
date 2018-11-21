#lang brag

octave
        : translation_unit
        | octave translation_unit
        ;

translation_unit
        : statement_list
        | FUNCTION function_declare eostmt statement_list eostmt ENDFUNCTION eostmt
        ;

typed_identifier
        : IDENTIFIER
        | IDENTIFIER ':' TYPE
        ;

primary_expression
        : typed_identifier                  
        | BOOLEAN
        | CONSTANT    
        | STRING_LITERAL        
        | '(' expression ')'
        | '[' ']'
        | '[' array_list ']'
        ;

postfix_expression
        : primary_expression
        | array_expression
        | postfix_expression TRANSPOSE
        | postfix_expression NCTRANSPOSE
        ;

index_expression
        : ':'
        | expression
        ;

index_expression_list
        : index_expression
        | index_expression_list ',' index_expression
        ;

array_expression
        : typed_identifier '(' index_expression_list ')'
        ;

unary_expression
        : postfix_expression
        | unary_operator postfix_expression
        ;

unary_operator
        : '+'
        | '-'
        | '~'
        ;

multiplicative_expression
        : unary_expression
        | multiplicative_expression '*' unary_expression
        | multiplicative_expression '/' unary_expression
        | multiplicative_expression '\\' unary_expression
        | multiplicative_expression '^' unary_expression
        | multiplicative_expression ARRAYMUL unary_expression
        | multiplicative_expression ARRAYDIV unary_expression
        | multiplicative_expression ARRAYRDIV unary_expression
        | multiplicative_expression ARRAYPOW unary_expression
        ;

additive_expression
        : multiplicative_expression
        | additive_expression '+' multiplicative_expression
        | additive_expression '-' multiplicative_expression
        ;

relational_expression
        : additive_expression
        | relational_expression '<' additive_expression
        | relational_expression '>' additive_expression
        | relational_expression LE_OP additive_expression
        | relational_expression GE_OP additive_expression
        ;

equality_expression
        : relational_expression
        | equality_expression EQ_OP relational_expression
        | equality_expression NE_OP relational_expression
        ;

and_expression
        : equality_expression
        | and_expression '&' equality_expression
        ;

or_expression
        : and_expression
        | or_expression '|' and_expression
        ;

expression
        : or_expression
    | expression ':' or_expression
    ;

assignment_expression
        : postfix_expression '=' expression

eostmt
        :  ','
        |  ';'
        |  CR
        ;

statement
        : global_statement
        | clear_statement
        | assignment_statement
        | expression_statement
        | selection_statement
        | iteration_statement
        | jump_statement
        ;

statement_list
        : statement
        | statement_list statement
        ;

identifier_list
        : typed_identifier
        | identifier_list typed_identifier
        ;

global_statement
        : GLOBAL identifier_list eostmt
        ;

clear_statement
        : CLEAR identifier_list eostmt
        ;

expression_statement
        : eostmt
        | expression eostmt
        ;

assignment_statement
        : assignment_expression eostmt
        ;

array_element
        : expression
        | expression_statement
        ;

array_list
        : array_element
        | array_list array_element
        ;

selection_statement
        : IF expression CR statement_list END CR
        | IF expression CR statement_list ELSE statement_list END CR
        | IF expression CR statement_list elseif_clause END CR
        | IF expression CR statement_list elseif_clause
          ELSE statement_list END CR
        ;

elseif_clause
        : ELSEIF expression statement_list
    | elseif_clause ELSEIF expression statement_list
        ;

iteration_statement
        : WHILE expression statement_list END eostmt
        | FOR typed_identifier '=' expression statement_list END eostmt
        | FOR '(' typed_identifier '=' expression ')' statement_list END eostmt 
        ;

jump_statement
        : BREAK eostmt
        | RETURN eostmt
        ;

func_ident_list
        : typed_identifier
        | func_ident_list ',' typed_identifier
        ;

func_return_list
        : typed_identifier
        | '[' func_ident_list ']'
        ;

function_declare_lhs
        : typed_identifier
        | typed_identifier '(' ')'
        | typed_identifier '(' func_ident_list ')'
        ;

function_declare
        : function_declare_lhs
        | func_return_list '=' function_declare_lhs
        ;
