/* This is a yacc-able parser for the entire ISO C++ grammar with no unresolved conflicts. */
/* The parse is SYNTACTICALLY consistent and requires no template or type name assistance.
 * The grammar in the C++ standard notes that its grammar is a superset of the true
 * grammar requiring semantic constraints to resolve ambiguities. This grammar is a really big
 * superset unifying expressions and declarations, eliminating the type/non-type distinction,
 * and iterating to find a consistent solution to the template/arith,metoic < ambiguity.
 * As a result the grammar is much simpler, but requires the missing semantic constraints to be
 * performed in a subsequent semantic pass, which is of course where they belong. This grammar will
 * support conversion of C++ tokens into an Abstract Syntax Tree. A lot of further work is required to
 * make that tree useful.
 *
 * The principles behind this grammar are described in my thesis on Meta-Compilation for C++, which
 * may be found via http://www.computing.surrey.ac.uk/research/dsrg/fog/FogThesis.html.
 *
 *  Author:         E.D.Willink             Ed.Willink@rrl.co.uk
 *  Date:           19-Jun-2001
 */
/*StartTester*/
%{
#include <CxxParsing.hxx>
using namespace std;
%}
/*EndTester*/
/*
 * The lexer (and/or a preprocessor) is expected to identify the following
 *
 *  Punctuation:
 */
%type <keyword> '+' '-' '*' '/' '%' '^' '&' '|' '~' '!' '<'  '>' '=' ':' '[' ']' '{' '}' '(' ')'
%type <keyword> '?' '.' '\'' '\"' '\\' '@' '$' ';' ','
/*
 *  Punctuation sequences
 */
%term <keyword> ARROW ARROW_STAR DEC EQ GE INC LE LOG_AND LOG_OR NE SHL SHR
%term <keyword> ASS_ADD ASS_AND ASS_DIV ASS_MOD ASS_MUL ASS_OR ASS_SHL ASS_SHR ASS_SUB ASS_XOR
%term <keyword> DOT_STAR ELLIPSIS SCOPE
/*
 *  Reserved words
 */
%term <access_specifier> PRIVATE PROTECTED PUBLIC
%term <built_in_id> BOOL CHAR DOUBLE FLOAT INT LONG SHORT SIGNED UNSIGNED VOID WCHAR_T
%term <class_key> CLASS ENUM NAMESPACE STRUCT TYPENAME UNION
%term <cv_qualifiers> CONST VOLATILE
%term <decl_specifier_id> AUTO EXPLICIT EXPORT EXTERN FRIEND INLINE MUTABLE REGISTER STATIC TEMPLATE TYPEDEF USING VIRTUAL
%term <keyword> ASM BREAK CASE CATCH CONST_CAST CONTINUE DEFAULT DELETE DO DYNAMIC_CAST
%term <keyword> ELSE FALSE FOR GOTO IF NEW OPERATOR REINTERPRET_CAST RETURN
%term <keyword> SIZEOF STATIC_CAST SWITCH THIS THROW TRUE TRY TYPEID WHILE
/*
 *  Parametric values.
 */
%term <character_literal> CharacterLiteral
%term <floating_literal> FloatingLiteral
%term <identifier> Identifier
%term <integer_literal> IntegerLiteral
%term <number_literal> NumberLiteral
%term <string_literal> StringLiteral
/*
 *  The lexer need not treat '0' as distinct from IntegerLiteral in the hope that pure-specifier can
 *  be distinguished, It isn't. Semantic rescue from = constant-expression is necessary.
 *
 *  The lexer is not required to distinguish template or type names, although a slight simplification to the
 *  grammar and elaboration of the action rules could make good use of template name information.
 *
 *  In return for not needing to use semantic information, the lexer must support back-tracking, which
 *  is easily achieved by a simple linear buffer, a reference implementation of which may be found in the
 *  accompanying CxxParsing.cxx. Back-tracking is used to support:
 *
 *  Binary search for a consistent parse of the template/arithmetic ambiguity.
 *      start_search() initialises the search
 *      advance_search() iterates the search
 *      end_search() cleans up after a search
 *      template_test() maintains context during a search
 *
 *  Lookahead to resolve the inheritance/anonymous bit-field similarity
 *      mark() saves the starting context
 *      unmark() pops it
 *      rewind_colon() restores the context and forces the missing :
 *
 *  Lookahead to resolve type 1 function parameter ambiguities
 *      mark_type1() potentially marks the starting position
 *      mark() marks the pre { position
 *      remark() rewinds to the starting position
 *      unmark() pops the starting position
 *
 *  Note that lookaheads may nest. 
 */

/*
 *  The parsing philosophy is unusual. The major ambiguities are resolved by creating a unified superset
 *  grammar rather than non-overlapping subgrammars. Thus the grammar for parameter-declaration covers an
 *  assignment-expression. Minor ambiguities whose resolution by supersetting would create more
 *  ambiguities are resolved the normal way with partitioned subgrammars.
 *  This eliminates the traditional expression/declaration and constructor/parenthesised declarator
 *  ambiguities at the syntactic level. A subsequent semantic level has to sort the problems out.
 *  The generality introduces four bogus ambiguities and defers the cast ambiguity for resolution
 *  once semantic information is available.
 *
 *  The C++ grammar comprises 561 rules and uses 897 states in yacc, with 0 unresolved conflicts.
 *  23 conflicts from 10 ambiguities are resolved by 8 %prec's, so that yacc and bison report 0 conflicts.
 *
 *  The ambiguities are:
 *  1) dangling else resolved to inner-most if
 *      1 conflict in 1 state on else
 *  2) < as start-template or less-than
 *      1 conflict in 1 states on <
 *  3) a :: b :: c resolved to favour a::b::c rather than a::b ::c or a ::b::c
 *      1 conflicts in 1 state for ::
 *  4) pointer operators maximised at end of conversion id/new in preference to binary operators
 *      2 conflicts in 4 states on * and &
 *  5a) (a)@b resolved to favour binary a@b rather than cast unary (a)(@b)
 *  5b) (a)(b) resolved to favour cast rather than call
 *      8 conflicts in 1 state for the 8 prefix operators: 6 unaries and ( and [.
 *  6) enum name { resolved to enum-specifier rather than function
 *      1 conflict in 1 state on {
 *  7) class name { resolved to class-specifier rather than function
 *      1 conflict in 1 state on {
 *  8) extern "C" resolved to linkage-specification rather than declaration
 *      1 conflict in 1 state on StringLiteral
 *  9) class X : forced to go through base-clause look-ahead
 *      1 conflict in 1 state on :
 *  10) id : forced to label_statement rather than constructor_head
 *      0 conflicts - but causes a double state for 2)
 *  of which
 *      1 is a fundamental C conflict - always correctly resolved
 *          can be removed - see the Java spec
 *      2, 3, 4 are fundamental C++ conflicts
 *          2 always consistently resolved by iteration
 *          3 always correctly resolved
 *          4 always correctly resolved
 *      5 is a result of not using type information - deferred for semantic repair
 *      6,7 are caused by parsing over-generous superset - always correctly resolved
 *      8 is caused by parsing over-generous superset - always correctly resolved
 *          can be removed at the expense of 7 rules and 5 states.
 *      9 is a look-ahead trick - always correctly resolved
 *          could be removed by marking one token sooner
 *      10 is caused by parsing over-generous superset - always correctly resolved
 *
 *  The hard problem of distinguishing
 *      class A { class B : C, D, E {           -- A::B privately inherits C, D and E
 *      class A { class B : C, D, E ;           -- C is width of anon bit-field
 *  is resolved by using a lookahead that assumes inheritance and rewinds for the bit-field.
 *
 *  The potential shift-reduce conflict on > is resolved by flattening part of the expression grammar
 *  to know when the next > is template end or arithmetic >.
 *
 *  The grammar is SYNTACTICALLY context-free with respect to type. No semantic assistance is required
 *  during syntactic analysis. However the cast ambiguity is deferred and must be recovered
 *  after syntactic analysis of a statement has completed. 
 *
 *  The grammar is SYNTACTICALLY context-free with respect to template-names. This is achieved by
 *  organising a binary search over all possible template/arithmetic ambiguities with respect to
 *  the enclosing statement. This is potentially exponentially inefficient but well-behaved in practice.
 *  Approximately 1% of statements trigger a search and approximately 1% of those are misparsed,
 *  requiring the semantic analysis to check and correct once template information is available.
 *  1.5 parse attempts are required on average per ambiguous statement.
 *
 *  The grammar supports type I function declarations at severe impediment to efficiency. A lookahead
 *  has to be performed after almost every non-statement close parenthesis. A one-line plus corollary
 *  change to postfix_expression is commented and strongly recommended to make this grammar as
 *  efficient as the rather large number of reduction levels permits.
 *
 *  Error recovery occurs mostly at the statement/declaration level. Recovery also occurs at
 *  the list-element level where this poses no hazard to statement/declaration level recovery. 
 *  Note that since error propagation interacts with the lookaheads for template iteration or
 *  type 1 function arguments, introduction of finer grained error recovery may repair a false
 *  parse and so cause a misparse.
 *
 *  The following syntactic analysis errors occur, but are correctable semantically:
 *  (cast)unary-op expr         is parsed as (parenthesised)binary-op expr
 *      The semantic test should look for a binary/call with a (type) as its left child.
 *  (parenthesised)(arguments)  is parsed as (cast)(parenthesised)
 *      The semantic test should look for a cast with a non-type as its left child.
 *  template < and arithmetic < may be cross-parsed (unless semnatic help is provided)
 *      approximately 0.01% are misparsed, and must be sorted out - not easy.
 *
 *  The syntactic analysis defers the following ambiguities for semantic resolution:
 *  declaration/expression is parsed as a unified concept
 *      Use type and context to complete the parse.
 *  ~class-name                 is parsed as unary~ name
 *      The semantic test should look for ~ with a type as its child.
 *  delete[] expr               is parsed as delete []expr
 *      The semantic test should look for delete with a [] cast of its child.
 *  operator new/delete[]       are parsed as array of operator new/delete
 *      The semantic test should look for array of operator new/delete
 *      or activate the two extra commented rules in operator
 *  template of an explicit_instantiation is buried deep in the tree
 *      dig it out 
 *  pure-specifier and constant-initializer are covered by assignment-expression
 *      just another of the deferred declaration/expression ambiguities
 *  sizeof and typeid don't distinguish type/value syntaxes
 *      probably makes life polymorphically easier
 */
/*  Action code is supplied by a large number of YACC_xxx macros that can be redefined
 *  by rewriting the include file rather than the grammar. The number of macros is
 *  slightly reduced by using the following protocols
 *
 *  YACC_LIST(0,0)      create empty list (may safely return 0).
 *  YACC_LIST(0,E)      create new list with content E (may return 0 if above returned non-0).
 *  YACC_LIST(L,E)      add E to L
 *  YACC_LIST(L,0)      error propagation, adding nothing to L.
 */
%type <bang> bang
%type <mark> colon_mark mark mark_type1
%type <nest> nest

%type <access_specifier> access_specifier
%type <base_specifier> base_specifier
%type <base_specifiers> base_specifier_list
%type <built_in_id> built_in_type_id built_in_type_specifier
%type <_class> class_specifier_head
%type <class_key> class_key
%type <condition> condition condition.opt
%type <cv_qualifiers> cv_qualifier cv_qualifier_seq.opt
%type <decl_specifier_id>  decl_specifier_affix decl_specifier_prefix decl_specifier_suffix function_specifier storage_class_specifier
%type <declaration> accessibility_specifier asm_definition block_declaration declaration explicit_specialization
%type <declaration> looped_declaration looping_declaration namespace_alias_definition
%type <declaration> specialised_block_declaration specialised_declaration template_declaration using_directive
%type <declarations> compound_declaration declaration_seq.opt
%type <declarator> nested_ptr_operator ptr_operator
%type <delete_expression> delete_expression
%type <enumerator> enumerator_definition
%type <enumerators> enumerator_clause enumerator_list enumerator_list_head
%type <exception_declaration> exception_declaration
%type <exception_specification> exception_specification
%type <expression> abstract_declarator.opt abstract_expression abstract_parameter_declaration abstract_pointer_declaration
%type <expression> additive_expression and_expression assignment_expression
%type <expression> bit_field_declaration bit_field_init_declaration bit_field_width boolean_literal
%type <expression> cast_expression conditional_expression constant_expression conversion_type_id ctor_definition
%type <expression> direct_abstract_declarator direct_abstract_declarator.opt direct_new_declarator
%type <expression> equality_expression exclusive_or_expression expression expression.opt
%type <expression> for_init_statement func_definition function_definition 
%type <expression> inclusive_or_expression init_declaration literal logical_and_expression logical_or_expression
%type <expression> multiplicative_expression new_declarator new_type_id
%type <expression> pm_expression postfix_expression primary_expression ptr_operator_seq ptr_operator_seq.opt
%type <expression> relational_expression shift_expression simple_declaration special_parameter_declaration
%type <expression> templated_throw_expression throw_expression templated_abstract_declaration templated_and_expression 
%type <expression>templated_assignment_expression templated_conditional_expression templated_equality_expression
%type <expression> templated_exclusive_or_expression templated_expression templated_inclusive_or_expression templated_logical_and_expression
%type <expression> templated_logical_or_expression templated_relational_expression type_id unary_expression
%type <expressions> constructor_head expression_list expression_list.opt init_declarations
%type <expressions> new_initializer.opt templated_expression_list type_id_list
%type <function_body> function_block function_body function_try_block try_block
%type <handler> handler
%type <handlers> handler_seq
%type <initializer_clause> braced_initializer initializer_clause looped_initializer_clause looping_initializer_clause
%type <initializer_clauses> initializer_list
%type <is_template> global_scope
%type <keyword> assignment_operator
%type <line> start_search start_search1
%type <mem_initializer> mem_initializer
%type <mem_initializers> ctor_initializer ctor_initializer.opt mem_initializer_list mem_initializer_list_head
%type <name> class_specifier conversion_function_id declarator_id destructor_id
%type <name> elaborated_class_specifier elaborated_enum_specifier elaborated_type_specifier elaborate_type_specifier
%type <name> enum_specifier enumerator id identifier_word id_scope identifier linkage_specification
%type <name> namespace_definition nested_id nested_pseudo_destructor_id nested_special_function_id
%type <name> mem_initializer_id operator operator_function_id pseudo_destructor_id scoped_id scoped_pseudo_destructor_id scoped_special_function_id
%type <name> simple_type_specifier special_function_id suffix_built_in_decl_specifier suffix_named_decl_specifier.bi
%type <name> suffix_built_in_decl_specifier.raw suffix_decl_specified_ids suffix_named_decl_specifiers
%type <name> suffix_named_decl_specifiers.sf suffix_decl_specified_scope suffix_named_decl_specifier
%type <name> template_id type_specifier
%type <new_expression> new_expression
%type <parameter> parameter_declaration templated_parameter_declaration
%type <parameters> parameters_clause parameter_declaration_clause parameter_declaration_list
%type <parenthesised> parenthesis_clause
%type <pointer_declarator> star_ptr_operator
%type <simple_type_parameter> simple_type_parameter
%type <statement> compound_statement control_statement declaration_statement iteration_statement jump_statement
%type <statement> labeled_statement looped_statement looping_statement selection_statement statement
%type <statements> statement_seq.opt
%type <strings> string
%type <template_argument> template_argument
%type <template_arguments> template_argument_list
%type <template_parameter> template_parameter
%type <template_parameters> template_parameter_clause template_parameter_list
%type <templated_type_parameter> templated_type_parameter
%type <type1_parameters> type1_parameters
%type <utility> util

/*
 *  C++ productions replaced by more generalised FOG productions
 */
%type <declaration> looped_member_declaration looping_member_declaration member_declaration using_declaration
%type <declarations> member_specification.opt
%type <expression> member_init_declaration simple_member_declaration
%type <expressions> member_init_declarations


%nonassoc SHIFT_THERE
%nonassoc SCOPE ELSE INC DEC '+' '-' '*' '&' '[' '{' '<' ':' StringLiteral
%nonassoc REDUCE_HERE_MOSTLY
%nonassoc '('
/*%nonassoc REDUCE_HERE */

%start translation_unit
%%

/*
 *  The %prec resolves a conflict in identifier_word : which is forced to be a shift of a label for
 *  a labeled-statement rather than a reduction for the name of a bit-field or generalised constructor.
 *  This is pretty dubious syntactically but correct for all semantic possibilities.
 *  The shift is only activated when the ambiguity exists at the start of a statement. In this context
 *  a bit-field declaration or constructor definition are not allowed.
 */
identifier_word:                    Identifier                                                  { $$ = $1; }
identifier:                         identifier_word                     %prec SHIFT_THERE
/*
 *  The %prec resolves the $014.2-3 ambiguity:
 *  Identifier '<' is forced to go through the is-it-a-template-name test
 *  All names absorb TEMPLATE with the name, so that no template_test is performed for them.
 *  This requires all potential declarations within an expression to perpetuate this policy
 *  and thereby guarantee the ultimate coverage of explicit_instantiation.
 */
id:                                 identifier                          %prec SHIFT_THERE       /* Force < through test */ { $$ = YACC_NAME($1); }
    |                               identifier template_test '+' template_argument_list '>'     { $$ = YACC_TEMPLATE_NAME($1, $4); }
    |                               identifier template_test '+' '>'                            { $$ = $1; ERRMSG("Empty template-argument-list"); }
    |                               identifier template_test '-'                                /* requeued < follows */  { $$ = YACC_NAME($1); }
    |                               template_id 
template_test:                      '<'             /* Queue '+' or '-' < as follow on */       { template_test(); }
global_scope:                       SCOPE                                                       { $$ = IS_DEFAULT; }
    |                               TEMPLATE global_scope                                       { $$ = IS_TEMPLATE; }
id_scope:                           id SCOPE                                                    { $$ = YACC_NESTED_SCOPE($1); }
/*
 *  A :: B :: C; is ambiguous How much is type and how much name ?
 *  The %prec maximises the (type) length which is the $07.1-2 semantic constraint.
 */
nested_id:                          id                                  %prec SHIFT_THERE       /* Maximise length */
    |                               id_scope nested_id                                          { $$ = YACC_NESTED_ID($1, $2); }
scoped_id:                          nested_id
    |                               global_scope nested_id                                      { $$ = YACC_GLOBAL_ID($1, $2); }

/*
 *  destructor_id has to be held back to avoid a conflict with a one's complement as per $05.3.1-9,
 *  It gets put back only when scoped or in a declarator_id, which is only used as an explicit member name.
 *  Declarations of an unscoped destructor are always parsed as a one's complement.
 */
destructor_id:                      '~' id                                                      { $$ = YACC_DESTRUCTOR_ID($2); }
    |                               TEMPLATE destructor_id                                      { $$ = YACC_SET_TEMPLATE_ID($2); }
special_function_id:                conversion_function_id
    |                               operator_function_id
    |                               TEMPLATE special_function_id                                { $$ = YACC_SET_TEMPLATE_ID($2); }
nested_special_function_id:         special_function_id
    |                               id_scope destructor_id                                      { $$ = YACC_NESTED_ID($1, $2); }
    |                               id_scope nested_special_function_id                         { $$ = YACC_NESTED_ID($1, $2); }
scoped_special_function_id:         nested_special_function_id
    |                               global_scope nested_special_function_id                     { $$ = YACC_GLOBAL_ID($1, $2); }

/* declarator-id is all names in all scopes, except reserved words */
declarator_id:                      scoped_id
    |                               scoped_special_function_id
    |                               destructor_id

/*  The standard defines pseudo-destructors in terms of type-name, which is class/enum/typedef, of which
 *  class-name is covered by a normal destructor. pseudo-destructors are supposed to support ~int() in
 *  templates, so the grammar here covers built-in names. Other names are covered by the lack of
 *  identifier/type discrimination.
 */
built_in_type_id:                   built_in_type_specifier
    |                               built_in_type_id built_in_type_specifier                    { $$ = YACC_BUILT_IN_IDS($1, $2); }
pseudo_destructor_id:               built_in_type_id SCOPE '~' built_in_type_id                 { $$ = YACC_PSEUDO_DESTRUCTOR_ID($1, $4); }
    |                               '~' built_in_type_id                                        { $$ = YACC_PSEUDO_DESTRUCTOR_ID(0, $2); }
    |                               TEMPLATE pseudo_destructor_id                               { $$ = YACC_SET_TEMPLATE_ID($2); }
nested_pseudo_destructor_id:        pseudo_destructor_id
    |                               id_scope nested_pseudo_destructor_id                        { $$ = YACC_NESTED_ID($1, $2); }
scoped_pseudo_destructor_id:        nested_pseudo_destructor_id
    |                               global_scope scoped_pseudo_destructor_id                    { $$ = YACC_GLOBAL_ID($1, $2); }

/*---------------------------------------------------------------------------------------------------
 * A.2 Lexical conventions
 *---------------------------------------------------------------------------------------------------*/
/*
 *  String concatenation is a phase 6, not phase 7 activity so does not really belong in the grammar.
 *  However it may be convenient to have it here to make this grammar fully functional.
 *  Unfortunately it introduces a conflict with the generalised parsing of extern "C" which
 *  is correctly resolved to maximise the string length as the token source should do anyway.
 */
string:                             StringLiteral                                               { $$ = $1; }
/*string:                           StringLiteral                           %prec SHIFT_THERE   { $$ = YACC_STRINGS($1, 0); } */
/*  |                               StringLiteral string  -- Perverse order avoids conflicts -- { $$ = YACC_STRINGS($1, $2); } */
literal:                            IntegerLiteral                                              { $$ = YACC_INTEGER_LITERAL_EXPRESSION($1); }
    |                               CharacterLiteral                                            { $$ = YACC_CHARACTER_LITERAL_EXPRESSION($1); }
    |                               FloatingLiteral                                             { $$ = YACC_FLOATING_LITERAL_EXPRESSION($1); }
    |                               string                                                      { $$ = YACC_STRING_LITERAL_EXPRESSION($1); }
    |                               boolean_literal
boolean_literal:                    FALSE                                                       { $$ = YACC_FALSE_EXPRESSION(); }
    |                               TRUE                                                        { $$ = YACC_TRUE_EXPRESSION(); }

/*---------------------------------------------------------------------------------------------------
 * A.3 Basic concepts
 *---------------------------------------------------------------------------------------------------*/
translation_unit:                   declaration_seq.opt                                         { YACC_RESULT($1); }

/*---------------------------------------------------------------------------------------------------
 * A.4 Expressions
 *---------------------------------------------------------------------------------------------------
 *  primary_expression covers an arbitrary sequence of all names with the exception of an unscoped destructor,
 *  which is parsed as its unary expression which is the correct disambiguation (when ambiguous).
 *  This eliminates the traditional A(B) meaning A B ambiguity, since we never have to tack an A onto
 *  the front of something that might start with (. The name length got maximised ab initio. The downside
 *  is that semantic interpretation must split the names up again.
 *
 *  Unification of the declaration and expression syntax means that unary and binary pointer declarator operators:
 *      int * * name
 *  are parsed as binary and unary arithmetic operators (int) * (*name). Since type information is not used
 *  ambiguities resulting from a cast
 *      (cast)*(value)
 *  are resolved to favour the binary rather than the cast unary to ease AST clean-up.
 *  The cast-call ambiguity must be resolved to the cast to ensure that (a)(b)c can be parsed.
 *
 *  The problem of the functional cast ambiguity
 *      name(arg)
 *  as call or declaration is avoided by maximising the name within the parsing kernel. So
 *  primary_id_expression picks up 
 *      extern long int const var = 5;
 *  as an assignment to the syntax parsed as "extern long int const var". The presence of two names is
 *  parsed so that "extern long into const" is distinguished from "var" considerably simplifying subsequent
 *  semantic resolution.
 *
 *  The generalised name is a concatenation of potential type-names (scoped identifiers or built-in sequences)
 *  plus optionally one of the special names such as an operator-function-id, conversion-function-id or
 *  destructor as the final name. 
 */
primary_expression:                 literal
    |                               THIS                                                    { $$ = YACC_THIS_EXPRESSION(); }
    |                               suffix_decl_specified_ids                               { $$ = $1; }
/*  |                               SCOPE identifier                                        -- covered by suffix_decl_specified_ids */
/*  |                               SCOPE operator_function_id                              -- covered by suffix_decl_specified_ids */
/*  |                               SCOPE qualified_id                                      -- covered by suffix_decl_specified_ids */
    |                               abstract_expression           %prec REDUCE_HERE_MOSTLY  /* Prefer binary to unary ops, cast to call */
/*  |                               id_expression                                           -- covered by suffix_decl_specified_ids */

/*
 *  Abstract-expression covers the () and [] of abstract-declarators.
 */
abstract_expression:                parenthesis_clause                                      { $$ = YACC_ABSTRACT_FUNCTION_EXPRESSION($1); }
    |                               '[' expression.opt ']'                                  { $$ = YACC_ABSTRACT_ARRAY_EXPRESSION($2); }
    |                               TEMPLATE parenthesis_clause                             { $$ = YACC_SET_TEMPLATE_EXPRESSION(YACC_ABSTRACT_FUNCTION_EXPRESSION($2)); }

/*  Type I function parameters are ambiguous with respect to the generalised name, so we have to do a lookahead following
 *  any function-like parentheses. This unfortunately hits normal code, so kill the -- lines and add the ++ lines for efficiency.
 *  Supporting Type I code under the superset causes perhaps 25% of lookahead parsing. Sometimes complete class definitions
 *  get traversed since they are valid generalised type I parameters!
 */
type1_parameters:       /*----*/    parameter_declaration_list ';'                          { $$ = YACC_TYPE1_PARAMETERS(0, $1); }
    |                   /*----*/    type1_parameters parameter_declaration_list ';'         { $$ = YACC_TYPE1_PARAMETERS($1, $2); }
mark_type1:                         /* empty */                                             { $$ = mark_type1(); }
postfix_expression:                 primary_expression
/*  |                   /++++++/    postfix_expression parenthesis_clause                   { $$ = YACC_CALL_EXPRESSION($1, $2); } */
    |                   /*----*/    postfix_expression parenthesis_clause mark_type1 '-'    { $$ = YACC_CALL_EXPRESSION($1, $2); }
    |                   /*----*/    postfix_expression parenthesis_clause mark_type1 '+' type1_parameters mark '{' error 
                        /*----*/                    { yyerrok; remark_type1($6); unmark(); unmark($5); $$ = YACC_TYPE1_EXPRESSION($1, $2, $5); }
    |                   /*----*/    postfix_expression parenthesis_clause mark_type1 '+' type1_parameters mark error 
                        /*----*/                    { yyerrok; remark_type1($3); unmark(); unmark(); $$ = YACC_CALL_EXPRESSION($1, $2); }
    |                   /*----*/    postfix_expression parenthesis_clause mark_type1 '+' error
                        /*----*/                    { yyerrok; remark_type1($3); unmark(); $$ = YACC_CALL_EXPRESSION($1, $2); }
    |                               postfix_expression '[' expression.opt ']'               { $$ = YACC_ARRAY_EXPRESSION($1, $3); }
/*  |                               destructor_id '[' expression.opt ']'                    -- not semantically valid */
/*  |                               destructor_id parenthesis_clause                        -- omitted to resolve known ambiguity */
/*  |                               simple_type_specifier '(' expression_list.opt ')'       -- simple_type_specifier is a primary_expression */
    |                               postfix_expression '.' declarator_id                    { $$ = YACC_DOT_EXPRESSION($1, $3); }
/*  |                               postfix_expression '.' TEMPLATE declarator_id           -- TEMPLATE absorbed into declarator_id. */
    |                               postfix_expression '.' scoped_pseudo_destructor_id      { $$ = YACC_DOT_EXPRESSION($1, $3); }
    |                               postfix_expression ARROW declarator_id                  { $$ = YACC_ARROW_EXPRESSION($1, $3); }
/*  |                               postfix_expression ARROW TEMPLATE declarator_id         -- TEMPLATE absorbed into declarator_id. */
    |                               postfix_expression ARROW scoped_pseudo_destructor_id    { $$ = YACC_ARROW_EXPRESSION($1, $3); }   
    |                               postfix_expression INC                                  { $$ = YACC_POST_INCREMENT_EXPRESSION($1); }
    |                               postfix_expression DEC                                  { $$ = YACC_POST_DECREMENT_EXPRESSION($1); }
    |                               DYNAMIC_CAST '<' type_id '>' '(' expression ')'         { $$ = YACC_DYNAMIC_CAST_EXPRESSION($3, $6); }
    |                               STATIC_CAST '<' type_id '>' '(' expression ')'          { $$ = YACC_STATIC_CAST_EXPRESSION($3, $6); }
    |                               REINTERPRET_CAST '<' type_id '>' '(' expression ')'     { $$ = YACC_REINTERPRET_CAST_EXPRESSION($3, $6); }
    |                               CONST_CAST '<' type_id '>' '(' expression ')'           { $$ = YACC_CONST_CAST_EXPRESSION($3, $6); }
    |                               TYPEID parameters_clause                                { $$ = YACC_TYPEID_EXPRESSION($2); }
/*  |                               TYPEID '(' expression ')'                               -- covered by parameters_clause */
/*  |                               TYPEID '(' type_id ')'                                  -- covered by parameters_clause */
expression_list.opt:                /* empty */                                             { $$ = YACC_EXPRESSIONS(0, 0); }
    |                               expression_list
expression_list:                    assignment_expression                                   { $$ = YACC_EXPRESSIONS(0, $1); }
    |                               expression_list ',' assignment_expression               { $$ = YACC_EXPRESSIONS($1, $3); }

unary_expression:                   postfix_expression
    |                               INC cast_expression                                     { $$ = YACC_PRE_INCREMENT_EXPRESSION($2); }
    |                               DEC cast_expression                                     { $$ = YACC_PRE_DECREMENT_EXPRESSION($2); }
    |                               ptr_operator cast_expression                            { $$ = YACC_POINTER_EXPRESSION($1, $2); }
/*  |                               '*' cast_expression                                     -- covered by ptr_operator */
/*  |                               '&' cast_expression                                     -- covered by ptr_operator */
/*  |                               decl_specifier_seq '*' cast_expression                  -- covered by binary operator */
/*  |                               decl_specifier_seq '&' cast_expression                  -- covered by binary operator */
    |                               suffix_decl_specified_scope star_ptr_operator cast_expression   /* covers e.g int ::type::* const t = 4 */
                                                                                            { $$ = YACC_SCOPED_POINTER_EXPRESSION($1, $2, $3); }
    |                               '+' cast_expression                                     { $$ = YACC_PLUS_EXPRESSION($2); }
    |                               '-' cast_expression                                     { $$ = YACC_MINUS_EXPRESSION($2); }
    |                               '!' cast_expression                                     { $$ = YACC_NOT_EXPRESSION($2); }
    |                               '~' cast_expression                                     { $$ = YACC_COMPLEMENT_EXPRESSION($2); }
    |                               SIZEOF unary_expression                                 { $$ = YACC_SIZEOF_EXPRESSION($2); }
/*  |                               SIZEOF '(' type_id ')'                                  -- covered by unary_expression */
    |                               new_expression                                          { $$ = $1; }
    |                               global_scope new_expression                             { $$ = YACC_GLOBAL_EXPRESSION($1, $2); }
    |                               delete_expression                                       { $$ = $1; }
    |                               global_scope delete_expression                          { $$ = YACC_GLOBAL_EXPRESSION($1, $2); }
/*  |                               DELETE '[' ']' cast_expression       -- covered by DELETE cast_expression since cast_expression covers ... */
/*  |                               SCOPE DELETE '[' ']' cast_expression //  ... abstract_expression cast_expression and so [] cast_expression */

delete_expression:                  DELETE cast_expression                                  /* also covers DELETE[] cast_expression */
                                                                                            { $$ = YACC_DELETE_EXPRESSION($2); }
new_expression:                     NEW new_type_id new_initializer.opt                     { $$ = YACC_NEW_TYPE_ID_EXPRESSION(0, $2, $3); }
    |                               NEW parameters_clause new_type_id new_initializer.opt   { $$ = YACC_NEW_TYPE_ID_EXPRESSION($2, $3, $4); }
    |                               NEW parameters_clause                                   { $$ = YACC_NEW_EXPRESSION($2, 0, 0); }
/*  |                               NEW '(' type-id ')'                                     -- covered by parameters_clause */
    |                               NEW parameters_clause parameters_clause new_initializer.opt { $$ = YACC_NEW_EXPRESSION($2, $3, $4); }
/*  |                               NEW '(' type-id ')' new_initializer                     -- covered by parameters_clause parameters_clause */
/*  |                               NEW parameters_clause '(' type-id ')'                   -- covered by parameters_clause parameters_clause */
                                                                                /* ptr_operator_seq.opt production reused to save a %prec */
new_type_id:                        type_specifier ptr_operator_seq.opt                     { $$ = YACC_TYPED_EXPRESSION($1, $2); }
    |                               type_specifier new_declarator                           { $$ = YACC_TYPED_EXPRESSION($1, $2); }
    |                               type_specifier new_type_id                              { $$ = YACC_TYPED_EXPRESSION($1, $2); }
new_declarator:                     ptr_operator new_declarator                             { $$ = YACC_POINTER_EXPRESSION($1, $2); }
    |                               direct_new_declarator
direct_new_declarator:              '[' expression ']'                                      { $$ = YACC_ABSTRACT_ARRAY_EXPRESSION($2); }
    |                               direct_new_declarator '[' constant_expression ']'       { $$ = YACC_ARRAY_EXPRESSION($1, $3); }
new_initializer.opt:                /* empty */                                             { $$ = YACC_EXPRESSIONS(0, 0); }
    |                               '(' expression_list.opt ')'                             { $$ = $2; }

/*  cast-expression is generalised to support a [] as well as a () prefix. This covers the omission of DELETE[] which when
 *  followed by a parenthesised expression was ambiguous. It also covers the gcc indexed array initialisation for free.
 */
cast_expression:                    unary_expression
    |                               abstract_expression cast_expression                         { $$ = YACC_CAST_EXPRESSION($1, $2); }
/*  |                               '(' type_id ')' cast_expression                             -- covered by abstract_expression */

pm_expression:                      cast_expression
    |                               pm_expression DOT_STAR cast_expression                      { $$ = YACC_DOT_STAR_EXPRESSION($1, $3); }
    |                               pm_expression ARROW_STAR cast_expression                    { $$ = YACC_ARROW_STAR_EXPRESSION($1, $3); }
multiplicative_expression:          pm_expression
    |                               multiplicative_expression star_ptr_operator pm_expression   { $$ = YACC_MULTIPLY_EXPRESSION($1, $2, $3); }
    |                               multiplicative_expression '/' pm_expression                 { $$ = YACC_DIVIDE_EXPRESSION($1, $3); }
    |                               multiplicative_expression '%' pm_expression                 { $$ = YACC_MODULUS_EXPRESSION($1, $3); }
additive_expression:                multiplicative_expression
    |                               additive_expression '+' multiplicative_expression           { $$ = YACC_ADD_EXPRESSION($1, $3); }
    |                               additive_expression '-' multiplicative_expression           { $$ = YACC_SUBTRACT_EXPRESSION($1, $3); }
shift_expression:                   additive_expression
    |                               shift_expression SHL additive_expression                    { $$ = YACC_SHIFT_LEFT_EXPRESSION($1, $3); }
    |                               shift_expression SHR additive_expression                    { $$ = YACC_SHIFT_RIGHT_EXPRESSION($1, $3); }
relational_expression:              shift_expression
    |                               relational_expression '<' shift_expression                  { $$ = YACC_LESS_THAN_EXPRESSION($1, $3); }
    |                               relational_expression '>' shift_expression                  { $$ = YACC_GREATER_THAN_EXPRESSION($1, $3); }
    |                               relational_expression LE shift_expression                   { $$ = YACC_LESS_EQUAL_EXPRESSION($1, $3); }
    |                               relational_expression GE shift_expression                   { $$ = YACC_GREATER_EQUAL_EXPRESSION($1, $3); }
equality_expression:                relational_expression
    |                               equality_expression EQ relational_expression                { $$ = YACC_EQUAL_EXPRESSION($1, $3); }
    |                               equality_expression NE relational_expression                { $$ = YACC_NOT_EQUAL_EXPRESSION($1, $3); }
and_expression:                     equality_expression
    |                               and_expression '&' equality_expression                      { $$ = YACC_AND_EXPRESSION($1, $3); }
exclusive_or_expression:            and_expression
    |                               exclusive_or_expression '^' and_expression                  { $$ = YACC_EXCLUSIVE_OR_EXPRESSION($1, $3); }
inclusive_or_expression:            exclusive_or_expression
    |                               inclusive_or_expression '|' exclusive_or_expression         { $$ = YACC_INCLUSIVE_OR_EXPRESSION($1, $3); }
logical_and_expression:             inclusive_or_expression
    |                               logical_and_expression LOG_AND inclusive_or_expression      { $$ = YACC_LOGICAL_AND_EXPRESSION($1, $3); }
logical_or_expression:              logical_and_expression
    |                               logical_or_expression LOG_OR logical_and_expression         { $$ = YACC_LOGICAL_OR_EXPRESSION($1, $3); }
conditional_expression:             logical_or_expression
    |                               logical_or_expression '?' expression ':' assignment_expression
                                                                                                { $$ = YACC_CONDITIONAL_EXPRESSION($1, $3, $5); }

/*  assignment-expression is generalised to cover the simple assignment of a braced initializer in order to contribute to the
 *  coverage of parameter-declaration and init-declaration.
 */
assignment_expression:              conditional_expression
    |                               logical_or_expression assignment_operator assignment_expression { $$ = YACC_ASSIGNMENT_EXPRESSION($1, $2, $3); }
    |                               logical_or_expression '=' braced_initializer                    { $$ = YACC_ASSIGNMENT_EXPRESSION($1, $2, $3); }
    |                               throw_expression
assignment_operator:                '=' | ASS_ADD | ASS_AND | ASS_DIV | ASS_MOD | ASS_MUL | ASS_OR | ASS_SHL | ASS_SHR | ASS_SUB | ASS_XOR

/*  expression is widely used and usually single-element, so the reductions are arranged so that a
 *  single-element expression is returned as is. Multi-element expressions are parsed as a list that
 *  may then behave polymorphically as an element or be compacted to an element. */ 
expression.opt:                     /* empty */                                                 { $$ = YACC_EXPRESSION(0); }
    |                               expression
expression:                         assignment_expression
    |                               expression_list ',' assignment_expression                   { $$ = YACC_EXPRESSION(YACC_EXPRESSIONS($1, $3)); }
constant_expression:                conditional_expression

/*  The grammar is repeated for when the parser stack knows that the next > must end a template.
 */
templated_relational_expression:    shift_expression
    |                               templated_relational_expression '<' shift_expression        { $$ = YACC_LESS_THAN_EXPRESSION($1, $3); }
    |                               templated_relational_expression LE shift_expression         { $$ = YACC_LESS_EQUAL_EXPRESSION($1, $3); }
    |                               templated_relational_expression GE shift_expression         { $$ = YACC_GREATER_EQUAL_EXPRESSION($1, $3); }
templated_equality_expression:      templated_relational_expression
    |                               templated_equality_expression EQ templated_relational_expression    { $$ = YACC_EQUAL_EXPRESSION($1, $3); }
    |                               templated_equality_expression NE templated_relational_expression    { $$ = YACC_NOT_EQUAL_EXPRESSION($1, $3); }
templated_and_expression:           templated_equality_expression
    |                               templated_and_expression '&' templated_equality_expression  { $$ = YACC_AND_EXPRESSION($1, $3); }
templated_exclusive_or_expression:  templated_and_expression
    |                               templated_exclusive_or_expression '^' templated_and_expression
                                                                                                { $$ = YACC_EXCLUSIVE_OR_EXPRESSION($1, $3); }
templated_inclusive_or_expression:  templated_exclusive_or_expression
    |                               templated_inclusive_or_expression '|' templated_exclusive_or_expression
                                                                                                { $$ = YACC_INCLUSIVE_OR_EXPRESSION($1, $3); }
templated_logical_and_expression:   templated_inclusive_or_expression
    |                               templated_logical_and_expression LOG_AND templated_inclusive_or_expression
                                                                                                { $$ = YACC_LOGICAL_AND_EXPRESSION($1, $3); }
templated_logical_or_expression:    templated_logical_and_expression
    |                               templated_logical_or_expression LOG_OR templated_logical_and_expression
                                                                                                { $$ = YACC_LOGICAL_OR_EXPRESSION($1, $3); }
templated_conditional_expression:   templated_logical_or_expression
    |                               templated_logical_or_expression '?' templated_expression ':' templated_assignment_expression
                                                                                                { $$ = YACC_CONDITIONAL_EXPRESSION($1, $3, $5); }
templated_assignment_expression:    templated_conditional_expression
    |                               templated_logical_or_expression assignment_operator templated_assignment_expression
                                                                                                { $$ = YACC_ASSIGNMENT_EXPRESSION($1, $2, $3); }
    |                               templated_throw_expression
templated_expression:               templated_assignment_expression
    |                               templated_expression_list ',' templated_assignment_expression
                                                                                                { $$ = YACC_EXPRESSION(YACC_EXPRESSIONS($1, $3)); }
templated_expression_list:          templated_assignment_expression                             { $$ = YACC_EXPRESSIONS(0, $1); }
    |                               templated_expression_list ',' templated_assignment_expression    { $$ = YACC_EXPRESSIONS($1, $3); }

/*---------------------------------------------------------------------------------------------------
 * A.5 Statements
 *---------------------------------------------------------------------------------------------------
 *  Parsing statements is easy once simple_declaration has been generalised to cover expression_statement.
 */
looping_statement:                  start_search looped_statement                               { $$ = YACC_LINED_STATEMENT($2, $1); end_search($$); }
looped_statement:                   statement
    |                               advance_search '+' looped_statement                         { $$ = $3; }
    |                               advance_search '-'                                          { $$ = 0; }
statement:                          control_statement
/*  |                               expression_statement                                        -- covered by declaration_statement */
    |                               compound_statement
    |                               declaration_statement
    |                               try_block                                                   { $$ = YACC_TRY_BLOCK_STATEMENT($1); }
control_statement:                  labeled_statement
    |                               selection_statement
    |                               iteration_statement
    |                               jump_statement
labeled_statement:                  identifier_word ':' looping_statement                       { $$ = YACC_LABEL_STATEMENT($1, $3); }
    |                               CASE constant_expression ':' looping_statement              { $$ = YACC_CASE_STATEMENT($2, $4); }
    |                               DEFAULT ':' looping_statement                               { $$ = YACC_DEFAULT_STATEMENT($3); }
/*expression_statement:             expression.opt ';'                                          -- covered by declaration_statement */
compound_statement:                 '{' statement_seq.opt '}'                                   { $$ = YACC_COMPOUND_STATEMENT($2); }
    |                               '{' statement_seq.opt looping_statement '#' bang error '}'  { $$ = $2; YACC_UNBANG($5, "Bad statement-seq."); }
statement_seq.opt:                  /* empty */                                                 { $$ = YACC_STATEMENTS(0, 0); }
    |                               statement_seq.opt looping_statement                         { $$ = YACC_STATEMENTS($1, YACC_COMPILE_STATEMENT($2)); }
    |                               statement_seq.opt looping_statement '#' bang error ';'      { $$ = $1; YACC_UNBANG($4, "Bad statement."); }
/*
 *  The dangling else conflict is resolved to the innermost if.
 */
selection_statement:                IF '(' condition ')' looping_statement    %prec SHIFT_THERE { $$ = YACC_IF_STATEMENT($3, $5, 0); }
    |                               IF '(' condition ')' looping_statement ELSE looping_statement { $$ = YACC_IF_STATEMENT($3, $5, $7); }
    |                               SWITCH '(' condition ')' looping_statement                  { $$ = YACC_SWITCH_STATEMENT($3, $5); }
condition.opt:                      /* empty */                                                 { $$ = YACC_CONDITION(0); }
    |                               condition
condition:                          parameter_declaration_list                                  { $$ = YACC_CONDITION($1); }
/*  |                               expression                                                  -- covered by parameter_declaration_list */
/*  |                               type_specifier_seq declarator '=' assignment_expression     -- covered by parameter_declaration_list */
iteration_statement:                WHILE '(' condition ')' looping_statement                   { $$ = YACC_WHILE_STATEMENT($3, $5); }
    |                               DO looping_statement WHILE '(' expression ')' ';'           { $$ = YACC_DO_WHILE_STATEMENT($2, $5); }
    |                               FOR '(' for_init_statement condition.opt ';' expression.opt ')' looping_statement
                                                                                                { $$ = YACC_FOR_STATEMENT($3, $4, $6, $8); }
for_init_statement:                 simple_declaration
/*  |                               expression_statement                                        -- covered by simple_declaration */
jump_statement:                     BREAK ';'                                                   { $$ = YACC_BREAK_STATEMENT(); }
    |                               CONTINUE ';'                                                { $$ = YACC_CONTINUE_STATEMENT(); }
    |                               RETURN expression.opt ';'                                   { $$ = YACC_RETURN_STATEMENT($2); }
    |                               GOTO identifier ';'                                         { $$ = YACC_GOTO_STATEMENT($2); }
declaration_statement:              block_declaration                                           { $$ = YACC_DECLARATION_STATEMENT($1); }

/*---------------------------------------------------------------------------------------------------
 * A.6 Declarations
 *---------------------------------------------------------------------------------------------------*/
compound_declaration:               '{' nest declaration_seq.opt '}'                            { $$ = $3; unnest($2); }
    |                               '{' nest declaration_seq.opt util looping_declaration '#' bang error '}'
                                                                                                { $$ = $3; unnest($2); YACC_UNBANG($7, "Bad declaration-seq."); }
declaration_seq.opt:                /* empty */                                                 { $$ = YACC_DECLARATIONS(0, 0); }
    |                               declaration_seq.opt util looping_declaration                { $$ = YACC_DECLARATIONS($1, YACC_COMPILE_DECLARATION($2, $3)); }
    |                               declaration_seq.opt util looping_declaration '#' bang error ';' { $$ = $1; YACC_UNBANG($5, "Bad declaration."); }
looping_declaration:                start_search1 looped_declaration                            { $$ = YACC_LINED_DECLARATION($2, $1); end_search($$); }
looped_declaration:                 declaration
    |                               advance_search '+' looped_declaration                       { $$ = $3; }
    |                               advance_search '-'                                          { $$ = 0; }
declaration:                        block_declaration
    |                               function_definition                                         { $$ = YACC_SIMPLE_DECLARATION($1); }
    |                               template_declaration
/*  |                               explicit_instantiation                                      -- covered by relevant declarations */
    |                               explicit_specialization
    |                               specialised_declaration
specialised_declaration:            linkage_specification                                       { $$ = YACC_LINKAGE_SPECIFICATION($1); }
    |                               namespace_definition                                        { $$ = YACC_NAMESPACE_DECLARATION($1); }
    |                               TEMPLATE specialised_declaration                            { $$ = YACC_SET_TEMPLATE_DECLARATION($2); }
block_declaration:                  simple_declaration                                          { $$ = YACC_SIMPLE_DECLARATION($1); }
    |                               specialised_block_declaration
specialised_block_declaration:      asm_definition
    |                               namespace_alias_definition
    |                               using_declaration
    |                               using_directive
    |                               TEMPLATE specialised_block_declaration                      { $$ = YACC_SET_TEMPLATE_DECLARATION($2); }
simple_declaration:                 ';'                                                         { $$ = YACC_EXPRESSION(0); }
    |                               init_declaration ';'
    |                               init_declarations ';'                                       { $$ = $1; }
    |                               decl_specifier_prefix simple_declaration                    { $$ = YACC_DECL_SPECIFIER_EXPRESSION($2, $1); }

/*  A decl-specifier following a ptr_operator provokes a shift-reduce conflict for
 *      * const name
 *  which is resolved in favour of the pointer, and implemented by providing versions
 *  of decl-specifier guaranteed not to start with a cv_qualifier.
 *
 *  decl-specifiers are implemented type-centrically. That is the semantic constraint
 *  that there must be a type is exploited to impose structure, but actually eliminate
 *  very little syntax. built-in types are multi-name and so need a different policy.
 *
 *  non-type decl-specifiers are bound to the left-most type in a decl-specifier-seq,
 *  by parsing from the right and attaching suffixes to the right-hand type. Finally
 *  residual prefixes attach to the left.                
 */
suffix_built_in_decl_specifier.raw: built_in_type_specifier                                     { $$ = $1; }
    |                               suffix_built_in_decl_specifier.raw built_in_type_specifier  { $$ = YACC_BUILT_IN_NAME($1, $2); }
    |                               suffix_built_in_decl_specifier.raw decl_specifier_suffix    { $$ = YACC_DECL_SPECIFIER_NAME($1, $2); }
suffix_built_in_decl_specifier:     suffix_built_in_decl_specifier.raw                          { $$ = $1; }
    |                               TEMPLATE suffix_built_in_decl_specifier                     { $$ = YACC_SET_TEMPLATE_NAME($2); }
suffix_named_decl_specifier:        scoped_id                                                   { $$ = $1; }
    |                               elaborate_type_specifier                                    { $$ = $1; }
    |                               suffix_named_decl_specifier decl_specifier_suffix           { $$ = YACC_DECL_SPECIFIER_NAME($1, $2); }
suffix_named_decl_specifier.bi:     suffix_named_decl_specifier                                 { $$ = YACC_NAME_EXPRESSION($1); }
    |                               suffix_named_decl_specifier suffix_built_in_decl_specifier.raw  { $$ = YACC_TYPED_NAME($1, $2); }
suffix_named_decl_specifiers:       suffix_named_decl_specifier.bi
    |                               suffix_named_decl_specifiers suffix_named_decl_specifier.bi { $$ = YACC_TYPED_NAME($1, $2); }
suffix_named_decl_specifiers.sf:    scoped_special_function_id          /* operators etc */     { $$ = YACC_NAME_EXPRESSION($1); }
    |                               suffix_named_decl_specifiers
    |                               suffix_named_decl_specifiers scoped_special_function_id     { $$ = YACC_TYPED_NAME($1, $2); }
suffix_decl_specified_ids:          suffix_built_in_decl_specifier
    |                               suffix_built_in_decl_specifier suffix_named_decl_specifiers.sf { $$ = YACC_TYPED_NAME($1, $2); }
    |                               suffix_named_decl_specifiers.sf
suffix_decl_specified_scope:        suffix_named_decl_specifiers SCOPE
    |                               suffix_built_in_decl_specifier suffix_named_decl_specifiers SCOPE { $$ = YACC_TYPED_NAME($1, $2); }
    |                               suffix_built_in_decl_specifier SCOPE                        { $$ = YACC_NAME_EXPRESSION($1); }

decl_specifier_affix:               storage_class_specifier
    |                               function_specifier
    |                               FRIEND                                                          
    |                               TYPEDEF
    |                               cv_qualifier                                                { $$ = $1; }

decl_specifier_suffix:              decl_specifier_affix

decl_specifier_prefix:              decl_specifier_affix
    |                               TEMPLATE decl_specifier_prefix                              { $$ = YACC_SET_TEMPLATE_DECL_SPECIFIER($2); }

storage_class_specifier:            REGISTER | STATIC | MUTABLE
    |                               EXTERN                  %prec SHIFT_THERE                   /* Prefer linkage specification */
    |                               AUTO

function_specifier:                 EXPLICIT
    |                               INLINE
    |                               VIRTUAL

type_specifier:                     simple_type_specifier
    |                               elaborate_type_specifier
    |                               cv_qualifier                                                { $$ = YACC_CV_DECL_SPECIFIER($1); }

elaborate_type_specifier:           class_specifier
    |                               enum_specifier
    |                               elaborated_type_specifier
    |                               TEMPLATE elaborate_type_specifier                           { $$ = YACC_SET_TEMPLATE_ID($2); }
simple_type_specifier:              scoped_id
    |                               built_in_type_specifier                                     { $$ = YACC_BUILT_IN_ID_ID($1); }
built_in_type_specifier:            CHAR | WCHAR_T | BOOL | SHORT | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID

/*
 *  The over-general use of declaration_expression to cover decl-specifier-seq.opt declarator in a function-definition means that
 *      class X {};
 *  could be a function-definition or a class-specifier.
 *      enum X {};
 *  could be a function-definition or an enum-specifier.
 *  The function-definition is not syntactically valid so resolving the false conflict in favour of the
 *  elaborated_type_specifier is correct.
 */
elaborated_type_specifier:          elaborated_class_specifier
    |                               elaborated_enum_specifier
    |                               TYPENAME scoped_id                                          { $$ = YACC_ELABORATED_TYPE_SPECIFIER($1, $2); }

elaborated_enum_specifier:          ENUM scoped_id               %prec SHIFT_THERE              { $$ = YACC_ELABORATED_TYPE_SPECIFIER($1, $2); }
enum_specifier:                     ENUM scoped_id enumerator_clause                            { $$ = YACC_ENUM_SPECIFIER_ID($2, $3); }
    |                               ENUM enumerator_clause                                      { $$ = YACC_ENUM_SPECIFIER_ID(0, $2); }
enumerator_clause:                  '{' enumerator_list_ecarb                                   { $$ = YACC_ENUMERATORS(0, 0); }
    |                               '{' enumerator_list enumerator_list_ecarb                   { $$ = $2; }
    |                               '{' enumerator_list ',' enumerator_definition_ecarb         { $$ = $2; }
enumerator_list_ecarb:              '}'                                                         { }
    |                               bang error '}'                                              { YACC_UNBANG($1, "Bad enumerator-list."); }
enumerator_definition_ecarb:        '}'                                                         { }
    |                               bang error '}'                                              { YACC_UNBANG($1, "Bad enumerator-definition."); }
enumerator_definition_filler:       /* empty */
    |                               bang error ','                                              { YACC_UNBANG($1, "Bad enumerator-definition."); }
enumerator_list_head:               enumerator_definition_filler                                { $$ = YACC_ENUMERATORS(0, 0); }
    |                               enumerator_list ',' enumerator_definition_filler
enumerator_list:                    enumerator_list_head enumerator_definition                  { $$ = YACC_ENUMERATORS($1, $2); }
enumerator_definition:              enumerator                                                  { $$ = YACC_ENUMERATOR($1, 0); }
    |                               enumerator '=' constant_expression                          { $$ = YACC_ENUMERATOR($1, $3); }
enumerator:                         identifier

namespace_definition:               NAMESPACE scoped_id compound_declaration                    { $$ = YACC_NAMESPACE_DEFINITION($2, $3); }
    |                               NAMESPACE compound_declaration                              { $$ = YACC_NAMESPACE_DEFINITION(0, $2); }
namespace_alias_definition:         NAMESPACE scoped_id '=' scoped_id ';'                       { $$ = YACC_NAMESPACE_ALIAS_DEFINITION($2, $4); }

using_declaration:                  USING declarator_id ';'                                     { $$ = YACC_USING_DECLARATION(false, $2); }
    |                               USING TYPENAME declarator_id ';'                            { $$ = YACC_USING_DECLARATION(true, $3); }

using_directive:                    USING NAMESPACE scoped_id ';'                               { $$ = YACC_USING_DIRECTIVE($3); }
asm_definition:                     ASM '(' string ')' ';'                                      { $$ = YACC_ASM_DEFINITION($3); }
linkage_specification:              EXTERN string looping_declaration                           { $$ = YACC_LINKAGE_SPECIFIER($2, $3); }
    |                               EXTERN string compound_declaration                          { $$ = YACC_LINKAGE_SPECIFIER($2, $3); }

/*---------------------------------------------------------------------------------------------------
 * A.7 Declarators
 *---------------------------------------------------------------------------------------------------*/
/*init-declarator is named init_declaration to reflect the embedded decl-specifier-seq.opt*/
init_declarations:                  assignment_expression ',' init_declaration                  { $$ = YACC_EXPRESSIONS(YACC_EXPRESSIONS(0, $1), $3); }
    |                               init_declarations ',' init_declaration                      { $$ = YACC_EXPRESSIONS($1, $3); }
init_declaration:                   assignment_expression
/*  |                               assignment_expression '=' initializer_clause                -- covered by assignment_expression */
/*  |                               assignment_expression '(' expression_list ')'               -- covered by another set of call arguments */

/*declarator:                                                                                   -- covered by assignment_expression */
/*direct_declarator:                                                                            -- covered by postfix_expression */

star_ptr_operator:                  '*'                                                         { $$ = YACC_POINTER_DECLARATOR(); }
    |                               star_ptr_operator cv_qualifier                              { $$ = YACC_CV_DECLARATOR($1, $2); }
nested_ptr_operator:                star_ptr_operator                                           { $$ = $1; }
    |                               id_scope nested_ptr_operator                                { $$ = YACC_NESTED_DECLARATOR($1, $2); }
ptr_operator:                       '&'                                                         { $$ = YACC_REFERENCE_DECLARATOR(); }
    |                               nested_ptr_operator                                         { $$ = $1; }
    |                               global_scope nested_ptr_operator                            { $$ = YACC_GLOBAL_DECLARATOR($1, $2); }
ptr_operator_seq:                   ptr_operator                                                { $$ = YACC_POINTER_EXPRESSION($1, YACC_EPSILON()); }
    |                               ptr_operator ptr_operator_seq                               { $$ = YACC_POINTER_EXPRESSION($1, $2); }
/* Independently coded to localise the shift-reduce conflict: sharing just needs another %prec */
ptr_operator_seq.opt:               /* empty */                         %prec SHIFT_THERE       /* Maximise type length */ { $$ = YACC_EXPRESSION(0); }
    |                               ptr_operator ptr_operator_seq.opt                           { $$ = YACC_POINTER_EXPRESSION($1, $2); }

cv_qualifier_seq.opt:               /* empty */                                                 { $$ = YACC_CV_QUALIFIERS(0, 0); }
    |                               cv_qualifier_seq.opt cv_qualifier                           { $$ = YACC_CV_QUALIFIERS($1, $2); }
cv_qualifier:                       CONST | VOLATILE /* | CvQualifier */

/*type_id                                                                                       -- also covered by parameter declaration */
type_id:                            type_specifier abstract_declarator.opt                      { $$ = YACC_TYPED_EXPRESSION($1, $2); }
    |                               type_specifier type_id                                      { $$ = YACC_TYPED_EXPRESSION($1, $2); }

/*abstract_declarator:                                                                          -- also covered by parameter declaration */
abstract_declarator.opt:            /* empty */                                                 { $$ = YACC_EPSILON(); }
    |                               ptr_operator abstract_declarator.opt                        { $$ = YACC_POINTER_EXPRESSION($1, $2); }
    |                               direct_abstract_declarator
direct_abstract_declarator.opt:     /* empty */                                                 { $$ = YACC_EPSILON(); }
    |                               direct_abstract_declarator
direct_abstract_declarator:         direct_abstract_declarator.opt parenthesis_clause           { $$ = YACC_CALL_EXPRESSION($1, $2); }
    |                               direct_abstract_declarator.opt '[' ']'                      { $$ = YACC_ARRAY_EXPRESSION($1, 0); }
    |                               direct_abstract_declarator.opt '[' constant_expression ']'  { $$ = YACC_ARRAY_EXPRESSION($1, $3); }
/*  |                               '(' abstract_declarator ')'                                 -- covered by parenthesis_clause */

parenthesis_clause:                 parameters_clause cv_qualifier_seq.opt                      { $$ = YACC_PARENTHESISED($1, $2, 0); }
    |                               parameters_clause cv_qualifier_seq.opt exception_specification  { $$ = YACC_PARENTHESISED($1, $2, $3); }
parameters_clause:                  '(' parameter_declaration_clause ')'                        { $$ = $2; }
/* parameter_declaration_clause also covers init_declaration, type_id, declarator and abstract_declarator. */
parameter_declaration_clause:       /* empty */                                                 { $$ = YACC_PARAMETERS(0, 0); }
    |                               parameter_declaration_list
    |                               parameter_declaration_list ELLIPSIS                         { $$ = YACC_PARAMETERS($1, YACC_ELLIPSIS_EXPRESSION()); }
parameter_declaration_list:         parameter_declaration                                       { $$ = YACC_PARAMETERS(0, $1); }
    |                               parameter_declaration_list ',' parameter_declaration        { $$ = YACC_PARAMETERS($1, $3); }

/* A typed abstract qualifier such as
 *      Class * ...
 * looks like a multiply, so pointers are parsed as their binary operation equivalents that
 * ultimately terminate with a degenerate right hand term.
 */
abstract_pointer_declaration:       ptr_operator_seq
    |                               multiplicative_expression star_ptr_operator ptr_operator_seq.opt { $$ = YACC_MULTIPLY_EXPRESSION($1, $2, $3); }
abstract_parameter_declaration:     abstract_pointer_declaration
    |                               and_expression '&'                                          { $$ = YACC_AND_EXPRESSION($1, YACC_EPSILON()); }
    |                               and_expression '&' abstract_pointer_declaration             { $$ = YACC_AND_EXPRESSION($1, $3); }
special_parameter_declaration:      abstract_parameter_declaration
    |                               abstract_parameter_declaration '=' assignment_expression    { $$ = YACC_ASSIGNMENT_EXPRESSION($1, $2, $3); }
    |                               ELLIPSIS                                                    { $$ = YACC_ELLIPSIS_EXPRESSION(); }
parameter_declaration:              assignment_expression                                       { $$ = YACC_EXPRESSION_PARAMETER($1); }
    |                               special_parameter_declaration                               { $$ = YACC_EXPRESSION_PARAMETER($1); }
    |                               decl_specifier_prefix parameter_declaration                 { $$ = YACC_DECL_SPECIFIER_PARAMETER($2, $1); }

/*  The grammar is repeated for use within template <>
 */
templated_parameter_declaration:    templated_assignment_expression                             { $$ = YACC_EXPRESSION_PARAMETER($1); }
    |                               templated_abstract_declaration                              { $$ = YACC_EXPRESSION_PARAMETER($1); }
    |                               templated_abstract_declaration '=' templated_assignment_expression
                                                    { $$ = YACC_EXPRESSION_PARAMETER(YACC_ASSIGNMENT_EXPRESSION($1, $2, $3)); }
    |                               decl_specifier_prefix templated_parameter_declaration       { $$ = YACC_DECL_SPECIFIER_PARAMETER($2, $1); }
templated_abstract_declaration:     abstract_pointer_declaration
    |                               templated_and_expression '&'                                { $$ = YACC_AND_EXPRESSION($1, 0); }
    |                               templated_and_expression '&' abstract_pointer_declaration   { $$ = YACC_AND_EXPRESSION($1, $3); }

/*  function_definition includes constructor, destructor, implicit int definitions too.
 *  A local destructor is successfully parsed as a function-declaration but the ~ was treated as a unary operator.
 *  constructor_head is the prefix ambiguity between a constructor and a member-init-list starting with a bit-field.
 */
function_definition:        ctor_definition
    |                       func_definition
func_definition:            assignment_expression function_try_block                    { $$ = YACC_FUNCTION_DEFINITION($1, $2); }
    |                       assignment_expression function_body                         { $$ = YACC_FUNCTION_DEFINITION($1, $2); }
    |                       decl_specifier_prefix func_definition                       { $$ = YACC_DECL_SPECIFIER_EXPRESSION($2, $1); }
ctor_definition:            constructor_head function_try_block                         { $$ = YACC_CTOR_DEFINITION($1, $2); }
    |                       constructor_head function_body                              { $$ = YACC_CTOR_DEFINITION($1, $2); }
    |                       decl_specifier_prefix ctor_definition                       { $$ = YACC_DECL_SPECIFIER_EXPRESSION($2, $1); }
constructor_head:           bit_field_init_declaration                                  { $$ = YACC_EXPRESSIONS(0, $1); }
    |                       constructor_head ',' assignment_expression                  { $$ = YACC_EXPRESSIONS($1, $3); }
function_try_block:         TRY function_block handler_seq                              { $$ = YACC_TRY_FUNCTION_BLOCK($2, $3); }
function_block:             ctor_initializer.opt function_body                          { $$ = YACC_CTOR_FUNCTION_BLOCK($2, $1); }
function_body:              compound_statement                                          { $$ = YACC_FUNCTION_BLOCK($1); }

/*  An = initializer looks like an extended assignment_expression.
 *  An () initializer looks like a function call.
 *  initializer is therefore flattened into its generalised customers.
 *initializer:              '=' initializer_clause                                      -- flattened into caller
 *  |                       '(' expression_list ')'                                     -- flattened into caller */
initializer_clause:         assignment_expression                                       { $$ = YACC_INITIALIZER_EXPRESSION_CLAUSE($1); }
    |                       braced_initializer
braced_initializer:         '{' initializer_list '}'                                    { $$ = YACC_INITIALIZER_LIST_CLAUSE($2); }
    |                       '{' initializer_list ',' '}'                                { $$ = YACC_INITIALIZER_LIST_CLAUSE($2); }
    |                       '{' '}'                                                     { $$ = YACC_INITIALIZER_LIST_CLAUSE(0); }
    |                       '{' looping_initializer_clause '#' bang error '}'           { $$ = 0; YACC_UNBANG($4, "Bad initializer_clause."); }
    |                       '{' initializer_list ',' looping_initializer_clause '#' bang error '}'
                                                                                        { $$ = $2; YACC_UNBANG($6, "Bad initializer_clause."); }
initializer_list:           looping_initializer_clause                                  { $$ = YACC_INITIALIZER_CLAUSES(0, $1); }
    |                       initializer_list ',' looping_initializer_clause             { $$ = YACC_INITIALIZER_CLAUSES($1, $3); }
looping_initializer_clause: start_search looped_initializer_clause                      { $$ = $2; end_search($$); }
looped_initializer_clause:  initializer_clause
    |                       advance_search '+' looped_initializer_clause                { $$ = $3; }
    |                       advance_search '-'                                          { $$ = 0; }

/*---------------------------------------------------------------------------------------------------
 * A.8 Classes
 *---------------------------------------------------------------------------------------------------
 *
 *  An anonymous bit-field declaration may look very like inheritance:
 *      class A : B = 3;
 *      class A : B ;
 *  The two usages are too distant to try to create and enforce a common prefix so we have to resort to
 *  a parser hack by backtracking. Inheritance is much the most likely so we mark the input stream context
 *  and try to parse a base-clause. If we successfully reach a { the base-clause is ok and inheritance was
 *  the correct choice so we unmark and continue. If we fail to find the { an error token causes back-tracking
 *  to the alternative parse in elaborated_class_specifier which regenerates the : and declares unconditional success.
 */
colon_mark:                 ':'                                                         { $$ = mark(); }
elaborated_class_specifier: class_key scoped_id                    %prec SHIFT_THERE    { $$ = YACC_ELABORATED_TYPE_SPECIFIER($1, $2); }
    |                       class_key scoped_id colon_mark error                        { $$ = YACC_ELABORATED_TYPE_SPECIFIER($1, $2); rewind_colon($3, $$); }
class_specifier_head:       class_key scoped_id colon_mark base_specifier_list '{'      { unmark($4); $$ = YACC_CLASS_SPECIFIER_ID($1, $2, $4); }
    |                       class_key ':' base_specifier_list '{'                       { $$ = YACC_CLASS_SPECIFIER_ID($1, 0, $3); }
    |                       class_key scoped_id '{'                                     { $$ = YACC_CLASS_SPECIFIER_ID($1, $2, 0); }
    |                       class_key '{'                                               { $$ = YACC_CLASS_SPECIFIER_ID($1, 0, 0); }
class_key:                  CLASS | STRUCT | UNION
class_specifier:            class_specifier_head member_specification.opt '}'           { $$ = YACC_CLASS_MEMBERS($1, $2); }
    |                       class_specifier_head member_specification.opt util looping_member_declaration '#' bang error '}'
                                            { $$ = YACC_CLASS_MEMBERS($1, $2); YACC_UNBANG($6, "Bad member_specification.opt."); }
member_specification.opt:   /* empty */                                                 { $$ = YACC_MEMBER_DECLARATIONS(0, 0); }
    |                       member_specification.opt util looping_member_declaration    { $$ = YACC_MEMBER_DECLARATIONS($1, YACC_COMPILE_DECLARATION($2, $3)); }
    |                       member_specification.opt util looping_member_declaration '#' bang error ';'
                                                                                                { $$ = $1; YACC_UNBANG($5, "Bad member-declaration."); }
looping_member_declaration: start_search looped_member_declaration                      { $$ = YACC_LINED_DECLARATION($2, $1); end_search($$); }
looped_member_declaration:  member_declaration
    |                       advance_search '+' looped_member_declaration                { $$ = $3; }
    |                       advance_search '-'                                          { $$ = 0; }
member_declaration:         accessibility_specifier
    |                       simple_member_declaration                                   { $$ = YACC_SIMPLE_DECLARATION($1); }
    |                       function_definition                                         { $$ = YACC_SIMPLE_DECLARATION($1); }
/*  |                       function_definition ';'                                     -- trailing ; covered by null declaration */
/*  |                       qualified_id ';'                                            -- covered by simple_member_declaration */
    |                       using_declaration
    |                       template_declaration

/*  The generality of constructor names (there need be no parenthesised argument list) means that that
 *          name : f(g), h(i)
 *  could be the start of a constructor or the start of an anonymous bit-field. An ambiguity is avoided by
 *  parsing the ctor-initializer of a function_definition as a bit-field.
 */
simple_member_declaration:  ';'                                                         { $$ = YACC_EXPRESSION(0); }
    |                       assignment_expression ';'
    |                       constructor_head ';'                                        { $$ = $1; }
    |                       member_init_declarations ';'                                { $$ = $1; }
    |                       decl_specifier_prefix simple_member_declaration             { $$ = YACC_DECL_SPECIFIER_EXPRESSION($2, $1); }
member_init_declarations:   assignment_expression ',' member_init_declaration           { $$ = YACC_EXPRESSIONS(YACC_EXPRESSIONS(0, $1), $3); }
    |                       constructor_head ',' bit_field_init_declaration             { $$ = YACC_EXPRESSIONS($1, $3); }
    |                       member_init_declarations ',' member_init_declaration        { $$ = YACC_EXPRESSIONS($1, $3); }
member_init_declaration:    assignment_expression
/*  |                       assignment_expression '=' initializer_clause                -- covered by assignment_expression */
/*  |                       assignment_expression '(' expression_list ')'               -- covered by another set of call arguments */
    |                       bit_field_init_declaration
accessibility_specifier:    access_specifier ':'                                        { $$ = YACC_ACCESSIBILITY_SPECIFIER($1); }
bit_field_declaration:      assignment_expression ':' bit_field_width                   { $$ = YACC_BIT_FIELD_EXPRESSION($1, $3); }
    |                       ':' bit_field_width                                         { $$ = YACC_BIT_FIELD_EXPRESSION(0, $2); }
bit_field_width:            logical_or_expression
/*  |                       logical_or_expression '?' expression ':' assignment_expression  -- has SR conflict w.r.t later = */
    |                       logical_or_expression '?' bit_field_width ':' bit_field_width { $$ = YACC_CONDITIONAL_EXPRESSION($1, $3, $5); }
bit_field_init_declaration: bit_field_declaration
    |                       bit_field_declaration '=' initializer_clause                { $$ = YACC_ASSIGNMENT_EXPRESSION($1, $2, $3); }

/*---------------------------------------------------------------------------------------------------
 * A.9 Derived classes
 *---------------------------------------------------------------------------------------------------*/
/*base_clause:              ':' base_specifier_list                                     -- flattened */
base_specifier_list:        base_specifier                                              { $$ = YACC_BASE_SPECIFIERS(0, $1); }
    |                       base_specifier_list ',' base_specifier                      { $$ = YACC_BASE_SPECIFIERS($1, $3); }
base_specifier:             scoped_id                                                   { $$ = YACC_BASE_SPECIFIER($1); }
    |                       access_specifier base_specifier                             { $$ = YACC_ACCESS_BASE_SPECIFIER($2, $1); }
    |                       VIRTUAL base_specifier                                      { $$ = YACC_VIRTUAL_BASE_SPECIFIER($2); }
access_specifier:           PRIVATE | PROTECTED | PUBLIC

/*---------------------------------------------------------------------------------------------------
 * A.10 Special member functions
 *---------------------------------------------------------------------------------------------------*/
conversion_function_id:     OPERATOR conversion_type_id                                 { $$ = YACC_CONVERSION_FUNCTION_ID($2); }
conversion_type_id:         type_specifier ptr_operator_seq.opt                         { $$ = YACC_TYPED_EXPRESSION($1, $2); }
    |                       type_specifier conversion_type_id                           { $$ = YACC_TYPED_EXPRESSION($1, $2); }
/*
 *  Ctor-initialisers can look like a bit field declaration, given the generalisation of names:
 *      Class(Type) : m1(1), m2(2) {}
 *      NonClass(bit_field) : int(2), second_variable, ...
 *  The grammar below is used within a function_try_block or function_definition.
 *  See simple_member_declaration for use in normal member function_definition.
 */
ctor_initializer.opt:       /* empty */                                                 { $$ = YACC_MEM_INITIALIZERS(0, 0); }
    |                       ctor_initializer
ctor_initializer:           ':' mem_initializer_list                                    { $$ = $2; }
    |                       ':' mem_initializer_list bang error                         { $$ = $2; YACC_UNBANG($3, "Bad ctor-initializer."); }
mem_initializer_list:       mem_initializer                                             { $$ = YACC_MEM_INITIALIZERS(0, $1); }
    |                       mem_initializer_list_head mem_initializer                   { $$ = YACC_MEM_INITIALIZERS($1, $2); }
mem_initializer_list_head:  mem_initializer_list ','
    |                       mem_initializer_list bang error ','                         { YACC_UNBANG($2, "Bad mem-initializer."); }
mem_initializer:            mem_initializer_id '(' expression_list.opt ')'              { $$ = YACC_MEM_INITIALIZER($1, $3); }
mem_initializer_id:         scoped_id

/*---------------------------------------------------------------------------------------------------
 * A.11 Overloading
 *---------------------------------------------------------------------------------------------------*/
operator_function_id:       OPERATOR operator                                           { $$ = YACC_OPERATOR_FUNCTION_ID($2); }
/*
 *  It is not clear from the ANSI standard whether spaces are permitted in delete[]. If not then it can
 *  be recognised and returned as DELETE_ARRAY by the lexer. Assuming spaces are permitted there is an
 *  ambiguity created by the over generalised nature of expressions. operator new is a valid delarator-id
 *  which we may have an undimensioned array of. Semantic rubbish, but syntactically valid. Since the
 *  array form is covered by the declarator consideration we can exclude the operator here. The need
 *  for a semantic rescue can be eliminated at the expense of a couple of shift-reduce conflicts by
 *  removing the comments on the next four lines.
 */
operator:             /*++++*/      NEW                                                         { $$ = YACC_OPERATOR_NEW_ID(); }
    |                 /*++++*/      DELETE                                                      { $$ = YACC_OPERATOR_DELETE_ID(); }
/*  |                 / ---- /      NEW                 %prec SHIFT_THERE                       { $$ = YACC_OPERATOR_NEW_ID(); }
/*  |                 / ---- /      DELETE              %prec SHIFT_THERE                       { $$ = YACC_OPERATOR_DELETE_ID(); }
/*  |                 / ---- /      NEW '[' ']'                                                 -- Covered by array of OPERATOR NEW */
/*  |                 / ---- /      DELETE '[' ']'                                              -- Covered by array of OPERATOR DELETE */
    |                               '+'                                                         { $$ = YACC_OPERATOR_ADD_ID(); }
    |                               '-'                                                         { $$ = YACC_OPERATOR_SUB_ID(); }
    |                               '*'                                                         { $$ = YACC_OPERATOR_MUL_ID(); }
    |                               '/'                                                         { $$ = YACC_OPERATOR_DIV_ID(); }
    |                               '%'                                                         { $$ = YACC_OPERATOR_MOD_ID(); }
    |                               '^'                                                         { $$ = YACC_OPERATOR_XOR_ID(); }
    |                               '&'                                                         { $$ = YACC_OPERATOR_BIT_AND_ID(); }
    |                               '|'                                                         { $$ = YACC_OPERATOR_BIT_OR_ID(); }
    |                               '~'                                                         { $$ = YACC_OPERATOR_BIT_NOT_ID(); }
    |                               '!'                                                         { $$ = YACC_OPERATOR_LOG_NOT_ID(); }
    |                               '='                                                         { $$ = YACC_OPERATOR_ASS_ID(); }
    |                               '<'                                                         { $$ = YACC_OPERATOR_LT_ID(); }
    |                               '>'                                                         { $$ = YACC_OPERATOR_GT_ID(); }
    |                               ASS_ADD                                                     { $$ = YACC_OPERATOR_ASS_ADD_ID(); }
    |                               ASS_SUB                                                     { $$ = YACC_OPERATOR_ASS_SUB_ID(); }
    |                               ASS_MUL                                                     { $$ = YACC_OPERATOR_ASS_MUL_ID(); }
    |                               ASS_DIV                                                     { $$ = YACC_OPERATOR_ASS_DIV_ID(); }
    |                               ASS_MOD                                                     { $$ = YACC_OPERATOR_ASS_MOD_ID(); }
    |                               ASS_XOR                                                     { $$ = YACC_OPERATOR_ASS_XOR_ID(); }
    |                               ASS_AND                                                     { $$ = YACC_OPERATOR_ASS_BIT_AND_ID(); }
    |                               ASS_OR                                                      { $$ = YACC_OPERATOR_ASS_BIT_OR_ID(); }
    |                               SHL                                                         { $$ = YACC_OPERATOR_SHL_ID(); }
    |                               SHR                                                         { $$ = YACC_OPERATOR_SHR_ID(); }
    |                               ASS_SHR                                                     { $$ = YACC_OPERATOR_ASS_SHR_ID(); }
    |                               ASS_SHL                                                     { $$ = YACC_OPERATOR_ASS_SHL_ID(); }
    |                               EQ                                                          { $$ = YACC_OPERATOR_EQ_ID(); }
    |                               NE                                                          { $$ = YACC_OPERATOR_NE_ID(); }
    |                               LE                                                          { $$ = YACC_OPERATOR_LE_ID(); }
    |                               GE                                                          { $$ = YACC_OPERATOR_GE_ID(); }
    |                               LOG_AND                                                     { $$ = YACC_OPERATOR_LOG_AND_ID(); }
    |                               LOG_OR                                                      { $$ = YACC_OPERATOR_LOG_OR_ID(); }
    |                               INC                                                         { $$ = YACC_OPERATOR_INC_ID(); }
    |                               DEC                                                         { $$ = YACC_OPERATOR_DEC_ID(); }
    |                               ','                                                         { $$ = YACC_OPERATOR_COMMA_ID(); }
    |                               ARROW_STAR                                                  { $$ = YACC_OPERATOR_ARROW_STAR_ID(); }
    |                               ARROW                                                       { $$ = YACC_OPERATOR_ARROW_ID(); }
    |                               '(' ')'                                                     { $$ = YACC_OPERATOR_CALL_ID(); }
    |                               '[' ']'                                                     { $$ = YACC_OPERATOR_INDEX_ID(); }

/*---------------------------------------------------------------------------------------------------
 * A.12 Templates
 *---------------------------------------------------------------------------------------------------*/
template_declaration:               template_parameter_clause declaration                       { $$ = YACC_TEMPLATE_DECLARATION($1, $2); }
    |                               EXPORT template_declaration                                 { $$ = YACC_DECL_SPECIFIER_DECLARATION($2, $1); }
template_parameter_clause:          TEMPLATE '<' template_parameter_list '>'                    { $$ = $3; }
template_parameter_list:            template_parameter                                          { $$ = YACC_TEMPLATE_PARAMETERS(0, $1); }
    |                               template_parameter_list ',' template_parameter              { $$ = YACC_TEMPLATE_PARAMETERS($1, $3); }
template_parameter:                 simple_type_parameter                                       { $$ = YACC_INIT_SIMPLE_TYPE_PARAMETER($1, 0); }
    |                               simple_type_parameter '=' type_id                           { $$ = YACC_INIT_SIMPLE_TYPE_PARAMETER($1, $3); }
    |                               templated_type_parameter                                    { $$ = YACC_INIT_TEMPLATED_PARAMETER($1, 0); }
    |                               templated_type_parameter '=' identifier                     { $$ = YACC_INIT_TEMPLATED_PARAMETER($1, $3); }
    |                               templated_parameter_declaration                             { $$ = YACC_TEMPLATE_PARAMETER($1); }
    |                               bang error                                                  { $$ = 0; YACC_UNBANG($1, "Bad template-parameter."); }
simple_type_parameter:              CLASS                                                       { $$ = YACC_CLASS_TYPE_PARAMETER(0); }
/*  |                               CLASS identifier                                            -- covered by parameter_declaration */
    |                               TYPENAME                                                    { $$ = YACC_TYPENAME_TYPE_PARAMETER(0); }
/*  |                               TYPENAME identifier                                         -- covered by parameter_declaration */
templated_type_parameter:           template_parameter_clause CLASS                             { $$ = YACC_TEMPLATED_TYPE_PARAMETER($1, 0); }
    |                               template_parameter_clause CLASS identifier                  { $$ = YACC_TEMPLATED_TYPE_PARAMETER($1, $3); }
template_id:                        TEMPLATE identifier '<' template_argument_list '>'          { $$ = YACC_TEMPLATE_NAME($2, $4); }
    |                               TEMPLATE template_id                                        { $$ = $2; }
/*
 *  template-argument is evaluated using a templated...expression so that > resolves to end of template.
 */
template_argument_list:             template_argument                                           { $$ = YACC_TEMPLATE_ARGUMENTS(0, $1); }
    |                               template_argument_list ',' template_argument                { $$ = YACC_TEMPLATE_ARGUMENTS($1, $3); }
template_argument:                  templated_parameter_declaration                             { $$ = YACC_TEMPLATE_ARGUMENT($1); }
/*  |                               type_id                                                     -- covered by templated_parameter_declaration */
/*  |                               template_name                                               -- covered by templated_parameter_declaration */
/*  |                               error                                                       -- must allow template failure to re-search */

/*
 *  Generalised naming makes identifier a valid declaration, so TEMPLATE identifier is too.
 *  The TEMPLATE prefix is therefore folded into all names, parenthesis_clause and decl_specifier_prefix.
 */
/*explicit_instantiation:           TEMPLATE declaration */
explicit_specialization:            TEMPLATE '<' '>' declaration                                { $$ = YACC_EXPLICIT_SPECIALIZATION($4); }

/*---------------------------------------------------------------------------------------------------
 * A.13 Exception Handling
 *---------------------------------------------------------------------------------------------------*/
try_block:                          TRY compound_statement handler_seq                          { $$ = YACC_TRY_BLOCK($2, $3); }
/*function_try_block:                                                                           -- moved near function_block */
handler_seq:                        handler                                                     { $$ = YACC_HANDLERS(0, $1); }
    |                               handler handler_seq                                         { $$ = YACC_HANDLERS($2, $1); }
handler:                            CATCH '(' exception_declaration ')' compound_statement      { $$ = YACC_HANDLER($3, $5); }
exception_declaration:              parameter_declaration                                       { $$ = YACC_EXCEPTION_DECLARATION($1); }
/*                                  ELLIPSIS                                                    -- covered by parameter_declaration */
throw_expression:                   THROW                                                       { $$ = YACC_THROW_EXPRESSION(0); }
    |                               THROW assignment_expression                                 { $$ = YACC_THROW_EXPRESSION($2); }
templated_throw_expression:         THROW                                                       { $$ = YACC_THROW_EXPRESSION(0); }
    |                               THROW templated_assignment_expression                       { $$ = YACC_THROW_EXPRESSION($2); }
exception_specification:            THROW '(' ')'                                               { $$ = YACC_EXCEPTION_SPECIFICATION(0); }
    |                               THROW '(' type_id_list ')'                                  { $$ = YACC_EXCEPTION_SPECIFICATION($3); }
type_id_list:                       type_id                                                     { $$ = YACC_EXPRESSIONS(0, $1); }
    |                               type_id_list ',' type_id                                    { $$ = YACC_EXPRESSIONS($1, $3); }

/*---------------------------------------------------------------------------------------------------
 * Back-tracking and context support
 *---------------------------------------------------------------------------------------------------*/
advance_search:                     error               { yyerrok; advance_search(); } /* Rewind and queue '+' or '-' '#' */       
bang:                               /* empty */         { $$ = YACC_BANG(); }   /* set flag to suppress "parse error" */ 
mark:                               /* empty */         { $$ = mark(); }        /* Push lookahead and input token stream context onto a stack */
nest:                               /* empty */         { $$ = nest(); }        /* Push a declaration nesting depth onto the parse stack */
start_search:                       /* empty */         { $$ = YACC_LINE(); start_search(false); }    /* Create/reset binary search context */
start_search1:                      /* empty */         { $$ = YACC_LINE(); start_search(true); }     /* Create/reset binary search context */
util:                               /* empty */         { $$ = YACC_UTILITY_MODE(); }           /* Get current utility mode */
/*StartTester*/
%%

#include <CxxParsing.cxx>
/*EndTester*/
