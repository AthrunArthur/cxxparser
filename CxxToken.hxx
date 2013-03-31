#ifndef CXXTOKEN_HXX
#define CXXTOKEN_HXX

#include <iostream>
#include <stdlib.h>

#define YYSTYPE CxxTokenType
//#define YYSTYPE_IS_DECLARED 1
#define YY_parse_STYPE CxxTokenType
#define YACC_BANG_TYPE size_t
#define YACC_MARK_TYPE size_t

#define YACC_BANG() push_bang()
#define YACC_UNBANG(bangValue, msg) pop_bang(bangValue); yyerrok; yyclearin; yyerror(msg);

#define ERRMSG(a) do { std::cout << "ERROR -- " << a << std::endl; increment_error_count(); } while (0)

#ifdef NEEDS_BOOL
enum bool { false, true };
#endif

#ifdef BISON_PP_CLASS
#define PARSE_TOKEN(a) BISON_PP_CLASS::a
#else
#define PARSE_TOKEN(a) a
#endif

extern size_t line_number;
extern bool c_keywords;
extern bool echo_line_numbers;
extern bool echo_line_text;
extern void increment_error_count();
extern int tokenMarkDepth;

class CxxToken
{
	int _value;
private:
	CxxToken(const CxxToken&);
	CxxToken& operator=(const CxxToken&);
public:
	CxxToken(int tokenValue = 0) : _value(tokenValue) {}
	virtual ~CxxToken() {}
	int value() const { return _value; }
};

enum CxxIsTemplate { IS_DEFAULT, IS_TEMPLATE };
enum CxxIsTree { IS_SCALAR, IS_TREE };
typedef CxxToken CxxTreeArgument;

class CxxStatement : public CxxToken {};
typedef CxxStatement CxxDeclaration;
class CxxExpression : public CxxStatement {};
class CxxName : public CxxExpression {};
class CxxTokens : public CxxToken {};
class CxxMetaObject : public CxxExpression {};
class CxxMetaStatement : public CxxStatement {};

class CxxStatements : public CxxStatement {};
typedef CxxStatements CxxDeclarations;
typedef CxxDeclarations CxxMemberDeclarations;
typedef CxxExpression CxxTreeExpression;

class CxxKeyword : public CxxName {};
class CxxDeclSpecifierId : public CxxKeyword {};

class CxxAccessSpecifier : public CxxKeyword {};
class CxxBaseSpecifier : public CxxToken {};
class CxxBaseSpecifiers : public CxxTokens {};
class CxxBrace : public CxxToken {};
class CxxBuiltInId : public CxxName {};
class CxxCharacterLiteral : public CxxToken {};
class CxxClass : public CxxToken {};
class CxxClassKey : public CxxKeyword {};
class CxxCondition : public CxxExpression {};
class CxxCvQualifiers : public CxxDeclSpecifierId {};
class CxxDeclarator : public CxxToken {};
typedef CxxExpression CxxDeleteExpression;
//class CxxDerived : public CxxToken {};
//class CxxEnum : public CxxToken {};
class CxxEnumerator : public CxxToken {};
class CxxEnumerators : public CxxTokens {};
class CxxExceptionDeclaration : public CxxToken {};
class CxxExceptionSpecification : public CxxToken {};
class CxxExpressions : public CxxExpression {};
class CxxFileId : public CxxToken {};
class CxxFileIds : public CxxTokens {};
class CxxFileName : public CxxToken {};
class CxxFloatingLiteral : public CxxToken {};
class CxxFunctionBody : public CxxStatement {};
class CxxFunctionDeclarations : public CxxDeclarations {};
class CxxHandler : public CxxToken {};
class CxxHandlers : public CxxTokens {};
class CxxIdentifier : public CxxName {};
//class CxxIds : public CxxTokens {};
class CxxInitializerClause : public CxxExpression {};
class CxxInitializerClauses : public CxxInitializerClause {};
class CxxIntegerLiteral : public CxxToken {};
class CxxLine : public CxxToken {};
//class CxxList : public CxxTokens {};
class CxxMemInitializer : public CxxToken {};
class CxxMemInitializers : public CxxTokens {};
class CxxMetaClass : public CxxStatement {};
class CxxMetaFunction : public CxxMetaObject {};
class CxxMetaInitializer : public CxxToken {};
class CxxMetaInitializers : public CxxTokens {};
class CxxMetaParameter : public CxxToken {};
class CxxMetaParameters : public CxxTokens {};
//class CxxMetaPrototype : public CxxToken {};
//class CxxMetaPrototypes : public CxxTokens {};
class CxxMetaType : public CxxName {};
class CxxMetaVariable : public CxxMetaObject {};
class CxxNamespace : public CxxToken {};
typedef CxxExpression CxxNewExpression;
class CxxNumberLiteral : public CxxExpression {};
class CxxParameter : public CxxExpression {};
class CxxParameters : public CxxExpression {};
class CxxParenthesised : public CxxToken {};
class CxxPointerDeclarator : public CxxDeclarator {};
class CxxPosition : public CxxName {};
class CxxSegment : public CxxName {};
class CxxSpacing : public CxxToken {};
class CxxStrings : public CxxToken {};
typedef CxxStrings CxxStringLiteral;
class CxxSubspace : public CxxToken {};
class CxxSyntaxMacroParameter : public CxxToken {};
class CxxSyntaxMacroParameters : public CxxTokens {};
class CxxTemplateArgument : public CxxToken {};
class CxxTemplateArguments : public CxxTokens {};
class CxxTemplateParameter : public CxxToken {};
class CxxTemplateParameters : public CxxTokens {};
class CxxSimpleTypeParameter : public CxxTemplateParameter {};
class CxxTemplatedTypeParameter : public CxxTemplateParameter {};
class CxxTokenStatements : public CxxTokens {};
class CxxTreeArguments : public CxxTokens {};
class CxxType1Parameters : public CxxTokens {};
class CxxTypeId : public CxxToken {};
class CxxTypeIds : public CxxTokens {};
class CxxUtility : public CxxToken {};

#define FOGPARSERVALUE_ENUM(T,N) \
	const T *name2(u_,N); \
	const T& N() const { return *name2(u_,N); } \
	const T* & N() { return name2(u_,N); }
#define FOGPARSERVALUE_POINTER(T,N) T *N;
#define FOGPARSERVALUE_VALUE(T,N) T N;

union CxxTokenType
{
		CxxToken *_token;

		FOGPARSERVALUE_VALUE(bool, _bool)
		FOGPARSERVALUE_VALUE(long, _long)
		FOGPARSERVALUE_POINTER(CxxBrace, brace)
		FOGPARSERVALUE_POINTER(CxxSpacing, spacing)

		FOGPARSERVALUE_POINTER(CxxAccessSpecifier, access_specifier)
		FOGPARSERVALUE_POINTER(CxxBaseSpecifier, base_specifier)
		FOGPARSERVALUE_POINTER(CxxBaseSpecifiers, base_specifiers)
		FOGPARSERVALUE_POINTER(CxxBuiltInId, built_in_id)
		FOGPARSERVALUE_POINTER(CxxCharacterLiteral, character_literal)
		FOGPARSERVALUE_POINTER(CxxClass, _class)
		FOGPARSERVALUE_POINTER(CxxClassKey, class_key)
		FOGPARSERVALUE_POINTER(CxxCondition, condition)
		FOGPARSERVALUE_POINTER(CxxCvQualifiers, cv_qualifiers)
		FOGPARSERVALUE_POINTER(CxxDeclSpecifierId, decl_specifier_id)
		FOGPARSERVALUE_POINTER(CxxDeclaration, declaration)
		FOGPARSERVALUE_POINTER(CxxDeclarations, declarations)
		FOGPARSERVALUE_POINTER(CxxDeclarator, declarator)
		FOGPARSERVALUE_POINTER(CxxDeleteExpression, delete_expression)
		FOGPARSERVALUE_POINTER(CxxEnumerator, enumerator)
		FOGPARSERVALUE_POINTER(CxxEnumerators, enumerators)
		FOGPARSERVALUE_POINTER(CxxExceptionDeclaration, exception_declaration)
		FOGPARSERVALUE_POINTER(CxxExceptionSpecification, exception_specification)
		FOGPARSERVALUE_POINTER(CxxExpression, expression)
		FOGPARSERVALUE_POINTER(CxxExpressions, expressions)
		FOGPARSERVALUE_POINTER(CxxFileId, file_id)
		FOGPARSERVALUE_POINTER(CxxFileIds, file_ids)
		FOGPARSERVALUE_POINTER(CxxFileName, file_name)
		FOGPARSERVALUE_POINTER(CxxFloatingLiteral, floating_literal)
		FOGPARSERVALUE_POINTER(CxxFunctionBody, function_body)
		FOGPARSERVALUE_POINTER(CxxHandler, handler)
		FOGPARSERVALUE_POINTER(CxxHandlers, handlers)
		FOGPARSERVALUE_POINTER(CxxIdentifier, identifier)
		FOGPARSERVALUE_POINTER(CxxInitializerClause, initializer_clause)
		FOGPARSERVALUE_POINTER(CxxInitializerClauses, initializer_clauses)
		FOGPARSERVALUE_POINTER(CxxIntegerLiteral, integer_literal)
		FOGPARSERVALUE_POINTER(CxxKeyword, keyword)
		FOGPARSERVALUE_POINTER(CxxLine, line)
		FOGPARSERVALUE_POINTER(CxxMemInitializer, mem_initializer)
		FOGPARSERVALUE_POINTER(CxxMemInitializers, mem_initializers)
		FOGPARSERVALUE_POINTER(CxxMemberDeclarations, member_declarations)
		FOGPARSERVALUE_POINTER(CxxMetaClass, meta_class)
		FOGPARSERVALUE_POINTER(CxxMetaFunction, meta_function)
		FOGPARSERVALUE_POINTER(CxxMetaInitializer, meta_initializer)
		FOGPARSERVALUE_POINTER(CxxMetaInitializers, meta_initializers)
		FOGPARSERVALUE_POINTER(CxxMetaObject, meta_object)
		FOGPARSERVALUE_POINTER(CxxMetaStatement, meta_statement)
		FOGPARSERVALUE_POINTER(CxxMetaType, meta_type)
		FOGPARSERVALUE_POINTER(CxxMetaVariable, meta_variable)
		FOGPARSERVALUE_POINTER(CxxName, name)
		FOGPARSERVALUE_POINTER(CxxNewExpression, new_expression)
		FOGPARSERVALUE_POINTER(CxxNumberLiteral, number_literal)
		FOGPARSERVALUE_POINTER(CxxParameter, parameter)
		FOGPARSERVALUE_POINTER(CxxParameters, parameters)
		FOGPARSERVALUE_POINTER(CxxParenthesised, parenthesised)
		FOGPARSERVALUE_POINTER(CxxPointerDeclarator, pointer_declarator)
		FOGPARSERVALUE_POINTER(CxxPosition, position)
		FOGPARSERVALUE_POINTER(CxxSegment, segment)
		FOGPARSERVALUE_POINTER(CxxSimpleTypeParameter, simple_type_parameter)
		FOGPARSERVALUE_POINTER(CxxStatement, statement)
		FOGPARSERVALUE_POINTER(CxxStatements, statements)
		FOGPARSERVALUE_POINTER(CxxStringLiteral, string_literal)
		FOGPARSERVALUE_POINTER(CxxStrings, strings)
		FOGPARSERVALUE_POINTER(CxxSubspace, subspace)
		FOGPARSERVALUE_POINTER(CxxSyntaxMacroParameter, syntax_macro_parameter)
		FOGPARSERVALUE_POINTER(CxxSyntaxMacroParameters, syntax_macro_parameters)
		FOGPARSERVALUE_POINTER(CxxTemplateArgument, template_argument)
		FOGPARSERVALUE_POINTER(CxxTemplateArguments, template_arguments)
		FOGPARSERVALUE_POINTER(CxxTemplateParameter, template_parameter)
		FOGPARSERVALUE_POINTER(CxxTemplateParameters, template_parameters)
		FOGPARSERVALUE_POINTER(CxxTemplatedTypeParameter, templated_type_parameter)
		FOGPARSERVALUE_POINTER(CxxToken, token)
		FOGPARSERVALUE_POINTER(CxxTokenStatements, token_statements)
		FOGPARSERVALUE_POINTER(CxxTokens, tokens)
		FOGPARSERVALUE_POINTER(CxxTreeArgument, tree_argument)
		FOGPARSERVALUE_POINTER(CxxTreeArguments, tree_arguments)
		FOGPARSERVALUE_POINTER(CxxTreeExpression, tree_expression)
		FOGPARSERVALUE_POINTER(CxxType1Parameters, type1_parameters)
		FOGPARSERVALUE_POINTER(CxxUtility, utility)

		FOGPARSERVALUE_VALUE(int, bang)
		FOGPARSERVALUE_VALUE(CxxIsTemplate, is_template)
		FOGPARSERVALUE_VALUE(YACC_MARK_TYPE, mark)
		FOGPARSERVALUE_VALUE(size_t, nest)
#if 0
	CxxAccessSpecifier *access_specifier;
	CxxBaseSpecifier *base_specifier;
	CxxBaseSpecifiers *base_specifiers;
	CxxBuiltInId *built_in_id;
	CxxCharacterLiteral *character_literal;
	CxxClass *_class;
	CxxClassKey *class_key;
	CxxCondition *condition;
	CxxCvQualifiers *cv_qualifiers;
	CxxDeclaration *declaration;
	CxxDeclarations *declarations;
	CxxDeclarator *declarator;
	CxxDeclSpecifierId *decl_specifier_id;
//	CxxDerived *derived;
//	CxxEnum *_enum;
	CxxEnumerator *enumerator;
	CxxEnumerators *enumerators;
	CxxExceptionDeclaration *exception_declaration;
	CxxExceptionSpecification *exception_specification;
	CxxExpression *expression;
	CxxExpressions *expressions;
	CxxFileId *file_id;
	CxxFileIds *file_ids;
	CxxFileName *file_name;
	CxxFloatingLiteral *floating_literal;
	CxxFunctionBody *function_body;
	CxxFunctionDeclarations *function_declarations;
	CxxHandler *handler;
	CxxHandlers *handlers;
	CxxIdentifier *identifier;
//	CxxIds *ids;
	CxxInitializerClause *initializer_clause;
	CxxInitializerClauses *initializer_clauses;
	CxxIntegerLiteral *integer_literal;
	CxxKeyword *keyword;
	CxxLine *line;
//	CxxList *list;
	CxxMemInitializer *mem_initializer;
	CxxMemInitializers *mem_initializers;
	CxxMemberDeclarations *member_declarations;
	CxxMetaClass *meta_class;
//	CxxMetaFunction *meta_function;
//	CxxMetaInitializer *meta_initializer;
//	CxxMetaInitializers *meta_initializers;
//	CxxMetaObject *meta_object;
	CxxMetaParameter *meta_parameter;
	CxxMetaParameters *meta_parameters;
//	CxxMetaPrototype *meta_prototype;
//	CxxMetaPrototypes *meta_prototypes;
//	CxxMetaStatement *meta_statement;
	CxxMetaType *meta_type;
//	CxxMetaVariable *meta_variable;
	CxxName *name;
//	CxxNamespace *_namespace;
	CxxNumberLiteral *number_literal;
	CxxParameter *parameter;
	CxxParameters *parameters;
	CxxParenthesised *parenthesised;
	CxxPointerDeclarator *pointer_declarator;
	CxxSegment *segment;
	CxxSimpleTypeParameter *simple_type_parameter;
	CxxStatement *statement;
	CxxStatements *statements;
	CxxStringLiteral *string_literal;
	CxxStrings *strings;
	CxxSyntaxMacroParameter *syntax_macro_parameter;
	CxxSyntaxMacroParameters *syntax_macro_parameters;
	CxxTemplateArgument *template_argument;
	CxxTemplateArguments *template_arguments;
	CxxTemplateParameter *template_parameter;
	CxxTemplateParameters *template_parameters;
	CxxTemplatedTypeParameter *templated_type_parameter;
	CxxToken *token;
	CxxTokens *tokens;
//	CxxTreeArgument *tree_argument;
//	CxxTreeArguments *tree_arguments;
//	CxxTreeExpression *tree_expression;
	CxxType1Parameters *type1_parameters;
//	CxxTypeId *type_id;
//	CxxTypeIds *type_ids;
	CxxUtility *utility;
	YACC_BANG_TYPE bang;
	YACC_MARK_TYPE mark;
	size_t nest;
#endif
};

class CxxNaffToken : public CxxToken
{
	typedef CxxToken Super;
	char *_text;
	int _leng;
private:
	CxxNaffToken(const CxxNaffToken&);
	CxxNaffToken& operator=(const CxxNaffToken&);
public:
	CxxNaffToken(int tokenValue, const char *yyText, int yyLeng);
	virtual ~CxxNaffToken();
};

#endif
