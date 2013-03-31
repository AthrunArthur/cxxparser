#ifndef CXX_LEXING_HXX
#define CXX_LEXING_HXX

#include <CxxToken.hxx>

#include <CxxParser.tab.h>

CxxToken *yyToken;
CxxToken *make_character(const char *someText, size_t aLength);
CxxToken *make_string(const char *someText, size_t aLength);
CxxToken *make_identifier(const char *someText, size_t aLength);
void make_line(const char *yyText, size_t yyLeng);
CxxToken *make_literal_character(const char *someText, size_t aLength);
CxxToken *make_narrow_character(const char *someText, size_t aLength);
CxxToken *make_narrow_string(const char *someText, size_t aLength);
CxxToken *make_number(const char *someText, size_t aLength);
CxxToken *make_token(size_t aCxxToken);
CxxToken *make_wide_character(const char *someText, size_t aLength);
CxxToken *make_wide_string(const char *someText, size_t aLength);

#define LEX_SAVE_LINE(yyText, yyLeng) make_line(yyText, yyLeng);
#define LEX_ASCII_TOKEN(a) yyToken = make_token(a); return true;
#define LEX_STATIC_TOKEN(a) static CxxToken theToken(PARSE_TOKEN(a)); yyToken = &theToken; return true;
#define LEX_C_STATIC_TOKEN(a) \
	if (c_keywords) { LEX_IDENTIFIER_TOKEN(yytext, yyleng) } \
	else { LEX_STATIC_TOKEN(a) } return true;
#define LEX_ESCAPED_TOKEN(yyText, yyLeng) LEX_STATIC_TOKEN(CharacterLiteral)
//	yyToken = make_literal_character(yytext, yyleng); return true;
#define LEX_CHARACTER_TOKEN(yyText, yyLeng) LEX_STATIC_TOKEN(CharacterLiteral)
//	yyToken = make_character(yyText, yyLeng); return true;
#define LEX_STRING_TOKEN(yyText, yyLeng) LEX_STATIC_TOKEN(StringLiteral)
//	yyToken = make_string(yyText, yyLeng); return true;
#define LEX_IDENTIFIER_TOKEN(yyText, yyLeng) LEX_STATIC_TOKEN(Identifier)
//	yyToken = make_identifier(yyText, yyLeng); return true;
#define LEX_NUMBER_TOKEN(yyText, yyLeng) LEX_STATIC_TOKEN(IntegerLiteral)
//	yyToken = make_number(yyText, yyLeng); return true;
#endif