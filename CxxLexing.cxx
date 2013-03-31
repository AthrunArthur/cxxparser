#ifdef FLEX_PP_CLASS
FLEX_PP_CLASS theLexer;
#define LEX_DOT theLexer .
#else
#define LEX_DOT
#endif

#include "CxxToken.hxx"

#include "CxxLexing.hxx"
#include "CxxParser.tab.h"
#include <iostream>
#include <stdint.h>
using namespace std;
bool c_keywords = false;
bool echo_line_numbers = false;
bool echo_line_text = false;
size_t line_number = 0;

#ifdef NEEDS_YYWRAP
int yywrap() { return 1; }
#endif
extern int yylex (void);
CxxToken *yylex_token()
{
	if (!LEX_DOT yylex())
		return 0;
	return yyToken;
}

//
//	Configure the lexer to reflect successful parsing of a character value, assigning it to yylval.
//
//	The source someText[aLength] should correspond to the parsed text including any L or ' prefix
//	but excluding any ' suffix. In this way the return can indicate whether a wide character has
//	been detected and the routine can accommodate a variety of erroneous terminations.
//
CxxToken *make_character(const char *someText, size_t aLength)
{
	bool isWide = false;
	if (someText && aLength)
	{
		if (*someText == 'L')
		{
			isWide = true;
			someText++;
			aLength--;
		}
		if (!aLength || (*someText != '\''))
			ERRMSG("BUG - bad start of character literal.");
		if (aLength)
		{
			someText++;
			aLength--;
		}
	}
	if (isWide)
		return make_wide_character(someText, aLength);
	else
		return make_narrow_character(someText, aLength);
}

CxxToken *make_identifier(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(Identifier), someText, aLength);
}

//
//	Buffer the incoming line, before any characters are analysed.
//
void make_line(const char *yyText, size_t yyLeng)
{
	if (echo_line_text)
		cout << tokenMarkDepth << ": "  << line_number << ": " << yyText << flush; 
	else if (echo_line_numbers)
		cout << line_number << endl; 
	line_number++ ; 
}

CxxToken *make_literal_character(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(CharacterLiteral), someText, aLength);
}

CxxToken *make_narrow_character(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(CharacterLiteral), someText, aLength);
}

CxxToken *make_narrow_string(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(StringLiteral), someText, aLength);
}

CxxToken *make_number(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(IntegerLiteral), someText, aLength);
}

//
//	Configure the lexer to reflect successful parsing of a categorised string.
//
//	The source someText[aLength] should correspond to the parsed text including any
//	L or " prefix but excluding any " suffix. In this way the return can indicate whether a wide
//	character has been detected and the routine can accommodate a variety of erroneous terminations.
//
CxxToken *make_string(const char *someText, size_t aLength)
{
	bool isWide = false;
	if (someText && aLength)
	{
		if (*someText == 'L')
		{
			isWide = true;
			someText++;
			aLength--;
		}
		if (!aLength || (*someText != '"'))
			ERRMSG("BUG - bad start of string literal.");
		if (aLength)
		{
			someText++;
			aLength--;
		}
	}
	if (isWide)
		return make_wide_string(someText, aLength);
	else
		return make_narrow_string(someText, aLength);
}

//
//	Return the appropriate 1 of 256 flyweight tokens for the ASCII characters.
//
CxxToken *make_token(size_t tokenValue)
{
	static CxxToken *asciiTokens[256];
	if (tokenValue >= (sizeof(asciiTokens)/sizeof(asciiTokens[0])))
	{
		ERRMSG("Cannot make_token for " << tokenValue);
		return 0;
	}
	CxxToken **p = &asciiTokens[tokenValue];
	CxxToken *theToken = *p;
	if (!theToken)
		*p = theToken = new CxxToken(tokenValue);
	return theToken;
}

CxxToken *make_wide_character(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(CharacterLiteral), someText, aLength);
}

CxxToken *make_wide_string(const char *someText, size_t aLength)
{
	return new CxxNaffToken(PARSE_TOKEN(StringLiteral), someText, aLength);
}

