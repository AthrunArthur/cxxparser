#include <CxxToken.hxx>
#include <memory.h>

//CxxToken::CxxToken()
//:
//	_value(0)
//{}

CxxNaffToken::CxxNaffToken(int tokenValue, const char *yyText, int yyLeng)
:
	Super(tokenValue), _text(new char[yyLeng+1]), _leng(yyLeng)
{
	memcpy(_text, yyText, yyLeng);
	_text[_leng] = 0;
}

CxxNaffToken::~CxxNaffToken() { delete[] _text; }
