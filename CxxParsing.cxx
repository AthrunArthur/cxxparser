#include <iostream>
#include <memory.h>
//#include <CxxToken.hxx>
//#include "CxxParsing.hxx"

extern CxxToken *yylex_token();
extern int yyparse();
extern int yychar;
#ifdef BISON_PP_CLASS
BISON_PP_CLASS theParser;
#define PARSE_DOT theParser .
#define PARSE_SCOPE BISON_PP_CLASS ::
#else
#define PARSE_DOT
#define PARSE_SCOPE
extern int yydebug;
#ifndef YYEMPTY
#define YYEMPTY -1
#endif
#endif

class CxxSearchContext
{
    friend class ios;                   // Suppress GNU error message for no public constructors.
	CxxSearchContext *_next;
	size_t _index;
	size_t _depth;
	size_t _size;
	size_t _mark;
	bool _enable_type1;
	size_t _line;
	size_t _advances;
	bool _status[32];
private:
	CxxSearchContext(CxxSearchContext *nextSearch)
		: _next(nextSearch), _index(0), _depth(0), _size(sizeof(_status)/sizeof(_status[0])),
		_mark(0), _enable_type1(false), _line(0), _advances(0) {}
	CxxSearchContext(const CxxSearchContext&);
	CxxSearchContext& operator=(const CxxSearchContext&);
	bool did_search() const { return _depth > 0 ? true : false; }
	void initialise(size_t markIndex, bool enableType1);
	CxxSearchContext *queue(CxxSearchContext *& listHead);
	void reset();
public:
	bool advance();
	bool enable_type1() const { return _enable_type1; }
	bool is_template();
	size_t mark() const { return _mark; }
private:
	static CxxSearchContext *_current;
	static CxxSearchContext *_free;
public:
	static size_t actual_searches;
	static size_t advances[16];
	static size_t max_search_depth;
	static size_t nested_searches;
	static size_t releases;
	static size_t search_advances;
	static size_t unnested_searches;
public:
	static CxxSearchContext *current() { return _current; }
	static void release();
	static void start(YACC_MARK_TYPE anIndex, bool enableType1);
};

size_t bang_depth = 0;
size_t error_count = 0;
size_t marked_error_count = 0;
bool in_type1 = false;
bool show_marked = false;
using namespace std;
int yydebug = 1;
extern FILE * yyin;
int main(int argc, char *argv[])
{
	for (--argc, ++argv; argc-- > 0; ++argv)
	{
		char *p = *argv;
		if (*p == '-')
		{
			switch (*(p+1))
			{
				case 'c':
					c_keywords = true;
					break;
				case 't':
					echo_line_text = true;
					break;
				case 'm':
					show_marked = true;
					break;
				case 'n':
					echo_line_numbers = true;
					break;
				case 'y':
					PARSE_DOT yydebug = true;
					break;
			}
		}
		else 
		{
			yyin = fopen(p ,"r");
		}
	}
	if (PARSE_DOT yyparse() != 0)
		ERRMSG("Failed to parse to end of file,");
	cout << "error_count = " << error_count
		 << "\n marked_error_count = " << marked_error_count
		 << "\n lines = " << line_number
		 << "\n unnested_searches = " << CxxSearchContext::unnested_searches
		 << "\n nested_searches = " << CxxSearchContext::nested_searches
		 << "\n releases = " << CxxSearchContext::releases
		 << "\n actual_searches = " << CxxSearchContext::actual_searches
		 << "\n max_search_depth = " << CxxSearchContext::max_search_depth
		 << "\n search_advances = " << CxxSearchContext::search_advances << endl;
	cout << "\n number of occurences of each advance"; 
	for (size_t i = 0; i < sizeof(CxxSearchContext::advances)/sizeof(CxxSearchContext::advances[0]); i++)
		cout << ' ' << CxxSearchContext::advances[i];
	cout << endl;	
	return 0;
}

static CxxToken **tokenBuffer = 0;				// Allocated buffer
static YACC_MARK_TYPE tokenReadIndex = 0;		// Read index
static size_t tokenSize = 0;					// Allocate buffer size
static YACC_MARK_TYPE tokenWriteIndex = 0;		// Write index
int tokenMarkDepth = 0;			        		// Write index
static CxxToken *primed_tokens[3] = {0, 0};		// Restarting sequence
static void token_put(CxxToken *aToken);

CxxSearchContext *CxxSearchContext::_current = 0;
CxxSearchContext *CxxSearchContext::_free = 0;
size_t CxxSearchContext::actual_searches = 0;
size_t CxxSearchContext::advances[16] = { 0 };
size_t CxxSearchContext::max_search_depth = 0;
size_t CxxSearchContext::nested_searches = 0;
size_t CxxSearchContext::releases = 0;
size_t CxxSearchContext::search_advances;
size_t CxxSearchContext::unnested_searches;

//
//	Implements a binary search counter, performing the increment at the
//	_index of othe failed search.
//
bool CxxSearchContext::advance()
{
	_advances++;
	size_t i = _depth;
	if (i <= 0)
		return false;
	while (--i > _index)
		_status[i] = false;
	while (true)
	{
		if (!_status[i])
		{
			_status[i] = true;
			_index = 0;
			return true;
		}
		if (i <= 0)
			return false;
		_status[i--] = false;
	}
}

void CxxSearchContext::initialise(size_t markIndex, bool enableType1)
{
	_index = 0;
	_depth = 0;
	_mark = markIndex;
    _enable_type1 = enableType1;
	_line = line_number;
	_advances = 0;
}

bool CxxSearchContext::is_template()
{
	if (_index >= _depth)
	{
		if (_depth >= _size)
		{
			ERRMSG("Binary search depth exceeded.");
			return false;
		}
		_status[_depth++] = false;
		if (_depth > max_search_depth)
			max_search_depth = _depth;
	}
	return _status[_index++] ? false : true;
}

//
//	Put this element onto listHead, returning element under this one.
//
CxxSearchContext *CxxSearchContext::queue(CxxSearchContext *& listHead)
{
	CxxSearchContext *oldNext = _next;
	_next = listHead;
	listHead = this;
	return oldNext;
}

//
//	Release the current search buffer.
//
void CxxSearchContext::release()
{
	if (_current)
	{
		releases++;
		_current->reset();
		_current = _current->queue(_free);
	}
}

void CxxSearchContext::reset()
{
	if (did_search())
	{
		_advances++;
		actual_searches++;
	}
	if (_advances >= sizeof(advances)/sizeof(advances[0]))
		advances[sizeof(advances)/sizeof(advances[0])-1]++;
	else
		advances[_advances]++;
}

void CxxSearchContext::start(YACC_MARK_TYPE anIndex, bool enableType1)
{
	if (!_current)
		unnested_searches++;
	else
		nested_searches++;
	if (!_free)
		_current = new CxxSearchContext(_current);
	else
		_free = _free->queue(_current);
	_current->initialise(anIndex, enableType1);
}

static CxxToken angleToken('<');
static CxxToken colonToken(':');
static CxxToken hashToken('#');
static CxxToken plusToken('+');
static CxxToken minusToken('-');

void PARSE_SCOPE yyerror(const char *s)
{
	if (!bang_depth && (tokenMarkDepth == 0))
	{
		cout << s << endl;
		increment_error_count();
	}
	else
	{
		if (show_marked)
			cout << "Marked " << s << endl;		
		marked_error_count++;
	}
}

extern YYSTYPE yylval;
//
//	Get the next token for the parser, invoking yylex_token to get the next token from the lexer.
//	This routine gets renamed to buffered_yylex by a #define when using yacc so that the two purposes
//	above are split allowing lookahead buffering and primimimg to occur.
//
int PARSE_SCOPE yylex()
{
	CxxToken *aToken = primed_tokens[0];
	if (aToken)
	{
		primed_tokens[0] = primed_tokens[1];
		primed_tokens[1] = primed_tokens[2];
		primed_tokens[2] = 0;
	}
	else if (tokenReadIndex < tokenWriteIndex)
		aToken = tokenBuffer[tokenReadIndex++];
	else
	{
		aToken = yylex_token();
		if (!aToken)
			return 0;
		if (tokenMarkDepth > 0)
			token_put(aToken);
		else
		{
			tokenWriteIndex = 0;
			tokenReadIndex = 0;
		}
	}
	yylval.token = aToken;
	return aToken->value();
}

//
//	Advance the binary search of template attempts. Rewinds and forces true into the input sequence
//	to proceed with the search. Rewinds and forces false to terminate it. Also forces a # that may then
//	be used to initiate error propagation.
//
void advance_search()
{
	CxxSearchContext::search_advances++;
	remark(CxxSearchContext::current()->mark());
	if (CxxSearchContext::current() && CxxSearchContext::current()->advance())
	{
		primed_tokens[0] = &plusToken;
		primed_tokens[1] = 0;
	}
	else
	{
		primed_tokens[0] = &minusToken;
		primed_tokens[1] = &hashToken;
	}
}

//
//	Complete a search, releasing the search context object and popping a mark off the stack.
//
void end_search(CxxToken *aToken)
{
	CxxSearchContext::release();
	unmark(aToken);
}

//
//	Notch up an error and establish a good break point.
//
void increment_error_count()
{
	error_count++;
}

//
//	Push a new marked context onto the stack, returning its identity for use by remark().
//	Any parser readahead is incorporated within the marked region.
//
YACC_MARK_TYPE mark()
{
	if (primed_tokens[0])
		ERRMSG("Unexpected primed_tokens[0] in mark.");
	YACC_MARK_TYPE markIndex = tokenReadIndex;
	if (PARSE_DOT yychar != YYEMPTY)
	{
//		if (primed_tokens[2])
//			token_put(primed_tokens[2]);
//		if (primed_tokens[1])
//			token_put(primed_tokens[1]);
//		if (primed_tokens[0])
//			token_put(primed_tokens[0]);
//		if (!tokenMarkDepth)
		if (!tokenReadIndex && !tokenWriteIndex)
		{
			token_put(PARSE_DOT yylval.token);
			tokenReadIndex = 0;
		}
		else if (!tokenReadIndex)
			ERRMSG("Unexpected 0 read index in mark.");
		else if (tokenBuffer[--tokenReadIndex] != PARSE_DOT yylval.token)
			ERRMSG("Unexpected unget in mark.");
		markIndex = tokenReadIndex;
		yyclearin;
		primed_tokens[0] = 0;
		primed_tokens[1] = 0;
	}
	tokenMarkDepth++; 
	bang_depth++;
	return markIndex;
}

//
//	If it is appropriate to do type I function parameter parsing perform a mark and force a rrue token
//	into the input stream. Otherwise just force a false token in.
//
YACC_MARK_TYPE mark_type1()
{
	if (!in_type1 && CxxSearchContext::current() && CxxSearchContext::current()->enable_type1())
	{
		YACC_MARK_TYPE markIndex = mark();
		primed_tokens[0] = &plusToken;
		primed_tokens[1] = 0;
		in_type1 = true;
        yyclearin; 
		return markIndex;
	}
	else
	{
		primed_tokens[0] = &minusToken;
		primed_tokens[1] = PARSE_DOT yychar != YYEMPTY ? PARSE_DOT yylval.token : 0;
        yyclearin; 
		return 0;			// Never used.
	}
}	

//
//	Push a bang context onto the error suppression stack, returning the context for restoration by pop_bang.
//
void pop_bang(YACC_BANG_TYPE bangValue)
{
	bang_depth = bangValue;
}

//
//	Push a bang context onto the error suppression stack, returning the context for restoration by pop_bang.
//
YACC_BANG_TYPE push_bang()
{
	return bang_depth++;
}

//
//	Reposition the input to restart at the position returned by a mark().
//
void remark(YACC_MARK_TYPE anIndex)
{
	tokenReadIndex = anIndex;
    yyclearin;
}

//
//	Reposition the input to restart at the position returned by a mark().
//
void remark_type1(YACC_MARK_TYPE anIndex)
{
	remark(anIndex);
	in_type1 = false;
}

//
//	Rewind the input stream back to anIndex and force a : prior to resuming input.
//
void rewind_colon(YACC_MARK_TYPE anIndex, const CxxToken *aToken)
{
	remark(anIndex);
	unmark();
	primed_tokens[0] = &colonToken;
	primed_tokens[1] = PARSE_DOT yylval.token;
}

//
//	Start a new binary search over the template/arithmetic alternative parses of a statement.
//	Marks the current position and saves it in a binary search context maintained on a private stack.
//
void start_search(bool enableType1)
{
	bool type1Enabled = !CxxSearchContext::current() || CxxSearchContext::current()->enable_type1() ? true : false;
	CxxSearchContext::start(mark(), enableType1 && type1Enabled ? true : false);
}

//
//	Determine whether the just parsed < should be interpreted as a template or arithmetic operator.
//	The implementation here intersacts with a binary search to traverse all possibilities in
//	multiple passes. The search proceeds by branch and bound presuming the template interpretation.
//	A true token is forced into the input stream to take the template interpretaion. A false token
//	otherwise.
//
//	An alternate implementation that keeps track of scopes may interact with semantic knowledge to make
//	the correct decision directly.
//
void template_test()
{
	if (!CxxSearchContext::current() || CxxSearchContext::current()->is_template())
	{
		primed_tokens[0] = &plusToken;
		primed_tokens[1] = 0;
	}
	else
	{
		primed_tokens[0] = &minusToken;
		primed_tokens[1] = &angleToken;
	}
}

void token_put(CxxToken *aToken)
{
	if (!tokenBuffer || !tokenSize)
		tokenBuffer = new CxxToken *[tokenSize = 256];
	else if (tokenWriteIndex >= tokenSize)
	{
		CxxToken **oldTokenBuffer = tokenBuffer;
		size_t oldTokenSize = tokenSize;
		tokenBuffer = new CxxToken *[tokenSize *= 2];
		memcpy(tokenBuffer, oldTokenBuffer, oldTokenSize * sizeof(*oldTokenBuffer));
		delete[] oldTokenBuffer;
	}
	tokenBuffer[tokenWriteIndex++] = aToken;
	tokenReadIndex = tokenWriteIndex;
}

//
//	Pop a marked context off the stack.
//
void unmark(const CxxToken *aToken)
{
    if (bang_depth)
        bang_depth--;
    else
        ERRMSG("BUG - should not unmark with 0 bang.");
	if (tokenMarkDepth <= 0)
		ERRMSG("Unexpected unmark.");
	else
		tokenMarkDepth--;
}
