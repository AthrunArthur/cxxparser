#
FILES= CxxLexer.cpp CxxParser.cpp CxxToken.cpp
INCLUDE = $(PWD)

CppParser : CxxLexer.l CxxLexer.cpp CxxLexing.cxx CxxLexing.hxx CxxParser.y CxxToken.cpp CxxToken.cxx CxxToken.hxx
	bison -d -v CxxParser.y
	flex CxxLexer.l
	g++  -I$(INCLUDE) $(FILES) -ly -lfl -o cppparser