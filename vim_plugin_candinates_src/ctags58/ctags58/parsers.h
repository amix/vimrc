/*
*   $Id: parsers.h 717 2009-07-07 03:40:50Z dhiebert $
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to all language parsing modules.
*
*   To add a new language parser, you need only modify this single source
*   file to add the name of the parser definition function.
*/
#ifndef _PARSERS_H
#define _PARSERS_H

/* Add the name of any new parser definition function here */
#define PARSER_LIST \
	AntParser, \
	AsmParser, \
	AspParser, \
	AwkParser, \
	BasicParser, \
	BetaParser, \
	CParser, \
	CppParser, \
	CsharpParser, \
	CobolParser, \
	DosBatchParser, \
	EiffelParser, \
	ErlangParser, \
	FlexParser, \
	FortranParser, \
	HtmlParser, \
	JavaParser, \
	JavaScriptParser, \
	LispParser, \
	LuaParser, \
	MakefileParser, \
	MatLabParser, \
	OcamlParser, \
	PascalParser, \
	PerlParser, \
	PhpParser, \
	PythonParser, \
	RexxParser, \
	RubyParser, \
	SchemeParser, \
	ShParser, \
	SlangParser, \
	SmlParser, \
	SqlParser, \
	TclParser, \
	TexParser, \
	VeraParser, \
	VerilogParser, \
	VhdlParser, \
	VimParser, \
	YaccParser

#endif  /* _PARSERS_H */

/* vi:set tabstop=4 shiftwidth=4: */
