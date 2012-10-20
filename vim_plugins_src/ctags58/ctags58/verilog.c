/*
*   $Id: verilog.c 573 2007-06-26 05:41:27Z elliotth $
* 
*   Copyright (c) 2003, Darren Hiebert
* 
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
* 
*   This module contains functions for generating tags for the Verilog HDL
*   (Hardware Description Language).
* 
*   Language definition documents:
*       http://www.eg.bucknell.edu/~cs320/verilog/verilog-manual.html
*       http://www.sutherland-hdl.com/on-line_ref_guide/vlog_ref_top.html
*       http://www.verilog.com/VerilogBNF.html
*       http://eesun.free.fr/DOC/VERILOG/verilog_manual1.html
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "get.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
 *   DATA DECLARATIONS
 */
typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

typedef enum {
	K_UNDEFINED = -1,
	K_CONSTANT,
	K_EVENT,
	K_FUNCTION,
	K_MODULE,
	K_NET,
	K_PORT,
	K_REGISTER,
	K_TASK
} verilogKind;

typedef struct {
	const char *keyword;
	verilogKind kind;
} keywordAssoc;

/*
 *   DATA DEFINITIONS
 */
static int Ungetc;
static int Lang_verilog;
static jmp_buf Exception;

static kindOption VerilogKinds [] = {
 { TRUE, 'c', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" }
};

static keywordAssoc VerilogKeywordTable [] = {
	{ "`define",   K_CONSTANT },
	{ "event",     K_EVENT },
	{ "function",  K_FUNCTION },
	{ "inout",     K_PORT },
	{ "input",     K_PORT },
	{ "integer",   K_REGISTER },
	{ "module",    K_MODULE },
	{ "output",    K_PORT },
	{ "parameter", K_CONSTANT },
	{ "real",      K_REGISTER },
	{ "realtime",  K_REGISTER },
	{ "reg",       K_REGISTER },
	{ "specparam", K_CONSTANT },
	{ "supply0",   K_NET },
	{ "supply1",   K_NET },
	{ "task",      K_TASK },
	{ "time",      K_REGISTER },
	{ "tri0",      K_NET },
	{ "tri1",      K_NET },
	{ "triand",    K_NET },
	{ "tri",       K_NET },
	{ "trior",     K_NET },
	{ "trireg",    K_NET },
	{ "wand",      K_NET },
	{ "wire",      K_NET },
	{ "wor",       K_NET }
};

/*
 *   FUNCTION DEFINITIONS
 */

static void initialize (const langType language)
{
	size_t i;
	const size_t count = 
			sizeof (VerilogKeywordTable) / sizeof (VerilogKeywordTable [0]);
	Lang_verilog = language;
	for (i = 0  ;  i < count  ;  ++i)
	{
		const keywordAssoc* const p = &VerilogKeywordTable [i];
		addKeyword (p->keyword, language, (int) p->kind);
	}
}

static void vUngetc (int c)
{
	Assert (Ungetc == '\0');
	Ungetc = c;
}

static int vGetc (void)
{
	int c;
	if (Ungetc == '\0')
		c = fileGetc ();
	else
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	if (c == '/')
	{
		int c2 = fileGetc ();
		if (c2 == EOF)
			longjmp (Exception, (int) ExceptionEOF);
		else if (c2 == '/')  /* strip comment until end-of-line */
		{
			do
				c = fileGetc ();
			while (c != '\n'  &&  c != EOF);
		}
		else if (c2 == '*')  /* strip block comment */
		{
			c = skipOverCComment();
		}
		else
		{
			fileUngetc (c2);
		}
	}
	else if (c == '"')  /* strip string contents */
	{
		int c2;
		do
			c2 = fileGetc ();
		while (c2 != '"'  &&  c2 != EOF);
		c = '@';
	}
	if (c == EOF)
		longjmp (Exception, (int) ExceptionEOF);
	return c;
}

static boolean isIdentifierCharacter (const int c)
{
	return (boolean)(isalnum (c)  ||  c == '_'  ||  c == '`');
}

static int skipWhite (int c)
{
	while (isspace (c))
		c = vGetc ();
	return c;
}

static int skipPastMatch (const char *const pair)
{
	const int begin = pair [0], end = pair [1];
	int matchLevel = 1;
	int c;
	do
	{
		c = vGetc ();
		if (c == begin)
			++matchLevel;
		else if (c == end)
			--matchLevel;
	}
	while (matchLevel > 0);
	return vGetc ();
}

static boolean readIdentifier (vString *const name, int c)
{
	vStringClear (name);
	if (isIdentifierCharacter (c))
	{
		while (isIdentifierCharacter (c))
		{
			vStringPut (name, c);
			c = vGetc ();
		}
		vUngetc (c);
		vStringTerminate (name);
	}
	return (boolean)(name->length > 0);
}

static void tagNameList (const verilogKind kind, int c)
{
	vString *name = vStringNew ();
	boolean repeat;
	Assert (isIdentifierCharacter (c));
	do
	{ 
		repeat = FALSE;
		if (isIdentifierCharacter (c))
		{
			readIdentifier (name, c);
			makeSimpleTag (name, VerilogKinds, kind);
		}
		else
			break;
		c = skipWhite (vGetc ());
		if (c == '[')
			c = skipPastMatch ("[]");
		c = skipWhite (c);
		if (c == '=')
		{
			if (c == '{')
				skipPastMatch ("{}");
			else
			{
				do
					c = vGetc ();
				while (c != ','  &&  c != ';');
			}
		}
		if (c == ',')
		{
			c = skipWhite (vGetc ());
			repeat = TRUE;
		}
		else
			repeat = FALSE;
	} while (repeat);
	vStringDelete (name);
	vUngetc (c);
}

static void findTag (vString *const name)
{
	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
	if (kind == K_CONSTANT && vStringItem (name, 0) == '`')
	{
		/* Bug #961001: Verilog compiler directives are line-based. */
		int c = skipWhite (vGetc ());
		readIdentifier (name, c);
		makeSimpleTag (name, VerilogKinds, kind);
		/* Skip the rest of the line. */
		do {
			c = vGetc();
		} while (c != '\n');
		vUngetc (c);
	}
	else if (kind != K_UNDEFINED)
	{
		int c = skipWhite (vGetc ());

		/* Many keywords can have bit width.
		*   reg [3:0] net_name;
		*   inout [(`DBUSWIDTH-1):0] databus;
		*/
		if (c == '(')
			c = skipPastMatch ("()");
		c = skipWhite (c);
		if (c == '[')
			c = skipPastMatch ("[]");
		c = skipWhite (c);
		if (c == '#')
		{
			c = vGetc ();
			if (c == '(')
				c = skipPastMatch ("()");
		}
		c = skipWhite (c);
		if (isIdentifierCharacter (c))
			tagNameList (kind, c);
	}
}

static void findVerilogTags (void)
{
	vString *const name = vStringNew ();
	volatile boolean newStatement = TRUE;
	volatile int c = '\0';
	exception_t exception = (exception_t) setjmp (Exception);

	if (exception == ExceptionNone) while (c != EOF)
	{
		c = vGetc ();
		switch (c)
		{
			case ';':
			case '\n':
				newStatement = TRUE;
				break;

			case ' ':
			case '\t':
				break;

			default:
				if (newStatement && readIdentifier (name, c))
					findTag (name);
				newStatement = FALSE;
				break;
		}
	}
	vStringDelete (name);
}

extern parserDefinition* VerilogParser (void)
{
	static const char *const extensions [] = { "v", NULL };
	parserDefinition* def = parserNew ("Verilog");
	def->kinds      = VerilogKinds;
	def->kindCount  = KIND_COUNT (VerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initialize;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
