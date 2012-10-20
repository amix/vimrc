/*
*   $Id: vhdl.c 652 2008-04-18 03:51:47Z elliotth $
* 
*   Copyright (c) 2008, Nicolas Vincent
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for VHDL files.
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include <ctype.h>	/* to define isalpha () */
#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *   MACROS
 */
#define isType(token,t)     (boolean) ((token)->type == (t))
#define isKeyword(token,k)  (boolean) ((token)->keyword == (k))

/*
 *   DATA DECLARATIONS
 */
typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*
 * Used to specify type of keyword.
 */
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_ABS,
	KEYWORD_ACCESS,
	KEYWORD_AFTER,
	KEYWORD_ALIAS,
	KEYWORD_ALL,
	KEYWORD_AND,
	KEYWORD_ARCHITECTURE,
	KEYWORD_ARRAY,
	KEYWORD_ASSERT,
	KEYWORD_ATTRIBUTE,
	KEYWORD_BEGIN,
	KEYWORD_BLOCK,
	KEYWORD_BODY,
	KEYWORD_BUFFER,
	KEYWORD_BUS,
	KEYWORD_CASE,
	KEYWORD_COMPONENT,
	KEYWORD_CONFIGURATION,
	KEYWORD_CONSTANT,
	KEYWORD_DISCONNECT,
	KEYWORD_DOWNTO,
	KEYWORD_ELSE,
	KEYWORD_ELSIF,
	KEYWORD_END,
	KEYWORD_ENTITY,
	KEYWORD_EXIT,
	KEYWORD_FILE,
	KEYWORD_FOR,
	KEYWORD_FUNCTION,
	KEYWORD_GENERATE,
	KEYWORD_GENERIC,
	KEYWORD_GROUP,
	KEYWORD_GUARDED,
	KEYWORD_IF,
	KEYWORD_IMPURE,
	KEYWORD_IN,
	KEYWORD_INERTIAL,
	KEYWORD_INOUT,
	KEYWORD_IS,
	KEYWORD_LABEL,
	KEYWORD_LIBRARY,
	KEYWORD_LINKAGE,
	KEYWORD_LITERAL,
	KEYWORD_LOOP,
	KEYWORD_MAP,
	KEYWORD_MOD,
	KEYWORD_NAND,
	KEYWORD_NEW,
	KEYWORD_NEXT,
	KEYWORD_NOR,
	KEYWORD_NOT,
	KEYWORD_NULL,
	KEYWORD_OF,
	KEYWORD_ON,
	KEYWORD_OPEN,
	KEYWORD_OR,
	KEYWORD_OTHERS,
	KEYWORD_OUT,
	KEYWORD_PACKAGE,
	KEYWORD_PORT,
	KEYWORD_POSTPONED,
	KEYWORD_PROCEDURE,
	KEYWORD_PROCESS,
	KEYWORD_PURE,
	KEYWORD_RANGE,
	KEYWORD_RECORD,
	KEYWORD_REGISTER,
	KEYWORD_REJECT,
	KEYWORD_RETURN,
	KEYWORD_ROL,
	KEYWORD_ROR,
	KEYWORD_SELECT,
	KEYWORD_SEVERITY,
	KEYWORD_SIGNAL,
	KEYWORD_SHARED,
	KEYWORD_SLA,
	KEYWORD_SLI,
	KEYWORD_SRA,
	KEYWORD_SRL,
	KEYWORD_SUBTYPE,
	KEYWORD_THEN,
	KEYWORD_TO,
	KEYWORD_TRANSPORT,
	KEYWORD_TYPE,
	KEYWORD_UNAFFECTED,
	KEYWORD_UNITS,
	KEYWORD_UNTIL,
	KEYWORD_USE,
	KEYWORD_VARIABLE,
	KEYWORD_WAIT,
	KEYWORD_WHEN,
	KEYWORD_WHILE,
	KEYWORD_WITH,
	KEYWORD_XNOR,
	KEYWORD_XOR
} keywordId;

/*  Used to determine whether keyword is valid for the current language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
} keywordDesc;

typedef enum eTokenType {
	TOKEN_NONE,		/* none */
	TOKEN_OPEN_PAREN,	/* ( */
	TOKEN_CLOSE_PAREN,	/* ) */
	TOKEN_COMMA,		/* the comma character */
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_PERIOD,		/* . */
	TOKEN_OPERATOR,
	TOKEN_SEMICOLON,	/* the semicolon character */
	TOKEN_STRING
} tokenType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	vString *string;		/* the name of the token */
	vString *scope;
	unsigned long lineNumber;	/* line number of tag */
	fpos_t filePosition;		/* file position of line containing name */
} tokenInfo;

/*
 *   DATA DEFINITIONS
 */
static int Lang_vhdl;
static jmp_buf Exception;

/* Used to index into the VhdlKinds table. */
typedef enum {
	VHDLTAG_UNDEFINED = -1,
	VHDLTAG_CONSTANT,
	VHDLTAG_TYPE,
	VHDLTAG_SUBTYPE,
	VHDLTAG_RECORD,
	VHDLTAG_ENTITY,
	VHDLTAG_COMPONENT,
	VHDLTAG_PROTOTYPE,
	VHDLTAG_FUNCTION,
	VHDLTAG_PROCEDURE,
	VHDLTAG_PACKAGE,
	VHDLTAG_LOCAL
} vhdlKind;

static kindOption VhdlKinds[] = {
	{TRUE, 'c', "constant", "constant declarations"},
	{TRUE, 't', "type", "type definitions"},
	{TRUE, 'T', "subtype", "subtype definitions"},
	{TRUE, 'r', "record", "record names"},
	{TRUE, 'e', "entity", "entity declarations"},
	{FALSE, 'C', "component", "component declarations"},
	{FALSE, 'd', "prototype", "prototypes"},
	{TRUE, 'f', "function", "function prototypes and declarations"},
	{TRUE, 'p', "procedure", "procedure prototypes and declarations"},
	{TRUE, 'P', "package", "package definitions"},
	{FALSE, 'l', "local", "local definitions"}
};

static keywordDesc VhdlKeywordTable[] = {
	{"abs", KEYWORD_ABS},
	{"access", KEYWORD_ACCESS},
	{"after", KEYWORD_AFTER},
	{"alias", KEYWORD_ALIAS},
	{"all", KEYWORD_ALL},
	{"and", KEYWORD_AND},
	{"architecture", KEYWORD_ARCHITECTURE},
	{"array", KEYWORD_ARRAY},
	{"assert", KEYWORD_ASSERT},
	{"attribute", KEYWORD_ATTRIBUTE},
	{"begin", KEYWORD_BEGIN},
	{"block", KEYWORD_BLOCK},
	{"body", KEYWORD_BODY},
	{"buffer", KEYWORD_BUFFER},
	{"bus", KEYWORD_BUS},
	{"case", KEYWORD_CASE},
	{"component", KEYWORD_COMPONENT},
	{"configuration", KEYWORD_CONFIGURATION},
	{"constant", KEYWORD_CONSTANT},
	{"disconnect", KEYWORD_DISCONNECT},
	{"downto", KEYWORD_DOWNTO},
	{"else", KEYWORD_ELSE},
	{"elsif", KEYWORD_ELSIF},
	{"end", KEYWORD_END},
	{"entity", KEYWORD_ENTITY},
	{"exit", KEYWORD_EXIT},
	{"file", KEYWORD_FILE},
	{"for", KEYWORD_FOR},
	{"function", KEYWORD_FUNCTION},
	{"generate", KEYWORD_GENERATE},
	{"generic", KEYWORD_GENERIC},
	{"group", KEYWORD_GROUP},
	{"guarded", KEYWORD_GUARDED},
	{"if", KEYWORD_IF},
	{"impure", KEYWORD_IMPURE},
	{"in", KEYWORD_IN},
	{"inertial", KEYWORD_INERTIAL},
	{"inout", KEYWORD_INOUT},
	{"is", KEYWORD_IS},
	{"label", KEYWORD_LABEL},
	{"library", KEYWORD_LIBRARY},
	{"linkage", KEYWORD_LINKAGE},
	{"literal", KEYWORD_LITERAL},
	{"loop", KEYWORD_LOOP},
	{"map", KEYWORD_MAP},
	{"mod", KEYWORD_MOD},
	{"nand", KEYWORD_NAND},
	{"new", KEYWORD_NEW},
	{"next", KEYWORD_NEXT},
	{"nor", KEYWORD_NOR},
	{"not", KEYWORD_NOT},
	{"null", KEYWORD_NULL},
	{"of", KEYWORD_OF},
	{"on", KEYWORD_ON},
	{"open", KEYWORD_OPEN},
	{"or", KEYWORD_OR},
	{"others", KEYWORD_OTHERS},
	{"out", KEYWORD_OUT},
	{"package", KEYWORD_PACKAGE},
	{"port", KEYWORD_PORT},
	{"postponed", KEYWORD_POSTPONED},
	{"procedure", KEYWORD_PROCEDURE},
	{"process", KEYWORD_PROCESS},
	{"pure", KEYWORD_PURE},
	{"range", KEYWORD_RANGE},
	{"record", KEYWORD_RECORD},
	{"register", KEYWORD_REGISTER},
	{"reject", KEYWORD_REJECT},
	{"return", KEYWORD_RETURN},
	{"rol", KEYWORD_ROL},
	{"ror", KEYWORD_ROR},
	{"select", KEYWORD_SELECT},
	{"severity", KEYWORD_SEVERITY},
	{"signal", KEYWORD_SIGNAL},
	{"shared", KEYWORD_SHARED},
	{"sla", KEYWORD_SLA},
	{"sli", KEYWORD_SLI},
	{"sra", KEYWORD_SRA},
	{"srl", KEYWORD_SRL},
	{"subtype", KEYWORD_SUBTYPE},
	{"then", KEYWORD_THEN},
	{"to", KEYWORD_TO},
	{"transport", KEYWORD_TRANSPORT},
	{"type", KEYWORD_TYPE},
	{"unaffected", KEYWORD_UNAFFECTED},
	{"units", KEYWORD_UNITS},
	{"until", KEYWORD_UNTIL},
	{"use", KEYWORD_USE},
	{"variable", KEYWORD_VARIABLE},
	{"wait", KEYWORD_WAIT},
	{"when", KEYWORD_WHEN},
	{"while", KEYWORD_WHILE},
	{"with", KEYWORD_WITH},
	{"xnor", KEYWORD_XNOR},
	{"xor", KEYWORD_XOR}
};

/*
 *   FUNCTION DECLARATIONS
 */
static void parseKeywords (tokenInfo * const token, boolean local);

/*
 *   FUNCTION DEFINITIONS
 */

static boolean isIdentChar1 (const int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentChar (const int c)
{
	return (boolean) (isalpha (c) || isdigit (c) || c == '_');
}

static boolean isIdentifierMatch (const tokenInfo * const token,
	const vString * const name)
{
	return (boolean) (isType (token, TOKEN_IDENTIFIER) &&
		strcasecmp (vStringValue (token->string), vStringValue (name)) == 0);
	/* XXX this is copy/paste from eiffel.c and slightly modified */
	/* shouldn't we use strNcasecmp ? */
}

static boolean isKeywordOrIdent (const tokenInfo * const token,
	const keywordId keyword, const vString * const name)
{
	return (boolean) (isKeyword (token, keyword) ||
		isIdentifierMatch (token, name));
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	token->string = vStringNew ();
	token->scope = vStringNew ();
	token->lineNumber = getSourceLineNumber ();
	token->filePosition = getInputFilePosition ();
	return token;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->string);
		vStringDelete (token->scope);
		eFree (token);
	}
}

/*
 *   Parsing functions
 */

static void parseString (vString * const string, const int delimiter)
{
	boolean end = FALSE;
	while (!end)
	{
		int c = fileGetc ();
		if (c == EOF)
			end = TRUE;
		else if (c == '\\')
		{
			c = fileGetc ();	/* This maybe a ' or ". */
			vStringPut (string, c);
		}
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

/*  Read a VHDL identifier beginning with "firstChar" and place it into "name".
*/
static void parseIdentifier (vString * const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar1 (c));
	do
	{
		vStringPut (string, c);
		c = fileGetc ();
	} while (isIdentChar (c));
	vStringTerminate (string);
	if (!isspace (c))
		fileUngetc (c);	/* unget non-identifier character */
}

static void readToken (tokenInfo * const token)
{
	int c;

	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

  getNextChar:
	do
	{
		c = fileGetc ();
		token->lineNumber = getSourceLineNumber ();
		token->filePosition = getInputFilePosition ();
	}
	while (c == '\t' || c == ' ' || c == '\n');

	switch (c)
	{
	case EOF:
		longjmp (Exception, (int) ExceptionEOF);
		break;
	case '(':
		token->type = TOKEN_OPEN_PAREN;
		break;
	case ')':
		token->type = TOKEN_CLOSE_PAREN;
		break;
	case ';':
		token->type = TOKEN_SEMICOLON;
		break;
	case '.':
		token->type = TOKEN_PERIOD;
		break;
	case ',':
		token->type = TOKEN_COMMA;
		break;
	case '\'':	/* only single char are inside simple quotes */
		break;	/* or it is for attributes so we don't care */
	case '"':
		token->type = TOKEN_STRING;
		parseString (token->string, c);
		token->lineNumber = getSourceLineNumber ();
		token->filePosition = getInputFilePosition ();
		break;
	case '-':
		c = fileGetc ();
		if (c == '-')	/* start of a comment */
		{
			fileSkipToCharacter ('\n');
			goto getNextChar;
		}
		else
		{
			if (!isspace (c))
				fileUngetc (c);
			token->type = TOKEN_OPERATOR;
		}
		break;
	default:
		if (!isIdentChar1 (c))
			token->type = TOKEN_NONE;
		else
		{
			parseIdentifier (token->string, c);
			token->lineNumber = getSourceLineNumber ();
			token->filePosition = getInputFilePosition ();
			token->keyword = analyzeToken (token->string, Lang_vhdl);
			if (isKeyword (token, KEYWORD_NONE))
				token->type = TOKEN_IDENTIFIER;
			else
				token->type = TOKEN_KEYWORD;
		}
		break;
	}
}

static void skipToKeyword (const keywordId keyword)
{
	tokenInfo *const token = newToken ();
	do
	{
		readToken (token);
	}
	while (!isKeyword (token, keyword));
	deleteToken (token);
}

static void skipToMatched (tokenInfo * const token)
{
	int nest_level = 0;
	tokenType open_token;
	tokenType close_token;

	switch (token->type)
	{
	case TOKEN_OPEN_PAREN:
		open_token = TOKEN_OPEN_PAREN;
		close_token = TOKEN_CLOSE_PAREN;
		break;
	default:
		return;
	}

	/*
	 * This routine will skip to a matching closing token.
	 * It will also handle nested tokens like the (, ) below.
	 *   (  name varchar(30), text binary(10)  )
	 */
	if (isType (token, open_token))
	{
		nest_level++;
		while (!(isType (token, close_token) && (nest_level == 0)))
		{
			readToken (token);
			if (isType (token, open_token))
			{
				nest_level++;
			}
			if (isType (token, close_token))
			{
				if (nest_level > 0)
				{
					nest_level--;
				}
			}
		}
		readToken (token);
	}
}

static void makeConstTag (tokenInfo * const token, const vhdlKind kind)
{
	if (VhdlKinds[kind].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);
		e.lineNumber = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName = VhdlKinds[kind].name;
		e.kind = VhdlKinds[kind].letter;
		makeTagEntry (&e);
	}
}

static void makeVhdlTag (tokenInfo * const token, const vhdlKind kind)
{
	if (VhdlKinds[kind].enabled)
	{
		/*
		 * If a scope has been added to the token, change the token
		 * string to include the scope when making the tag.
		 */
		if (vStringLength (token->scope) > 0)
		{
			vString *fulltag = vStringNew ();
			vStringCopy (fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue (token->string));
			vStringTerminate (fulltag);
			vStringCopy (token->string, fulltag);
			vStringDelete (fulltag);
		}
		makeConstTag (token, kind);
	}
}

static void initialize (const langType language)
{
	size_t i;
	const size_t count =
		sizeof (VhdlKeywordTable) / sizeof (VhdlKeywordTable[0]);
	Lang_vhdl = language;
	for (i = 0; i < count; ++i)
	{
		const keywordDesc *const p = &VhdlKeywordTable[i];
		addKeyword (p->name, language, (int) p->id);
	}
}

static void parsePackage (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	Assert (isKeyword (token, KEYWORD_PACKAGE));
	readToken (token);
	if (isKeyword (token, KEYWORD_BODY))
	{
		readToken (name);
		makeVhdlTag (name, VHDLTAG_PACKAGE);
	}
	else if (isType (token, TOKEN_IDENTIFIER))
	{
		makeVhdlTag (token, VHDLTAG_PACKAGE);
	}
	deleteToken (name);
}

static void parseModule (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	const vhdlKind kind = isKeyword (token, KEYWORD_ENTITY) ?
		VHDLTAG_ENTITY : VHDLTAG_COMPONENT;
	Assert (isKeyword (token, KEYWORD_ENTITY) ||
		isKeyword (token, KEYWORD_COMPONENT));
	readToken (name);
	if (kind == VHDLTAG_COMPONENT)
	{
		makeVhdlTag (name, VHDLTAG_COMPONENT);
		skipToKeyword (KEYWORD_END);
		fileSkipToCharacter (';');
	}
	else
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_IS))
		{
			makeVhdlTag (name, VHDLTAG_ENTITY);
			skipToKeyword (KEYWORD_END);
			fileSkipToCharacter (';');
		}
	}
	deleteToken (name);
}

static void parseRecord (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	Assert (isKeyword (token, KEYWORD_RECORD));
	readToken (name);
	do
	{
		readToken (token);	/* should be a colon */
		fileSkipToCharacter (';');
		makeVhdlTag (name, VHDLTAG_RECORD);
		readToken (name);
	}
	while (!isKeyword (name, KEYWORD_END));
	fileSkipToCharacter (';');
	deleteToken (name);
}

static void parseTypes (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	const vhdlKind kind = isKeyword (token, KEYWORD_TYPE) ?
		VHDLTAG_TYPE : VHDLTAG_SUBTYPE;
	Assert (isKeyword (token, KEYWORD_TYPE) ||
		isKeyword (token, KEYWORD_SUBTYPE));
	readToken (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_IS))
	{
		readToken (token);	/* type */
		if (isKeyword (token, KEYWORD_RECORD))
		{
			makeVhdlTag (name, kind);
			/*TODO: make tags of the record's names */
			parseRecord (token);
		}
		else
		{
			makeVhdlTag (name, kind);
		}
	}
	deleteToken (name);
}

static void parseConstant (boolean local)
{
	tokenInfo *const name = newToken ();
	readToken (name);
	if (local)
	{
		makeVhdlTag (name, VHDLTAG_LOCAL);
	}
	else
	{
		makeVhdlTag (name, VHDLTAG_CONSTANT);
	}
	fileSkipToCharacter (';');
	deleteToken (name);
}

static void parseSubProgram (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	boolean endSubProgram = FALSE;
	const vhdlKind kind = isKeyword (token, KEYWORD_FUNCTION) ?
		VHDLTAG_FUNCTION : VHDLTAG_PROCEDURE;
	Assert (isKeyword (token, KEYWORD_FUNCTION) ||
		isKeyword (token, KEYWORD_PROCEDURE));
	readToken (name);	/* the name of the function or procedure */
	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipToMatched (token);
	}

	if (kind == VHDLTAG_FUNCTION)
	{
		if (isKeyword (token, KEYWORD_RETURN))
		{
			/* Read datatype */
			readToken (token);
			while (! isKeyword (token, KEYWORD_IS) &&
					! isType (token, TOKEN_SEMICOLON))
			{
				readToken (token);
			}
		}
	}

	if (isType (token, TOKEN_SEMICOLON))
	{
		makeVhdlTag (name, VHDLTAG_PROTOTYPE);
	}
	else if (isKeyword (token, KEYWORD_IS))
	{
		if (kind == VHDLTAG_FUNCTION)
		{
			makeVhdlTag (name, VHDLTAG_FUNCTION);
			do
			{
				readToken (token);
				if (isKeyword (token, KEYWORD_END))
				{
					readToken (token);
					endSubProgram = isKeywordOrIdent (token,
						KEYWORD_FUNCTION, name->string);
					fileSkipToCharacter (';');
				}
				else
				{
					parseKeywords (token, TRUE);
				}
			} while (!endSubProgram);
		}
		else
		{
			makeVhdlTag (name, VHDLTAG_PROCEDURE);
			do
			{
				readToken (token);
				if (isKeyword (token, KEYWORD_END))
				{
					readToken (token);
					endSubProgram = isKeywordOrIdent (token,
						KEYWORD_PROCEDURE, name->string);
					fileSkipToCharacter (';');
				}
				else
				{
					parseKeywords (token, TRUE);
				}
			} while (!endSubProgram);
		}
	}
	deleteToken (name);
}

/* TODO */
/* records */
static void parseKeywords (tokenInfo * const token, boolean local)
{
	switch (token->keyword)
	{
	case KEYWORD_END:
		fileSkipToCharacter (';');
		break;
	case KEYWORD_CONSTANT:
		parseConstant (local);
		break;
	case KEYWORD_TYPE:
		parseTypes (token);
		break;
	case KEYWORD_SUBTYPE:
		parseTypes (token);
		break;
	case KEYWORD_ENTITY:
		parseModule (token);
		break;
	case KEYWORD_COMPONENT:
		parseModule (token);
		break;
	case KEYWORD_FUNCTION:
		parseSubProgram (token);
		break;
	case KEYWORD_PROCEDURE:
		parseSubProgram (token);
		break;
	case KEYWORD_PACKAGE:
		parsePackage (token);
		break;
	default:
		break;
	}
}

static void parseVhdlFile (tokenInfo * const token)
{
	do
	{
		readToken (token);
		parseKeywords (token, FALSE);
	} while (!isKeyword (token, KEYWORD_END));
}

static void findVhdlTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception = (exception_t) (setjmp (Exception));

	while (exception == ExceptionNone)
		parseVhdlFile (token);

	deleteToken (token);
}

extern parserDefinition *VhdlParser (void)
{
	static const char *const extensions[] = { "vhdl", "vhd", NULL };
	parserDefinition *def = parserNew ("VHDL");
	def->kinds = VhdlKinds;
	def->kindCount = KIND_COUNT (VhdlKinds);
	def->extensions = extensions;
	def->parser = findVhdlTags;
	def->initialize = initialize;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noet: */
