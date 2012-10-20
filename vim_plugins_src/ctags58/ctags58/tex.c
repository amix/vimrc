/*
 *	 $Id: tex.c 666 2008-05-15 17:47:31Z dfishburn $
 *
 *	 Copyright (c) 2008, David Fishburn
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License.
 *
 *	 This module contains functions for generating tags for TeX language files.
 *
 *	 Tex language reference:
 *		 http://en.wikibooks.org/wiki/TeX#The_Structure_of_TeX
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#include <setjmp.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(boolean) ((token)->type == (t))
#define isKeyword(token,k)	(boolean) ((token)->keyword == (k))

/*
 *	 DATA DECLARATIONS
 */

typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*
 * Used to specify type of keyword.
 */
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_chapter,
	KEYWORD_section,
	KEYWORD_subsection,
	KEYWORD_subsubsection,
	KEYWORD_part,
	KEYWORD_paragraph,
	KEYWORD_subparagraph
} keywordId;

/*	Used to determine whether keyword is valid for the token language and
 *	what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
} keywordDesc;

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_EXCLAMATION,
	TOKEN_FORWARD_SLASH,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_OPEN_MXML,
	TOKEN_CLOSE_MXML,
	TOKEN_CLOSE_SGML,
	TOKEN_LESS_THAN,
	TOKEN_GREATER_THAN,
	TOKEN_QUESTION_MARK,
	TOKEN_STAR
} tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long 	lineNumber;
	fpos_t 			filePosition;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_js;

static jmp_buf Exception;

typedef enum {
	TEXTAG_CHAPTER,
	TEXTAG_SECTION,
	TEXTAG_SUBSECTION,
	TEXTAG_SUBSUBSECTION,
	TEXTAG_PART,
	TEXTAG_PARAGRAPH,
	TEXTAG_SUBPARAGRAPH,
	TEXTAG_COUNT
} texKind;

static kindOption TexKinds [] = {
	{ TRUE,  'c', "chapter",		  "chapters"		   },
	{ TRUE,  's', "section",		  "sections"		   },
	{ TRUE,  'u', "subsection",		  "subsections"		   },
	{ TRUE,  'b', "subsubsection",	  "subsubsections"	   },
	{ TRUE,  'p', "part",			  "parts"			   },
	{ TRUE,  'P', "paragraph",		  "paragraphs"		   },
	{ TRUE,  'G', "subparagraph",	  "subparagraphs"	   }
};

static const keywordDesc TexKeywordTable [] = {
	/* keyword			keyword ID */
	{ "chapter",		KEYWORD_chapter				},
	{ "section",		KEYWORD_section				},
	{ "subsection",		KEYWORD_subsection			},
	{ "subsubsection",	KEYWORD_subsubsection		},
	{ "part",			KEYWORD_part				},
	{ "paragraph",		KEYWORD_paragraph			},
	{ "subparagraph",	KEYWORD_subparagraph		}
};

/*
 *	 FUNCTION DEFINITIONS
 */

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) || isdigit (c) || c == '$' || 
		  c == '_' || c == '#');
}

static void buildTexKeywordHash (void)
{
	const size_t count = sizeof (TexKeywordTable) /
		sizeof (TexKeywordTable [0]);
	size_t i;
	for (i = 0	;  i < count  ;  ++i)
	{
		const keywordDesc* const p = &TexKeywordTable [i];
		addKeyword (p->name, Lang_js, (int) p->id);
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();
	token->lineNumber   = getSourceLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

/*
 *	 Tag generation functions
 */

static void makeConstTag (tokenInfo *const token, const texKind kind)
{
	if (TexKinds [kind].enabled )
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName	   = TexKinds [kind].name;
		e.kind		   = TexKinds [kind].letter;

		makeTagEntry (&e);
	}
}

static void makeTexTag (tokenInfo *const token, texKind kind)
{
	vString *	fulltag;

	if (TexKinds [kind].enabled)
	{
		/*
		 * If a scope has been added to the token, change the token
		 * string to include the scope when making the tag.
		 */
		if ( vStringLength (token->scope) > 0 )
		{
			fulltag = vStringNew ();
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

/*
 *	 Parsing functions
 */

static void parseString (vString *const string, const int delimiter)
{
	boolean end = FALSE;
	while (! end)
	{
		int c = fileGetc ();
		if (c == EOF)
			end = TRUE;
		else if (c == '\\')
		{
			c = fileGetc(); /* This maybe a ' or ". */
			vStringPut (string, c);
		}
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

/*	
 *	Read a C identifier beginning with "firstChar" and places it into
 *	"name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar (c));
	do
	{
		vStringPut (string, c);
		c = fileGetc ();
	} while (isIdentChar (c));

	vStringTerminate (string);
	if (!isspace (c))
		fileUngetc (c);		/* unget non-identifier character */
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:
	do
	{
		c = fileGetc ();
		token->lineNumber   = getSourceLineNumber ();
		token->filePosition = getInputFilePosition ();
	}
	while (c == '\t'  ||  c == ' ' ||  c == '\n');

	switch (c)
	{
		case EOF: longjmp (Exception, (int)ExceptionEOF);	break;
		case '(': token->type = TOKEN_OPEN_PAREN;			break;
		case ')': token->type = TOKEN_CLOSE_PAREN;			break;
		case ';': token->type = TOKEN_SEMICOLON;			break;
		case ',': token->type = TOKEN_COMMA;				break;
		case '.': token->type = TOKEN_PERIOD;				break;
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '=': token->type = TOKEN_EQUAL_SIGN;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;
		case '?': token->type = TOKEN_QUESTION_MARK;		break;
		case '*': token->type = TOKEN_STAR;					break;

		case '\'':
		case '"':
				  token->type = TOKEN_STRING;
				  parseString (token->string, c);
				  token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '\\':
				  /*
				   * All Tex tags start with a backslash.
				   * Check if the next character is an alpha character
				   * else it is not a potential tex tag.
				   */
				  c = fileGetc ();
				  if (! isalpha (c))
					  fileUngetc (c);
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getSourceLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->keyword = analyzeToken (token->string, Lang_js);
					  if (isKeyword (token, KEYWORD_NONE))
						  token->type = TOKEN_IDENTIFIER;
					  else
						  token->type = TOKEN_KEYWORD;
				  }
				  break;

		case '%':
				  fileSkipToCharacter ('\n'); /* % are single line comments */
				  goto getNextChar;
				  break;

		default:
				  if (! isIdentChar (c))
					  token->type = TOKEN_UNDEFINED;
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getSourceLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->type = TOKEN_IDENTIFIER;
				  }
				  break;
	}
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy (dest->string, src->string);
	vStringCopy (dest->scope, src->scope);
}

/*
 *	 Scanning functions
 */

static boolean parseTag (tokenInfo *const token, texKind kind)
{
	tokenInfo *const name = newToken ();
	vString *	fullname;
	boolean		useLongName = TRUE;

	fullname = vStringNew ();
	vStringClear (fullname);

	/*
	 * Tex tags are of these formats:
	 *   \keyword{any number of words}
	 *   \keyword[short desc]{any number of words}
	 *   \keyword*[short desc]{any number of words}
	 *
	 * When a keyword is found, loop through all words within
	 * the curly braces for the tag name.
	 */

	if (isType (token, TOKEN_KEYWORD))
	{
		copyToken (name, token);
		readToken (token);
	}

	if (isType (token, TOKEN_OPEN_SQUARE))
	{
		useLongName = FALSE;

		readToken (token);
		while (! isType (token, TOKEN_CLOSE_SQUARE) )
		{
			if (isType (token, TOKEN_IDENTIFIER))
			{
				if (fullname->length > 0)
					vStringCatS (fullname, " ");
				vStringCatS (fullname, vStringValue (token->string));
			}
			readToken (token);
		}
		vStringTerminate (fullname);
		vStringCopy (name->string, fullname);
		makeTexTag (name, kind);
	}

	if (isType (token, TOKEN_STAR))
	{
		readToken (token);
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		readToken (token);
		while (! isType (token, TOKEN_CLOSE_CURLY) )
		{
			if (isType (token, TOKEN_IDENTIFIER) && useLongName)
			{
				if (fullname->length > 0)
					vStringCatS (fullname, " ");
				vStringCatS (fullname, vStringValue (token->string));
			}
			readToken (token);
		}
		if (useLongName) 
		{
			vStringTerminate (fullname);
			vStringCopy (name->string, fullname);
			makeTexTag (name, kind);
		}
	}

	deleteToken (name);
	vStringDelete (fullname);
	return TRUE;
}

static void parseTexFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_chapter:	
					parseTag (token, TEXTAG_CHAPTER); 
					break;
				case KEYWORD_section:	
					parseTag (token, TEXTAG_SECTION); 
					break;
				case KEYWORD_subsection:	
					parseTag (token, TEXTAG_SUBSUBSECTION); 
					break;
				case KEYWORD_subsubsection:	
					parseTag (token, TEXTAG_SUBSUBSECTION); 
					break;
				case KEYWORD_part:	
					parseTag (token, TEXTAG_PART); 
					break;
				case KEYWORD_paragraph:	
					parseTag (token, TEXTAG_PARAGRAPH); 
					break;
				case KEYWORD_subparagraph:	
					parseTag (token, TEXTAG_SUBPARAGRAPH); 
					break;
				default:
					break;
			}
		} 
	} while (TRUE);
}

static void initialize (const langType language)
{
	Assert (sizeof (TexKinds) / sizeof (TexKinds [0]) == TEXTAG_COUNT);
	Lang_js = language;
	buildTexKeywordHash ();
}

static void findTexTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception;
	
	exception = (exception_t) (setjmp (Exception));
	while (exception == ExceptionNone)
		parseTexFile (token);

	deleteToken (token);
}

/* Create parser definition stucture */
extern parserDefinition* TexParser (void)
{
	static const char *const extensions [] = { "tex", NULL };
	parserDefinition *const def = parserNew ("Tex");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kinds		= TexKinds;
	def->kindCount	= KIND_COUNT (TexKinds);
	def->parser		= findTexTags;
	def->initialize = initialize;

	return def;
}
/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
