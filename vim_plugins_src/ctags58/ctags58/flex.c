/*
 *	 $Id: flex.c 666 2008-05-15 17:47:31Z dfishburn $
 *
 *	 Copyright (c) 2008, David Fishburn
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License.
 *
 *	 This module contains functions for generating tags for Adobe languages.
 *	 There are a number of different ones, but this will begin with:
 *	     Flex
 *	         MXML files (*.mMacromedia XML)
 *	         ActionScript files (*.as)
 *
 *	 Flex 3 language reference
 *		 http://livedocs.adobe.com/flex/3/langref/index.html
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
 * Tracks class and function names already created
 */
static stringList *ClassNames;
static stringList *FunctionNames;

/*	Used to specify type of keyword.
*/
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_function,
	KEYWORD_capital_function,
	KEYWORD_object,
	KEYWORD_capital_object,
	KEYWORD_prototype,
	KEYWORD_var,
	KEYWORD_new,
	KEYWORD_this,
	KEYWORD_for,
	KEYWORD_while,
	KEYWORD_do,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_switch,
	KEYWORD_try,
	KEYWORD_catch,
	KEYWORD_finally,
	KEYWORD_public,
	KEYWORD_private,
	KEYWORD_static,
	KEYWORD_class,
	KEYWORD_id,
	KEYWORD_script,
	KEYWORD_cdata,
	KEYWORD_mx
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
	TOKEN_QUESTION_MARK
} tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long 	lineNumber;
	fpos_t 			filePosition;
	int				nestLevel;
	boolean			ignoreTag;
	boolean			isClass;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_js;

static jmp_buf Exception;

typedef enum {
	FLEXTAG_FUNCTION,
	FLEXTAG_CLASS,
	FLEXTAG_METHOD,
	FLEXTAG_PROPERTY,
	FLEXTAG_VARIABLE,
	FLEXTAG_MXTAG,
	FLEXTAG_COUNT
} flexKind;

static kindOption FlexKinds [] = {
	{ TRUE,  'f', "function",	  "functions"		   },
	{ TRUE,  'c', "class",		  "classes"			   },
	{ TRUE,  'm', "method",		  "methods"			   },
	{ TRUE,  'p', "property",	  "properties"		   },
	{ TRUE,  'v', "variable",	  "global variables"   },
	{ TRUE,  'x', "mxtag",		  "mxtags" 			   }
};

static const keywordDesc FlexKeywordTable [] = {
	/* keyword		keyword ID */
	{ "function",	KEYWORD_function			},
	{ "Function",	KEYWORD_capital_function	},
	{ "object",		KEYWORD_object				},
	{ "Object",		KEYWORD_capital_object		},
	{ "prototype",	KEYWORD_prototype			},
	{ "var",		KEYWORD_var					},
	{ "new",		KEYWORD_new					},
	{ "this",		KEYWORD_this				},
	{ "for",		KEYWORD_for					},
	{ "while",		KEYWORD_while				},
	{ "do",			KEYWORD_do					},
	{ "if",			KEYWORD_if					},
	{ "else",		KEYWORD_else				},
	{ "switch",		KEYWORD_switch				},
	{ "try",		KEYWORD_try					},
	{ "catch",		KEYWORD_catch				},
	{ "finally",	KEYWORD_finally				},
	{ "public",		KEYWORD_public				},
	{ "private",	KEYWORD_private				},
	{ "static",		KEYWORD_static				},
	{ "class",		KEYWORD_class				},
	{ "id",			KEYWORD_id					},
	{ "script",		KEYWORD_script				},
	{ "cdata",		KEYWORD_cdata				},
	{ "mx",			KEYWORD_mx					}
};

/*
 *	 FUNCTION DEFINITIONS
 */

/* Recursive functions */
static void parseFunction (tokenInfo *const token);
static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent);
static boolean parseLine (tokenInfo *const token);
static boolean parseActionScript (tokenInfo *const token);

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) || isdigit (c) || c == '$' || 
		 c == '@' || c == '_' || c == '#');
}

static void buildFlexKeywordHash (void)
{
	const size_t count = sizeof (FlexKeywordTable) /
		sizeof (FlexKeywordTable [0]);
	size_t i;
	for (i = 0	;  i < count  ;  ++i)
	{
		const keywordDesc* const p = &FlexKeywordTable [i];
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
	token->nestLevel	= 0;
	token->isClass		= FALSE;
	token->ignoreTag	= FALSE;
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

static void makeConstTag (tokenInfo *const token, const flexKind kind)
{
	if (FlexKinds [kind].enabled && ! token->ignoreTag )
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName	   = FlexKinds [kind].name;
		e.kind		   = FlexKinds [kind].letter;

		makeTagEntry (&e);
	}
}

static void makeFlexTag (tokenInfo *const token, flexKind kind)
{
	vString *	fulltag;

	if (FlexKinds [kind].enabled && ! token->ignoreTag )
	{
	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n makeFlexTag start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
		if (kind == FLEXTAG_FUNCTION && token->isClass )
		{
			kind = FLEXTAG_METHOD;
		}
		/*
		 * If a scope has been added to the token, change the token
		 * string to include the scope when making the tag.
		 */
		if ( vStringLength(token->scope) > 0 )
		{
			fulltag = vStringNew ();
			vStringCopy(fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue(token->string));
			vStringTerminate(fulltag);
			vStringCopy(token->string, fulltag);
			vStringDelete (fulltag);
		}
		makeConstTag (token, kind);
	}
}

static void makeClassTag (tokenInfo *const token)
{ 
	vString *	fulltag;

	if ( ! token->ignoreTag )
	{
		fulltag = vStringNew ();
		if (vStringLength (token->scope) > 0)
		{
			vStringCopy(fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue(token->string));
		}
		else
		{
			vStringCopy(fulltag, token->string);
		}
		vStringTerminate(fulltag);
		if ( ! stringListHas(ClassNames, vStringValue (fulltag)) )
		{
			stringListAdd (ClassNames, vStringNewCopy (fulltag));
			makeFlexTag (token, FLEXTAG_CLASS);
		}
		vStringDelete (fulltag);
	}
}

static void makeMXTag (tokenInfo *const token)
{ 
	vString *	fulltag;

	if ( ! token->ignoreTag )
	{
		fulltag = vStringNew ();
		if (vStringLength (token->scope) > 0)
		{
			vStringCopy(fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue(token->string));
		}
		else
		{
			vStringCopy(fulltag, token->string);
		}
		vStringTerminate(fulltag);
		makeFlexTag (token, FLEXTAG_MXTAG);
		vStringDelete (fulltag);
	}
}

static void makeFunctionTag (tokenInfo *const token)
{ 
	vString *	fulltag;

	if ( ! token->ignoreTag )
	{
		fulltag = vStringNew ();
		if (vStringLength (token->scope) > 0)
		{
			vStringCopy(fulltag, token->scope);
			vStringCatS (fulltag, ".");
			vStringCatS (fulltag, vStringValue(token->string));
		}
		else
		{
			vStringCopy(fulltag, token->string);
		}
		vStringTerminate(fulltag);
		if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) )
		{
			stringListAdd (FunctionNames, vStringNewCopy (fulltag));
			makeFlexTag (token, FLEXTAG_FUNCTION);
		}
		vStringDelete (fulltag);
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
			vStringPut(string, c);
		}
		else if (c == delimiter)
			end = TRUE;
		else
			vStringPut (string, c);
	}
	vStringTerminate (string);
}

/*	Read a C identifier beginning with "firstChar" and places it into
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

		case '\'':
		case '"':
				  token->type = TOKEN_STRING;
				  parseString (token->string, c);
				  token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '\\':
				  c = fileGetc ();
				  if (c != '\\'  && c != '"'  &&  !isspace (c))
					  fileUngetc (c);
				  token->type = TOKEN_CHARACTER;
				  token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '/':
				  {
					  int d = fileGetc ();
					  if ( (d != '*') &&		/* is this the start of a comment? */
							  (d != '/') &&		/* is a one line comment? */
							  (d != '>') )		/* is this a close XML tag? */
					  {
						  fileUngetc (d);
						  token->type = TOKEN_FORWARD_SLASH;
						  token->lineNumber = getSourceLineNumber ();
						  token->filePosition = getInputFilePosition ();
					  }
					  else
					  {
						  if (d == '*')
						  {
							  do
							  {
								  fileSkipToCharacter ('*');
								  c = fileGetc ();
								  if (c == '/')
									  break;
								  else
									  fileUngetc (c);
							  } while (c != EOF && c != '\0');
							  goto getNextChar;
						  }
						  else if (d == '/')	/* is this the start of a comment?  */
						  {
							  fileSkipToCharacter ('\n');
							  goto getNextChar;
						  }
						  else if (d == '>')	/* is this the start of a comment?  */
						  {
							  token->type = TOKEN_CLOSE_SGML;
							  token->lineNumber = getSourceLineNumber ();
							  token->filePosition = getInputFilePosition ();
						  }
					  }
					  break;
				  }

		case '<':
				  {
					  /*
					   * An XML comment looks like this 
					   *   <!-- anything over multiple lines -->
					   */
					  int d = fileGetc ();

					  if ( (d != '!' )  && 		/* is this the start of a comment? */
					       (d != '/' )  &&	 	/* is this the start of a closing mx tag */
					       (d != 'm' )    ) 	/* is this the start of a mx tag */
					  {
						  fileUngetc (d);
						  token->type = TOKEN_LESS_THAN;
						  token->lineNumber = getSourceLineNumber ();
						  token->filePosition = getInputFilePosition ();

					  }
					  else
					  {
						  if (d == '!')
						  {
							  int e = fileGetc ();
							  if ( e != '-' ) 		/* is this the start of a comment? */
							  {
								  fileUngetc (e);
								  fileUngetc (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getSourceLineNumber ();
								  token->filePosition = getInputFilePosition ();
							  }
							  else
							  {
								  if (e == '-')
								  {
									  int f = fileGetc ();
									  if ( f != '-' ) 		/* is this the start of a comment? */
									  {
										  fileUngetc (f);
										  fileUngetc (e);
										  fileUngetc (d);
										  token->type = TOKEN_LESS_THAN;
										  token->lineNumber = getSourceLineNumber ();
										  token->filePosition = getInputFilePosition ();
									  }
									  else
									  {
										  if (f == '-')
										  {
											  do
											  {
												  fileSkipToCharacter ('-');
												  c = fileGetc ();
												  if (c == '-') 
												  {
													  d = fileGetc ();
													  if (d == '>')
														  break;
													  else
													  {
														  fileUngetc (d);
														  fileUngetc (c);
													  }
													  break;
												  }
												  else
													  fileUngetc (c);
											  } while (c != EOF && c != '\0');
											  goto getNextChar;
										  }
									  }
								  }
							  }
						  }
						  else if (d == 'm')
						  {
							  int e = fileGetc ();
							  if ( e != 'x' ) 		/* continuing an mx tag */
							  {
								  fileUngetc (e);
								  fileUngetc (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getSourceLineNumber ();
								  token->filePosition = getInputFilePosition ();
							  }
							  else
							  {
								  if (e == 'x')
								  {
									  int f = fileGetc ();
									  if ( f != ':' ) 		/* is this the start of a comment? */
									  {
										  fileUngetc (f);
										  fileUngetc (e);
										  fileUngetc (d);
										  token->type = TOKEN_LESS_THAN;
										  token->lineNumber = getSourceLineNumber ();
										  token->filePosition = getInputFilePosition ();
									  }
									  else
									  {
										  if (f == ':')
										  {
											  token->type = TOKEN_OPEN_MXML;
											  token->lineNumber = getSourceLineNumber ();
											  token->filePosition = getInputFilePosition ();
										  }
									  }
								  }
							  }
						  }
						  else if (d == '/')
						  {
							  int e = fileGetc ();
							  if ( e != 'm' ) 		/* continuing an mx tag */
							  {
								  fileUngetc (e);
								  fileUngetc (d);
								  token->type = TOKEN_LESS_THAN;
								  token->lineNumber = getSourceLineNumber ();
								  token->filePosition = getInputFilePosition ();
							  }
							  else
							  {
								  int f = fileGetc ();
								  if ( f != 'x' ) 		/* continuing an mx tag */
								  {
									  fileUngetc (f);
									  fileUngetc (e);
									  token->type = TOKEN_LESS_THAN;
									  token->lineNumber = getSourceLineNumber ();
									  token->filePosition = getInputFilePosition ();
								  }
								  else
								  {
									  if (f == 'x')
									  {
										  int g = fileGetc ();
										  if ( g != ':' ) 		/* is this the start of a comment? */
										  {
											  fileUngetc (g);
											  fileUngetc (f);
											  fileUngetc (e);
											  token->type = TOKEN_LESS_THAN;
											  token->lineNumber = getSourceLineNumber ();
											  token->filePosition = getInputFilePosition ();
										  }
										  else
										  {
											  if (g == ':')
											  {
												  token->type = TOKEN_CLOSE_MXML;
												  token->lineNumber = getSourceLineNumber ();
												  token->filePosition = getInputFilePosition ();
											  }
										  }
									  }
								  }
							  }
						  }
					  }
					  break;
				  }

		case '>':
				  token->type = TOKEN_GREATER_THAN;
				  token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '!': 
				  token->type = TOKEN_EXCLAMATION;			
				  /*token->lineNumber = getSourceLineNumber ();
				  token->filePosition = getInputFilePosition ();*/
				  break;

		default:
				  if (! isIdentChar (c))
					  token->type = TOKEN_UNDEFINED;
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
	}
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->nestLevel = src->nestLevel;
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	dest->isClass = src->isClass;
	vStringCopy(dest->string, src->string);
	vStringCopy(dest->scope, src->scope);
}

/*
 *	 Token parsing functions
 */

static void skipArgumentList (tokenInfo *const token)
{
	int nest_level = 0;

	/*
	 * Other databases can have arguments with fully declared
	 * datatypes:
	 *	 (	name varchar(30), text binary(10)  )
	 * So we must check for nested open and closing parantheses
	 */

	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		nest_level++;
		while (! (isType (token, TOKEN_CLOSE_PAREN) && (nest_level == 0)))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				nest_level++;
			}
			if (isType (token, TOKEN_CLOSE_PAREN))
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

static void skipArrayList (tokenInfo *const token)
{
	int nest_level = 0;

	/*
	 * Handle square brackets
	 *	 var name[1]
	 * So we must check for nested open and closing square brackets
	 */

	if (isType (token, TOKEN_OPEN_SQUARE))	/* arguments? */
	{
		nest_level++;
		while (! (isType (token, TOKEN_CLOSE_SQUARE) && (nest_level == 0)))
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_SQUARE))
			{
				nest_level++;
			}
			if (isType (token, TOKEN_CLOSE_SQUARE))
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

static void addContext (tokenInfo* const parent, const tokenInfo* const child)
{
	if (vStringLength (parent->string) > 0)
	{
		vStringCatS (parent->string, ".");
	}
	vStringCatS (parent->string, vStringValue(child->string));
	vStringTerminate(parent->string);
}

static void addToScope (tokenInfo* const token, vString* const extra)
{
	if (vStringLength (token->scope) > 0)
	{
		vStringCatS (token->scope, ".");
	}
	vStringCatS (token->scope, vStringValue(extra));
	vStringTerminate(token->scope);
}

/*
 *	 Scanning functions
 */

static void findCmdTerm (tokenInfo *const token)
{
	/*
	 * Read until we find either a semicolon or closing brace. 
	 * Any nested braces will be handled within.
	 */
	while (! ( isType (token, TOKEN_SEMICOLON) ||
				isType (token, TOKEN_CLOSE_CURLY) ) )
	{
		/* Handle nested blocks */
		if ( isType (token, TOKEN_OPEN_CURLY))
		{
			parseBlock (token, token);
		} 
		else if ( isType (token, TOKEN_OPEN_PAREN) )
		{
			skipArgumentList(token);
		}
		else 
		{
			readToken (token);
		}
	} 
}

static void parseSwitch (tokenInfo *const token)
{
	/*
	 * switch (expression){
	 * case value1:
	 *	   statement;
	 *	   break;
	 * case value2:
	 *	   statement;
	 *	   break;
	 * default : statement;
	 * }
	 */

	readToken (token);

	if (isType (token, TOKEN_OPEN_PAREN)) 
	{
		skipArgumentList(token);
	}

	if (isType (token, TOKEN_OPEN_CURLY)) 
	{
		do
		{
			readToken (token);
		} while (! (isType (token, TOKEN_CLOSE_SGML) || 
					isType (token, TOKEN_CLOSE_MXML) ||
					isType (token, TOKEN_CLOSE_CURLY) ||
					isType (token, TOKEN_GREATER_THAN)) ); 
	}

}

static void parseLoop (tokenInfo *const token)
{
	/*
	 * Handles these statements
	 *	   for (x=0; x<3; x++)
	 *		   document.write("This text is repeated three times<br>");
	 *	   
	 *	   for (x=0; x<3; x++)
	 *	   {
	 *		   document.write("This text is repeated three times<br>");
	 *	   }
	 *	   
	 *	   while (number<5){
	 *		   document.write(number+"<br>");
	 *		   number++;
	 *	   }
	 *	   
	 *	   do{
	 *		   document.write(number+"<br>");
	 *		   number++;
	 *	   }
	 *	   while (number<5);
	 */

	if (isKeyword (token, KEYWORD_for) || isKeyword (token, KEYWORD_while))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_PAREN)) 
		{
			/*
			 * Handle nameless functions, these will only
			 * be considered methods.
			 */
			skipArgumentList(token);
		}

		if (isType (token, TOKEN_OPEN_CURLY)) 
		{
			/*
			 * This will be either a function or a class.
			 * We can only determine this by checking the body
			 * of the function.  If we find a "this." we know
			 * it is a class, otherwise it is a function.
			 */
			parseBlock (token, token);
		} 
		else 
		{
			parseLine(token);
		}
	} 
	else if (isKeyword (token, KEYWORD_do))
	{
		readToken(token);

		if (isType (token, TOKEN_OPEN_CURLY)) 
		{
			/*
			 * This will be either a function or a class.
			 * We can only determine this by checking the body
			 * of the function.  If we find a "this." we know
			 * it is a class, otherwise it is a function.
			 */
			parseBlock (token, token);
		} 
		else 
		{
			parseLine(token);
		}

		readToken(token);

		if (isKeyword (token, KEYWORD_while))
		{
			readToken(token);

			if (isType (token, TOKEN_OPEN_PAREN)) 
			{
				/*
				 * Handle nameless functions, these will only
				 * be considered methods.
				 */
				skipArgumentList(token);
			}
		}
	}
}

static boolean parseIf (tokenInfo *const token)
{
	boolean read_next_token = TRUE;
	/*
	 * If statements have two forms
	 *	   if ( ... )
	 *		   one line;
	 *
	 *	   if ( ... )  
	 *		  statement;
	 *	   else
	 *		  statement
	 *	    
	 *	   if ( ... ) {
	 *		  multiple;
	 *		  statements;
	 *	   }
	 *
	 *
	 *	   if ( ... ) {
	 *		  return elem
	 *	   }
	 *
	 *     This example if correctly written, but the
	 *     else contains only 1 statement without a terminator
	 *     since the function finishes with the closing brace.
	 *
     *     function a(flag){
     *         if(flag)
     *             test(1);
     *         else
     *             test(2)
     *     }
	 *
	 * TODO:  Deal with statements that can optional end
	 *		  without a semi-colon.  Currently this messes up
	 *		  the parsing of blocks.
	 *		  Need to somehow detect this has happened, and either
	 *		  backup a token, or skip reading the next token if 
	 *		  that is possible from all code locations.
	 *
	 */

	readToken (token);

	if (isKeyword (token, KEYWORD_if))
	{
		/*
		 * Check for an "else if" and consume the "if"
		 */
		readToken (token);
	}

	if (isType (token, TOKEN_OPEN_PAREN)) 
	{
		/* 
		 * Handle nameless functions, these will only
		 * be considered methods.
		 */
		skipArgumentList(token);
	}

	if (isType (token, TOKEN_OPEN_CURLY)) 
	{
		/*
		 * This will be either a function or a class.
		 * We can only determine this by checking the body
		 * of the function.  If we find a "this." we know
		 * it is a class, otherwise it is a function.
		 */
		parseBlock (token, token);
	} 
	else 
	{
		findCmdTerm (token);

		/*
		 * The IF could be followed by an ELSE statement.
		 * This too could have two formats, a curly braced
		 * multiline section, or another single line.
		 */

		if (isType (token, TOKEN_CLOSE_CURLY)) 
		{
			/*
			 * This statement did not have a line terminator.
			 */
			read_next_token = FALSE;
		} 
		else 
		{
			readToken (token);

			if (isType (token, TOKEN_CLOSE_CURLY)) 
			{
				/*
				* This statement did not have a line terminator.
				*/
				read_next_token = FALSE;
			} 
			else
			{
				if (isKeyword (token, KEYWORD_else))
					read_next_token = parseIf (token); 
			}
		} 
	}
	return read_next_token;
}

static void parseFunction (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
     *     private static function ioErrorHandler( event:IOErrorEvent ):void {
	 */

	if ( isKeyword(token, KEYWORD_function) )
	{
		readToken (token);
	}

	copyToken (name, token);
	/* Add scope in case this is an INNER function 
	addToScope(name, token->scope);
	*/

	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseFunction: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseFunction: name isClass:%d  scope:%s  name:%s\n"
				, name->isClass
				, vStringValue(name->scope)
				, vStringValue(name->string)
				);
			);

	readToken (token);

	if ( isType (token, TOKEN_OPEN_PAREN) )
		skipArgumentList(token);

	if ( isType (token, TOKEN_COLON) )
	{
		/*
		 *   function fname ():ReturnType 
		 */
		readToken (token);
		readToken (token);
	}

	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		DebugStatement ( 
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end: name isClass:%d  scope:%s  name:%s\n"
					, name->isClass
					, vStringValue(name->scope)
					, vStringValue(name->string)
					);
				);
		parseBlock (token, name);
		DebugStatement ( 
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end2: token isClass:%d  scope:%s  name:%s\n"
					, token->isClass
					, vStringValue(token->scope)
					, vStringValue(token->string)
					);
				);
		DebugStatement ( 
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end2: token isClass:%d  scope:%s  name:%s\n"
					, token->isClass
					, vStringValue(token->scope)
					, vStringValue(token->string)
					);
				);
		DebugStatement ( 
				debugPrintf (DEBUG_PARSE
					, "\n parseFunction end3: name isClass:%d  scope:%s  name:%s\n"
					, name->isClass
					, vStringValue(name->scope)
					, vStringValue(name->string)
					);
				);
		makeFunctionTag (name);
	}

	findCmdTerm (token);

	deleteToken (name);
}

static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent)
{
	boolean read_next_token = TRUE;
	vString * saveScope = vStringNew ();

	vStringClear(saveScope);
	vStringCopy (saveScope, token->scope);
	token->nestLevel++;
	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseBlock start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	/*
	 * Make this routine a bit more forgiving.
	 * If called on an open_curly advance it
	 */
	if ( isType (token, TOKEN_OPEN_CURLY) && 
			isKeyword(token, KEYWORD_NONE) )
		readToken(token);

	if (! isType (token, TOKEN_CLOSE_CURLY))
	{
		/*
		 * Read until we find the closing brace, 
		 * any nested braces will be handled within
		 */
		do
		{
			if (isType (token, TOKEN_OPEN_CURLY))
			{
				/* Handle nested blocks */
				parseBlock (token, parent);
			} 
			else 
			{
				/*
				 * It is possible for a line to have no terminator
				 * if the following line is a closing brace.
				 * parseLine will detect this case and indicate
				 * whether we should read an additional token.
				 */
				read_next_token = parseLine (token);
			}

			/*
			 * Always read a new token unless we find a statement without
			 * a ending terminator
			 */
			if( read_next_token ) 
				readToken(token);

			/*
			 * If we find a statement without a terminator consider the 
			 * block finished, otherwise the stack will be off by one.
			 */
		} while (! isType (token, TOKEN_CLOSE_CURLY) && read_next_token );
	}

	vStringDelete(saveScope);
	token->nestLevel--;

	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseBlock end: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	return FALSE;
}

static void parseMethods (tokenInfo *const token, tokenInfo *const class)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   validProperty  : 2,
	 *	   validMethod    : function(a,b) {}
	 *	   'validMethod2' : function(a,b) {}
     *     container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}		
	 */

	do
	{
		readToken (token);
		if (isType (token, TOKEN_STRING) || isKeyword(token, KEYWORD_NONE))
		{
			copyToken (name, token);

			readToken (token);
			if ( isType (token, TOKEN_COLON) )
			{
				readToken (token);
				if ( isKeyword (token, KEYWORD_function) )
				{
					readToken (token);
					if ( isType (token, TOKEN_OPEN_PAREN) )
					{
						skipArgumentList(token);
					}

					if (isType (token, TOKEN_OPEN_CURLY)) 
					{
						addToScope (name, class->string);
						makeFlexTag (name, FLEXTAG_METHOD);
						parseBlock (token, name);

						/*
						 * Read to the closing curly, check next
						 * token, if a comma, we must loop again
						 */
						readToken (token);
					}
				}
				else
				{
						addToScope (name, class->string);
						makeFlexTag (name, FLEXTAG_PROPERTY);

						/*
						 * Read the next token, if a comma
						 * we must loop again
						 */
						readToken (token);
				}
			}
		}
	} while ( isType(token, TOKEN_COMMA) );

	findCmdTerm (token);

	deleteToken (name);
}

static boolean parseVar (tokenInfo *const token, boolean is_public)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const secondary_name = newToken ();
	vString * saveScope = vStringNew ();
	boolean is_terminated = TRUE;

	vStringClear(saveScope);
	vStringCopy (saveScope, token->scope);
	/*
	 * Variables are defined as:
	 *     private static var lastFaultMessage:Date = new Date( 0 );
	 *     private static var webRequests:ArrayCollection = new ArrayCollection();
	 */

	if ( isKeyword(token, KEYWORD_var) )
	{
		readToken(token);
	}

	/* Variable name */
	copyToken (name, token);
	readToken(token);

	if ( isType (token, TOKEN_COLON) )
	{
		/*
		 *   var vname ():DataType = new Date();
		 *   var vname ():DataType;
		 */
		readToken (token);
		readToken (token);
	}

	while (! isType (token, TOKEN_SEMICOLON) )
	{
		readToken (token);
	}

	if ( isType (token, TOKEN_SEMICOLON) )
	{
		/*
		 * Only create variables for global scope
		 */
		/* if ( token->nestLevel == 0 && is_global ) */
		if ( is_public )
		{
			if (isType (token, TOKEN_SEMICOLON)) 
				makeFlexTag (name, FLEXTAG_VARIABLE);
		}
	}

	vStringCopy(token->scope, saveScope);
	deleteToken (name);
	deleteToken (secondary_name);
	vStringDelete(saveScope);

	return is_terminated;
}

static boolean parseClass (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	vString * saveScope = vStringNew ();
	boolean saveIsClass = token->isClass;

	vStringClear(saveScope);
	vStringCopy (saveScope, token->scope);
	/*
	 * Variables are defined as:
	 *     private static var lastFaultMessage:Date = new Date( 0 );
	 *     private static var webRequests:ArrayCollection = new ArrayCollection();
	 */

	if ( isKeyword(token, KEYWORD_class) )
	{
		readToken(token);
	}

	token->isClass = TRUE;
	/* Add class name to scope */
	addToScope(token, token->string);
	/* Class name */
	copyToken (name, token);
	readToken(token);

	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseClass start: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		makeClassTag (name);
		parseBlock (token, name);
	}

	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseClass end: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	vStringCopy(token->scope, saveScope);
	token->isClass = saveIsClass;
	deleteToken (name);
	vStringDelete(saveScope);

	return TRUE;
}

static boolean parseStatement (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const secondary_name = newToken ();
	vString * saveScope = vStringNew ();
	boolean is_public = FALSE;
	boolean is_class = FALSE;
	boolean is_terminated = TRUE;
	boolean is_global = FALSE;
	boolean is_prototype = FALSE;
	vString *	fulltag;

	vStringClear(saveScope);
	vStringCopy (saveScope, token->scope);
	DebugStatement ( 
			debugPrintf (DEBUG_PARSE
				, "\n parseStatement: token isClass:%d  scope:%s  name:%s\n"
				, token->isClass
				, vStringValue(token->scope)
				, vStringValue(token->string)
				);
			);
	/*
	 * Functions can be named or unnamed.
	 * This deals with these formats:
	 * Function
	 *	   validFunctionOne = function(a,b) {}
	 *	   testlib.validFunctionFive = function(a,b) {}
	 *	   var innerThree = function(a,b) {}
	 *	   var innerFour = (a,b) {}
	 *	   var D2 = secondary_fcn_name(a,b) {}
	 *	   var D3 = new Function("a", "b", "return a+b;");
	 * Class
	 *	   testlib.extras.ValidClassOne = function(a,b) {
	 *		   this.a = a; 
	 *	   }
	 * Class Methods
	 *	   testlib.extras.ValidClassOne.prototype = {
	 *		   'validMethodOne' : function(a,b) {},
	 *		   'validMethodTwo' : function(a,b) {}
	 *	   }
     *     ValidClassTwo = function () 
     *     {
     *         this.validMethodThree = function() {}
     *         // unnamed method
     *         this.validMethodFour = () {}
     *     }
	 *	   Database.prototype.validMethodThree = Database_getTodaysDate;
	 */

	if ( isKeyword(token, KEYWORD_public) )
	{
		is_public = TRUE;
		readToken(token);
	}

	if ( isKeyword(token, KEYWORD_private) )
	{
		readToken(token);
	}

	if ( isKeyword(token, KEYWORD_static) )
	{
		readToken(token);
	}

	if (isType(token, TOKEN_KEYWORD))
	{
		switch (token->keyword)
		{
			case KEYWORD_for:	   
			case KEYWORD_while:
			case KEYWORD_do:
				parseLoop (token); 
				break;
			case KEYWORD_if:
			case KEYWORD_else:
			case KEYWORD_try:
			case KEYWORD_catch:
			case KEYWORD_finally:
				/* Common semantics */
				is_terminated = parseIf (token); 
				break;
			case KEYWORD_switch:
				parseSwitch (token); 
				break;
			case KEYWORD_class:
				parseClass (token); 
				return is_terminated;
				break;
			case KEYWORD_function:
				parseFunction (token); 
				return is_terminated;
				break;
			case KEYWORD_var:
				parseVar (token, is_public); 
				return is_terminated;
				break;
			default:
				readToken(token);
				break;
		}
	} 

	copyToken (name, token);

	while (! isType (token, TOKEN_CLOSE_CURLY) &&
	       ! isType (token, TOKEN_SEMICOLON)   &&
	       ! isType (token, TOKEN_EQUAL_SIGN)  )
	{
		/* Potentially the name of the function */
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			/*
			 * Cannot be a global variable is it has dot references in the name
			 */
			is_global = FALSE;
			do
			{
				readToken (token);
				if ( isKeyword(token, KEYWORD_NONE) )
				{
					if ( is_class )
					{
						vStringCopy(saveScope, token->scope);
						addToScope(token, name->string);
					} 
					else 
						addContext (name, token);
				} 
				else if ( isKeyword(token, KEYWORD_prototype) ) 
				{
					/*
					 * When we reach the "prototype" tag, we infer:
					 *     "BindAgent" is a class
					 *     "build"     is a method
					 *
					 * function BindAgent( repeatableIdName, newParentIdName ) {
					 * }	
					 *
					 * CASE 1
					 * Specified function name: "build"
					 *     BindAgent.prototype.build = function( mode ) {
					 *     	  ignore everything within this function
					 *     }
					 *
					 * CASE 2
					 * Prototype listing
					 *     ValidClassOne.prototype = {
					 *         'validMethodOne' : function(a,b) {},
					 *         'validMethodTwo' : function(a,b) {}
					 *     }
					 *
					 */
					makeClassTag (name);
					is_class = TRUE;
					is_prototype = TRUE;

					/*
					 * There should a ".function_name" next.
					 */
					readToken (token);
					if (isType (token, TOKEN_PERIOD))
					{
						/*
						 * Handle CASE 1
						 */
						readToken (token);
						if ( isKeyword(token, KEYWORD_NONE) )
						{
							vStringCopy(saveScope, token->scope);
							addToScope(token, name->string);

							makeFlexTag (token, FLEXTAG_METHOD);
							/*
							 * We can read until the end of the block / statement.
							 * We need to correctly parse any nested blocks, but
							 * we do NOT want to create any tags based on what is
							 * within the blocks.
							 */
							token->ignoreTag = TRUE;
							/*
							 * Find to the end of the statement 
							 */
							findCmdTerm (token);
							token->ignoreTag = FALSE;
							is_terminated = TRUE;
							goto cleanUp;
						}
					} 
					else if (isType (token, TOKEN_EQUAL_SIGN)) 
					{
						readToken (token);
						if (isType (token, TOKEN_OPEN_CURLY)) 
						{
							/*
							 * Handle CASE 2
							 *
							 * Creates tags for each of these class methods
							 *     ValidClassOne.prototype = {
							 *         'validMethodOne' : function(a,b) {},
							 *         'validMethodTwo' : function(a,b) {}
							 *     }
							 */
							parseMethods(token, name);
							/*
							 * Find to the end of the statement 
							 */
							findCmdTerm (token);
							token->ignoreTag = FALSE;
							is_terminated = TRUE;
							goto cleanUp;
						}
					}
				}
				readToken (token);
			} while (isType (token, TOKEN_PERIOD));
		}

		if ( isType (token, TOKEN_OPEN_PAREN) )
			skipArgumentList(token);

		if ( isType (token, TOKEN_COLON) )
		{
			/*
			 * Functions are of this form:
			 *   function fname ():ReturnType {
			 */
			readToken (token);
			readToken (token);
		}

		if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token);

	}

	if ( isType (token, TOKEN_CLOSE_CURLY) )
	{
		/*
		 * Reaching this section without having
		 * processed an open curly brace indicates
		 * the statement is most likely not terminated.
		 */
		is_terminated = FALSE;
		goto cleanUp;
	}

	if ( isType (token, TOKEN_SEMICOLON) )
	{
		/*
		 * Only create variables for global scope
		 */
		if ( token->nestLevel == 0 && is_global )
		{
			/*
			 * Handles this syntax:
			 *	   var g_var2;
			 */
			if (isType (token, TOKEN_SEMICOLON)) 
				makeFlexTag (name, FLEXTAG_VARIABLE);
		}
		/* 
		 * Statement has ended.
		 * This deals with calls to functions, like:
		 *     alert(..);
		 */
		goto cleanUp;
	}

	if ( isType (token, TOKEN_EQUAL_SIGN) )
	{
		readToken (token);

		if ( isKeyword (token, KEYWORD_function) )
		{
			readToken (token);

			if ( isKeyword (token, KEYWORD_NONE) && 
					! isType (token, TOKEN_OPEN_PAREN) )
			{
				/*
				 * Functions of this format:
				 *	   var D2A = function theAdd(a, b) 
				 *	   {					 
				 *		  return a+b;
				 *	   }					 
				 * Are really two separate defined functions and 
				 * can be referenced in two ways:
				 *	   alert( D2A(1,2) );			  // produces 3
				 *	   alert( theAdd(1,2) );		  // also produces 3
				 * So it must have two tags:
				 *	   D2A
				 *	   theAdd
				 * Save the reference to the name for later use, once
				 * we have established this is a valid function we will
				 * create the secondary reference to it.
				 */
				copyToken (secondary_name, token);
				readToken (token);
			}

			if ( isType (token, TOKEN_OPEN_PAREN) )
				skipArgumentList(token);

			if (isType (token, TOKEN_OPEN_CURLY)) 
			{
				/*
				 * This will be either a function or a class.
				 * We can only determine this by checking the body
				 * of the function.  If we find a "this." we know
				 * it is a class, otherwise it is a function.
				 */
				if ( token->isClass ) 
				{
					makeFlexTag (name, FLEXTAG_METHOD);
					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name);
					parseBlock (token, name);
				} 
				else 
				{
					parseBlock (token, name);
					makeFunctionTag (name);

					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name);

					/*
					 * Find to the end of the statement 
					 */
					goto cleanUp;
				}
			}
		} 
		else if (isType (token, TOKEN_OPEN_PAREN)) 
		{
			/*
			 * Handle nameless functions
			 *     this.method_name = () {}
			 */
			skipArgumentList(token);

			if (isType (token, TOKEN_OPEN_CURLY)) 
			{
				/*
				 * Nameless functions are only setup as methods.
				 */
				makeFlexTag (name, FLEXTAG_METHOD);
				parseBlock (token, name);
			}
		} 
		else if (isType (token, TOKEN_OPEN_CURLY)) 
		{
			/*
			 * Creates tags for each of these class methods
			 *     ValidClassOne.prototype = {
			 *         'validMethodOne' : function(a,b) {},
			 *         'validMethodTwo' : function(a,b) {}
			 *     }
			 */
			parseMethods(token, name);
			if (isType (token, TOKEN_CLOSE_CURLY)) 
			{
				/*
				 * Assume the closing parantheses terminates
				 * this statements.
				 */
				is_terminated = TRUE;
			}
		}
		else if (isKeyword (token, KEYWORD_new))
		{
			readToken (token);
			if ( isKeyword (token, KEYWORD_function) || 
					isKeyword (token, KEYWORD_capital_function) ||
					isKeyword (token, KEYWORD_object) ||
					isKeyword (token, KEYWORD_capital_object) )
			{
				if ( isKeyword (token, KEYWORD_object) || 
						isKeyword (token, KEYWORD_capital_object) )
					is_class = TRUE;

				readToken (token);
				if ( isType (token, TOKEN_OPEN_PAREN) )
					skipArgumentList(token);

				if (isType (token, TOKEN_SEMICOLON)) 
				{
					if ( token->nestLevel == 0 )
					{
						if ( is_class )
						{
							makeClassTag (name);
						} else {
							makeFunctionTag (name);
						}
					}
				}
			}
		}
		else if (isKeyword (token, KEYWORD_NONE))
		{
			/*
			 * Only create variables for global scope
			 */
			if ( token->nestLevel == 0 && is_global )
			{
				/*
				 * A pointer can be created to the function.  
				 * If we recognize the function/class name ignore the variable.
				 * This format looks identical to a variable definition.
				 * A variable defined outside of a block is considered
				 * a global variable:
				 *	   var g_var1 = 1;
				 *	   var g_var2;
				 * This is not a global variable:
				 *	   var g_var = function;
				 * This is a global variable:
				 *	   var g_var = different_var_name;
				 */
				fulltag = vStringNew ();
				if (vStringLength (token->scope) > 0)
				{
					vStringCopy(fulltag, token->scope);
					vStringCatS (fulltag, ".");
					vStringCatS (fulltag, vStringValue(token->string));
				}
				else
				{
					vStringCopy(fulltag, token->string);
				}
				vStringTerminate(fulltag);
				if ( ! stringListHas(FunctionNames, vStringValue (fulltag)) &&
						! stringListHas(ClassNames, vStringValue (fulltag)) )
				{
					findCmdTerm (token);
					if (isType (token, TOKEN_SEMICOLON)) 
						makeFlexTag (name, FLEXTAG_VARIABLE);
				}
				vStringDelete (fulltag);
			}
		}
	}
	findCmdTerm (token);

	/*
	 * Statements can be optionally terminated in the case of 
	 * statement prior to a close curly brace as in the
	 * document.write line below:
	 *
	 * function checkForUpdate() {
	 *	   if( 1==1 ) {
	 *		   document.write("hello from checkForUpdate<br>")
	 *	   }
	 *	   return 1;
	 * }
	 */
	if ( ! is_terminated && isType (token, TOKEN_CLOSE_CURLY)) 
		is_terminated = FALSE;


cleanUp:
	vStringCopy(token->scope, saveScope);
	deleteToken (name);
	deleteToken (secondary_name);
	vStringDelete(saveScope);

	return is_terminated;
}

static boolean parseLine (tokenInfo *const token)
{
	boolean is_terminated = TRUE;
	/*
	 * Detect the common statements, if, while, for, do, ...
	 * This is necessary since the last statement within a block "{}"
	 * can be optionally terminated.
	 *
	 * If the statement is not terminated, we need to tell
	 * the calling routine to prevent reading an additional token
	 * looking for the end of the statement.
	 */

	if (isType(token, TOKEN_KEYWORD))
	{
		switch (token->keyword)
		{
			case KEYWORD_for:	   
			case KEYWORD_while:
			case KEYWORD_do:
				parseLoop (token); 
				break;
			case KEYWORD_if:
			case KEYWORD_else:
			case KEYWORD_try:
			case KEYWORD_catch:
			case KEYWORD_finally:
				/* Common semantics */
				is_terminated = parseIf (token); 
				break;
			case KEYWORD_switch:
				parseSwitch (token); 
				break;
			default:			   
				parseStatement (token); 
				break;
		}
	} 
	else 
	{
		/*
		 * Special case where single line statements may not be
		 * SEMICOLON terminated.  parseBlock needs to know this
		 * so that it does not read the next token.
		 */
		is_terminated = parseStatement (token); 
	}
	return is_terminated;
}

static boolean parseCDATA (tokenInfo *const token)
{
	if (isType (token, TOKEN_LESS_THAN))
	{
		/*
		 * Handle these tags 
		 * <![CDATA[
		 *    ...
		 * ]]>
		 */
		readToken (token);
		if (isType (token, TOKEN_EXCLAMATION))
		{
			/*
			 * Not sure why I had to comment these out, but I did.
			 * readToken (token);
			 * if (isType (token, TOKEN_OPEN_SQUARE))
			 * {
			*/
				readToken (token);
				if (isKeyword (token, KEYWORD_cdata))
				{
					readToken (token);
					if (isType (token, TOKEN_OPEN_SQUARE))
					{
						parseActionScript (token);
						if (isType (token, TOKEN_CLOSE_SQUARE))
						{
							readToken (token);
							if (isType (token, TOKEN_CLOSE_SQUARE))
							{
								readToken (token);
							}
						}
					}
				}
			/*} Not sure */
		}
	}
	else
	{
		parseActionScript (token);
	}
	return TRUE;
}

static boolean parseMXML (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const type = newToken ();
	/*
	 * Detect the common statements, if, while, for, do, ...
	 * This is necessary since the last statement within a block "{}"
	 * can be optionally terminated.
	 *
	 * If the statement is not terminated, we need to tell
	 * the calling routine to prevent reading an additional token
	 * looking for the end of the statement.
	 */

	readToken (token);

	if (isKeyword (token, KEYWORD_script))
	{
		/*
		 * These tags can be of this form:
		 * <mx:Script src="filename.as" />
		 */
		do
		{
			readToken (token);
		} while (! (isType (token, TOKEN_CLOSE_SGML) || 
					isType (token, TOKEN_CLOSE_MXML) ||
					isType (token, TOKEN_GREATER_THAN)) ); 

		if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * We have found a </mx:type> tag 
			 * Finish reading the "type" and ">"
			 */
			readToken (token);
			readToken (token);
			goto cleanUp;
		}
		if (isType (token, TOKEN_CLOSE_SGML))
		{
			/*
			 * We have found a <mx:Script src="filename.as" />
			 */
			goto cleanUp;
		}

		/*
		 * This is a beginning of an embedded script.
		 * These typically are of this format:
		 *    <mx:Script>
		 *        <![CDATA[
		 *        ... ActionScript ...
		 *        ]]>
		 *    </mx:Script>
		 */
		readToken (token);
		parseCDATA (token);

		readToken (token);
		if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * We have found a </mx:type> tag 
			 * Finish reading the "type" and ">"
			 */
			readToken (token);
			readToken (token);
		}
		goto cleanUp;
	}

	copyToken (type, token);

	readToken (token);
	do
	{
		if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		}
		else if (isKeyword (token, KEYWORD_id))
		{
			/* = */
			readToken (token);
			readToken (token);

			copyToken (name, token);
			addToScope (name, type->string);
			makeMXTag (name);
		}
		readToken (token);
	} while (! (isType (token, TOKEN_CLOSE_SGML) || isType (token, TOKEN_CLOSE_MXML)) ); 

	if (isType (token, TOKEN_CLOSE_MXML))
	{
		/*
		 * We have found a </mx:type> tag 
		 * Finish reading the "type" and ">"
		 */
		readToken (token);
		readToken (token);
	}

cleanUp:
	deleteToken (name);
	deleteToken (type);
	return TRUE;
}

static boolean parseActionScript (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_LESS_THAN))
		{
			/*
			 * Handle these tags 
			 * <![CDATA[
			 *    ...
			 * ]]>
			 */
			readToken (token);
			if (isType (token, TOKEN_EQUAL_SIGN))
			{
				if (isType (token, TOKEN_OPEN_SQUARE))
				{
					readToken (token);
					if (isKeyword (token, KEYWORD_cdata))
					{
						readToken (token);
					}
				}
			}
		}
		if (isType (token, TOKEN_CLOSE_SQUARE))
		{
			/*
			 * Handle these tags 
			 * <![CDATA[
			 *    ...
			 * ]]>
			 */
			readToken (token);
			if (isType (token, TOKEN_CLOSE_SQUARE))
			{
				readToken (token);
				if (isType (token, TOKEN_GREATER_THAN))
				{
					return TRUE;
				}
			}
		}
		else if (isType (token, TOKEN_CLOSE_MXML))
		{
			/*
			 * Read the Script> tags 
			 */
			readToken (token);
			readToken (token);
			return TRUE;
		} 
		else if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		} 
		else 
		{
			if (isType(token, TOKEN_KEYWORD))
			{
				switch (token->keyword)
				{
					case KEYWORD_function:	parseFunction (token); break;
					default:				parseLine (token); break;
				}
			} 
			else 
			{
				parseLine (token); 
			}
		}
	} while (TRUE);
}

static void parseFlexFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_OPEN_MXML))
		{
			parseMXML (token);
		} 
		if (isType (token, TOKEN_LESS_THAN))
		{
			readToken (token);
			if (isType (token, TOKEN_QUESTION_MARK))
			{
				readToken (token);
				while (! isType (token, TOKEN_QUESTION_MARK) )
				{
					readToken (token);
				} 
				readToken (token);
			}
		} 
		else 
		{
			parseActionScript (token);
		}
	} while (TRUE);
}

static void initialize (const langType language)
{
	Assert (sizeof (FlexKinds) / sizeof (FlexKinds [0]) == FLEXTAG_COUNT);
	Lang_js = language;
	buildFlexKeywordHash ();
}

static void findFlexTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception;
	
	ClassNames = stringListNew ();
	FunctionNames = stringListNew ();
	
	exception = (exception_t) (setjmp (Exception));
	while (exception == ExceptionNone)
		parseFlexFile (token);

	stringListDelete (ClassNames);
	stringListDelete (FunctionNames);
	ClassNames = NULL;
	FunctionNames = NULL;
	deleteToken (token);
}

/* Create parser definition stucture */
extern parserDefinition* FlexParser (void)
{
	static const char *const extensions [] = { "as", "mxml", NULL };
	parserDefinition *const def = parserNew ("Flex");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kinds		= FlexKinds;
	def->kindCount	= KIND_COUNT (FlexKinds);
	def->parser		= findFlexTags;
	def->initialize = initialize;

	return def;
}
/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
