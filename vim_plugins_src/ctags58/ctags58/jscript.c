/*
 *	 $Id: jscript.c 666 2008-05-15 17:47:31Z dfishburn $
 *
 *	 Copyright (c) 2003, Darren Hiebert
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License.
 *
 *	 This module contains functions for generating tags for JavaScript language
 *	 files.
 *
 *	 This is a good reference for different forms of the function statement:
 *		 http://www.permadi.com/tutorial/jsFunc/
 *   Another good reference:
 *       http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide
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
	KEYWORD_finally
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
	TOKEN_FORWARD_SLASH,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE
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
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_js;

static jmp_buf Exception;

typedef enum {
	JSTAG_FUNCTION,
	JSTAG_CLASS,
	JSTAG_METHOD,
	JSTAG_PROPERTY,
	JSTAG_VARIABLE,
	JSTAG_COUNT
} jsKind;

static kindOption JsKinds [] = {
	{ TRUE,  'f', "function",	  "functions"		   },
	{ TRUE,  'c', "class",		  "classes"			   },
	{ TRUE,  'm', "method",		  "methods"			   },
	{ TRUE,  'p', "property",	  "properties"		   },
	{ TRUE,  'v', "variable",	  "global variables"   }
};

static const keywordDesc JsKeywordTable [] = {
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
	{ "finally",	KEYWORD_finally				}
};

/*
 *	 FUNCTION DEFINITIONS
 */

/* Recursive functions */
static void parseFunction (tokenInfo *const token);
static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent);
static boolean parseLine (tokenInfo *const token, boolean is_inside_class);

static boolean isIdentChar (const int c)
{
	return (boolean)
		(isalpha (c) || isdigit (c) || c == '$' || 
		 c == '@' || c == '_' || c == '#');
}

static void buildJsKeywordHash (void)
{
	const size_t count = sizeof (JsKeywordTable) /
		sizeof (JsKeywordTable [0]);
	size_t i;
	for (i = 0	;  i < count  ;  ++i)
	{
		const keywordDesc* const p = &JsKeywordTable [i];
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

static void makeConstTag (tokenInfo *const token, const jsKind kind)
{
	if (JsKinds [kind].enabled && ! token->ignoreTag )
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;
		e.kindName	   = JsKinds [kind].name;
		e.kind		   = JsKinds [kind].letter;

		makeTagEntry (&e);
	}
}

static void makeJsTag (tokenInfo *const token, const jsKind kind)
{
	vString *	fulltag;

	if (JsKinds [kind].enabled && ! token->ignoreTag )
	{
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
			makeJsTag (token, JSTAG_CLASS);
		}
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
			makeJsTag (token, JSTAG_FUNCTION);
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
							  (d != '/') )		/* is a one line comment? */
					  {
						  token->type = TOKEN_FORWARD_SLASH;
						  fileUngetc (d);
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
					  }
					  break;
				  }

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
			parseLine(token, FALSE);
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
			parseLine(token, FALSE);
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
	boolean is_class = FALSE;

	/*
	 * This deals with these formats
	 *	   function validFunctionTwo(a,b) {}
	 */

	readToken (name);
	/* Add scope in case this is an INNER function */
	addToScope(name, token->scope);

	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		do
		{
			readToken (token);
			if ( isKeyword(token, KEYWORD_NONE) )
			{
				addContext (name, token);
				readToken (token);
			}
		} while (isType (token, TOKEN_PERIOD));
	}

	if ( isType (token, TOKEN_OPEN_PAREN) )
		skipArgumentList(token);

	if ( isType (token, TOKEN_OPEN_CURLY) )
	{
		is_class = parseBlock (token, name);
		if ( is_class ) 
			makeClassTag (name);
		else 
			makeFunctionTag (name);
	}

	findCmdTerm (token);

	deleteToken (name);
}

static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent)
{
	boolean is_class = FALSE;
	boolean read_next_token = TRUE;
	vString * saveScope = vStringNew ();

	token->nestLevel++;
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
			read_next_token = TRUE;
			if (isKeyword (token, KEYWORD_this))
			{
				/*
				 * Means we are inside a class and have found
				 * a class, not a function
				 */
				is_class = TRUE;
				vStringCopy(saveScope, token->scope);
				addToScope (token, parent->string);

				/*
				 * Ignore the remainder of the line
				 * findCmdTerm(token);
				 */
				parseLine (token, is_class);

				vStringCopy(token->scope, saveScope);
			} 
			else if (isKeyword (token, KEYWORD_var))
			{
				/*
				 * Potentially we have found an inner function.
				 * Set something to indicate the scope
				 */
				vStringCopy(saveScope, token->scope);
				addToScope (token, parent->string);
				parseLine (token, is_class);
				vStringCopy(token->scope, saveScope);
			} 
			else if (isKeyword (token, KEYWORD_function))
			{
				vStringCopy(saveScope, token->scope);
				addToScope (token, parent->string);
				parseFunction (token);
				vStringCopy(token->scope, saveScope);
			} 
			else if (isType (token, TOKEN_OPEN_CURLY))
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
				read_next_token = parseLine (token, is_class);
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

	return is_class;
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
			copyToken(name, token);

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
						makeJsTag (name, JSTAG_METHOD);
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
						makeJsTag (name, JSTAG_PROPERTY);

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

static boolean parseStatement (tokenInfo *const token, boolean is_inside_class)
{
	tokenInfo *const name = newToken ();
	tokenInfo *const secondary_name = newToken ();
	vString * saveScope = vStringNew ();
	boolean is_class = FALSE;
	boolean is_terminated = TRUE;
	boolean is_global = FALSE;
	boolean is_prototype = FALSE;
	vString *	fulltag;

	vStringClear(saveScope);
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

	if ( is_inside_class ) 
		is_class = TRUE;
	/*
	 * var can preceed an inner function
	 */
	if ( isKeyword(token, KEYWORD_var) )
	{
		/*
		 * Only create variables for global scope
		 */
		if ( token->nestLevel == 0 )
		{
			is_global = TRUE;
		}
		readToken(token);
	}

	if ( isKeyword(token, KEYWORD_this) )
	{
		readToken(token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken(token);
		}
	}

	copyToken(name, token);

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

							makeJsTag (token, JSTAG_METHOD);
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

		if ( isType (token, TOKEN_OPEN_SQUARE) )
			skipArrayList(token);

		/*
		if ( isType (token, TOKEN_OPEN_CURLY) )
		{
			is_class = parseBlock (token, name);
		}
		*/
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
				makeJsTag (name, JSTAG_VARIABLE);
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
				copyToken(secondary_name, token);
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
				if ( is_inside_class ) 
				{
					makeJsTag (name, JSTAG_METHOD);
					if ( vStringLength(secondary_name->string) > 0 )
						makeFunctionTag (secondary_name);
					parseBlock (token, name);
				} 
				else 
				{
					is_class = parseBlock (token, name);
					if ( is_class ) 
						makeClassTag (name);
					else 
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
				makeJsTag (name, JSTAG_METHOD);
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
						makeJsTag (name, JSTAG_VARIABLE);
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

static boolean parseLine (tokenInfo *const token, boolean is_inside_class)
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
				parseStatement (token, is_inside_class); 
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
		is_terminated = parseStatement (token, is_inside_class); 
	}
	return is_terminated;
}

static void parseJsFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType(token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_function:	parseFunction (token); break;
				default:				parseLine (token, FALSE); break;
			}
		} 
		else 
		{
			parseLine (token, FALSE); 
		}
	} while (TRUE);
}

static void initialize (const langType language)
{
	Assert (sizeof (JsKinds) / sizeof (JsKinds [0]) == JSTAG_COUNT);
	Lang_js = language;
	buildJsKeywordHash ();
}

static void findJsTags (void)
{
	tokenInfo *const token = newToken ();
	exception_t exception;
	
	ClassNames = stringListNew ();
	FunctionNames = stringListNew ();
	
	exception = (exception_t) (setjmp (Exception));
	while (exception == ExceptionNone)
		parseJsFile (token);

	stringListDelete (ClassNames);
	stringListDelete (FunctionNames);
	ClassNames = NULL;
	FunctionNames = NULL;
	deleteToken (token);
}

/* Create parser definition stucture */
extern parserDefinition* JavaScriptParser (void)
{
	static const char *const extensions [] = { "js", NULL };
	parserDefinition *const def = parserNew ("JavaScript");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kinds		= JsKinds;
	def->kindCount	= KIND_COUNT (JsKinds);
	def->parser		= findJsTags;
	def->initialize = initialize;

	return def;
}
/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
