/*
*   Copyright (c) 2009, Vincent Berthoux
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Objective Caml
*   language files.
*/
/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "keyword.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/* To get rid of unused parameter warning in
 * -Wextra */
#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif
#define OCAML_MAX_STACK_SIZE 256

typedef enum {
	K_CLASS,        /* Ocaml class, relatively rare */
	K_METHOD,       /* class method */
	K_MODULE,       /* Ocaml module OR functor */
	K_VAR,
	K_TYPE,         /* name of an OCaml type */
	K_FUNCTION,
	K_CONSTRUCTOR,  /* Constructor of a sum type */
	K_RECORDFIELD,
	K_EXCEPTION
} ocamlKind;

static kindOption OcamlKinds[] = {
	{TRUE, 'c', "class", "classes"},
	{TRUE, 'm', "method", "Object's method"},
	{TRUE, 'M', "module", "Module or functor"},
	{TRUE, 'v', "var", "Global variable"},
	{TRUE, 't', "type", "Type name"},
	{TRUE, 'f', "function", "A function"},
	{TRUE, 'C', "Constructor", "A constructor"},
	{TRUE, 'r', "Record field", "A 'structure' field"},
	{TRUE, 'e', "Exception", "An exception"}
};

typedef enum {
	OcaKEYWORD_and,
	OcaKEYWORD_begin,
	OcaKEYWORD_class,
	OcaKEYWORD_do,
	OcaKEYWORD_done,
	OcaKEYWORD_else,
	OcaKEYWORD_end,
	OcaKEYWORD_exception,
	OcaKEYWORD_for,
	OcaKEYWORD_functor,
	OcaKEYWORD_fun,
	OcaKEYWORD_if,
	OcaKEYWORD_in,
	OcaKEYWORD_let,
	OcaKEYWORD_match,
	OcaKEYWORD_method,
	OcaKEYWORD_module,
	OcaKEYWORD_mutable,
	OcaKEYWORD_object,
	OcaKEYWORD_of,
	OcaKEYWORD_rec,
	OcaKEYWORD_sig,
	OcaKEYWORD_struct,
	OcaKEYWORD_then,
	OcaKEYWORD_try,
	OcaKEYWORD_type,
	OcaKEYWORD_val,
	OcaKEYWORD_virtual,
	OcaKEYWORD_while,
	OcaKEYWORD_with,

	OcaIDENTIFIER,
	Tok_PARL,       /* '(' */
	Tok_PARR,       /* ')' */
	Tok_BRL,        /* '[' */
	Tok_BRR,        /* ']' */
	Tok_CurlL,      /* '{' */
	Tok_CurlR,      /* '}' */
	Tok_Prime,      /* '\'' */
	Tok_Pipe,       /* '|' */
	Tok_EQ,         /* '=' */
	Tok_Val,        /* string/number/poo */
	Tok_Op,         /* any operator recognized by the language */
	Tok_semi,       /* ';' */
	Tok_comma,      /* ',' */
	Tok_To,         /* '->' */
	Tok_Sharp,      /* '#' */
	Tok_Backslash,  /* '\\' */

	Tok_EOF         /* END of file */
} ocamlKeyword;

typedef struct sOcaKeywordDesc {
	const char *name;
	ocamlKeyword id;
} ocaKeywordDesc;

typedef ocamlKeyword ocaToken;

static const ocaKeywordDesc OcamlKeywordTable[] = {
	{ "and"       , OcaKEYWORD_and       }, 
	{ "begin"     , OcaKEYWORD_begin     }, 
	{ "class"     , OcaKEYWORD_class     }, 
	{ "do"        , OcaKEYWORD_do        }, 
	{ "done"      , OcaKEYWORD_done      }, 
	{ "else"      , OcaKEYWORD_else      }, 
	{ "end"       , OcaKEYWORD_end       }, 
	{ "exception" , OcaKEYWORD_exception }, 
	{ "for"       , OcaKEYWORD_for       }, 
	{ "fun"       , OcaKEYWORD_fun       }, 
	{ "function"  , OcaKEYWORD_fun       }, 
	{ "functor"   , OcaKEYWORD_functor   }, 
	{ "in"        , OcaKEYWORD_in        }, 
	{ "let"       , OcaKEYWORD_let       }, 
	{ "match"     , OcaKEYWORD_match     }, 
	{ "method"    , OcaKEYWORD_method    }, 
	{ "module"    , OcaKEYWORD_module    }, 
	{ "mutable"   , OcaKEYWORD_mutable   }, 
	{ "object"    , OcaKEYWORD_object    }, 
	{ "of"        , OcaKEYWORD_of        }, 
	{ "rec"       , OcaKEYWORD_rec       }, 
	{ "sig"       , OcaKEYWORD_sig       }, 
	{ "struct"    , OcaKEYWORD_struct    }, 
	{ "then"      , OcaKEYWORD_then      }, 
	{ "try"       , OcaKEYWORD_try       }, 
	{ "type"      , OcaKEYWORD_type      }, 
	{ "val"       , OcaKEYWORD_val       }, 
	{ "value"     , OcaKEYWORD_let       }, /* just to handle revised syntax */
	{ "virtual"   , OcaKEYWORD_virtual   }, 
	{ "while"     , OcaKEYWORD_while     }, 
	{ "with"      , OcaKEYWORD_with      }, 

	{ "or"        , Tok_Op               }, 
	{ "mod "      , Tok_Op               }, 
	{ "land "     , Tok_Op               }, 
	{ "lor "      , Tok_Op               }, 
	{ "lxor "     , Tok_Op               }, 
	{ "lsl "      , Tok_Op               }, 
	{ "lsr "      , Tok_Op               }, 
	{ "asr"       , Tok_Op               }, 
	{ "->"        , Tok_To               }, 
	{ "true"      , Tok_Val              }, 
	{ "false"     , Tok_Val              }
};

static langType Lang_Ocaml;

boolean exportLocalInfo = FALSE;

/*//////////////////////////////////////////////////////////////////
//// lexingInit             */
typedef struct _lexingState {
	vString *name;	/* current parsed identifier/operator */
	const unsigned char *cp;	/* position in stream */
} lexingState;

/* array of the size of all possible value for a char */
boolean isOperator[1 << (8 * sizeof (char))] = { FALSE };

static void initKeywordHash ( void )
{
	const size_t count = sizeof (OcamlKeywordTable) / sizeof (ocaKeywordDesc);
	size_t i;

	for (i = 0; i < count; ++i)
	{
		addKeyword (OcamlKeywordTable[i].name, Lang_Ocaml,
			(int) OcamlKeywordTable[i].id);
	}
}

/* definition of all the operator in OCaml,
 * /!\ certain operator get special treatment
 * in regards of their role in OCaml grammar :
 * '|' ':' '=' '~' and '?' */
static void initOperatorTable ( void )
{
	isOperator['!'] = TRUE;
	isOperator['$'] = TRUE;
	isOperator['%'] = TRUE;
	isOperator['&'] = TRUE;
	isOperator['*'] = TRUE;
	isOperator['+'] = TRUE;
	isOperator['-'] = TRUE;
	isOperator['.'] = TRUE;
	isOperator['/'] = TRUE;
	isOperator[':'] = TRUE;
	isOperator['<'] = TRUE;
	isOperator['='] = TRUE;
	isOperator['>'] = TRUE;
	isOperator['?'] = TRUE;
	isOperator['@'] = TRUE;
	isOperator['^'] = TRUE;
	isOperator['~'] = TRUE;
	isOperator['|'] = TRUE;
}

/*//////////////////////////////////////////////////////////////////////
//// Lexing                                     */
static boolean isNum (char c)
{
	return c >= '0' && c <= '9';
}
static boolean isLowerAlpha (char c)
{
	return c >= 'a' && c <= 'z';
}

static boolean isUpperAlpha (char c)
{
	return c >= 'A' && c <= 'Z';
}

static boolean isAlpha (char c)
{
	return isLowerAlpha (c) || isUpperAlpha (c);
}

static boolean isIdent (char c)
{
	return isNum (c) || isAlpha (c) || c == '_' || c == '\'';
}

static boolean isSpace (char c)
{
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static void eatWhiteSpace (lexingState * st)
{
	const unsigned char *cp = st->cp;
	while (isSpace (*cp))
		cp++;

	st->cp = cp;
}

static void eatString (lexingState * st)
{
	boolean lastIsBackSlash = FALSE;
	boolean unfinished = TRUE;
	const unsigned char *c = st->cp + 1;

	while (unfinished)
	{
		/* end of line should never happen.
		 * we tolerate it */
		if (c == NULL || c[0] == '\0')
			break;
		else if (*c == '"' && !lastIsBackSlash)
			unfinished = FALSE;
		else
			lastIsBackSlash = *c == '\\';

		c++;
	}

	st->cp = c;
}

static void eatComment (lexingState * st)
{
	boolean unfinished = TRUE;
	boolean lastIsStar = FALSE;
	const unsigned char *c = st->cp + 2;

	while (unfinished)
	{
		/* we've reached the end of the line..
		 * so we have to reload a line... */
		if (c == NULL || *c == '\0')
		{
			st->cp = fileReadLine ();
			/* WOOPS... no more input...
			 * we return, next lexing read
			 * will be null and ok */
			if (st->cp == NULL)
				return;
			c = st->cp;
			continue;
		}
		/* we've reached the end of the comment */
		else if (*c == ')' && lastIsStar)
			unfinished = FALSE;
		/* here we deal with imbricated comment, which
		 * are allowed in OCaml */
		else if (c[0] == '(' && c[1] == '*')
		{
			st->cp = c;
			eatComment (st);
			c = st->cp;
			lastIsStar = FALSE;
		}
		else
			lastIsStar = '*' == *c;

		c++;
	}

	st->cp = c;
}

static void readIdentifier (lexingState * st)
{
	const unsigned char *p;
	vStringClear (st->name);

	/* first char is a simple letter */
	if (isAlpha (*st->cp) || *st->cp == '_')
		vStringPut (st->name, (int) *st->cp);

	/* Go till you get identifier chars */
	for (p = st->cp + 1; isIdent (*p); p++)
		vStringPut (st->name, (int) *p);

	st->cp = p;

	vStringTerminate (st->name);
}

static ocamlKeyword eatNumber (lexingState * st)
{
	while (isNum (*st->cp))
		st->cp++;
	return Tok_Val;
}

/* Operator can be defined in OCaml as a function
 * so we must be ample enough to parse them normally */
static ocamlKeyword eatOperator (lexingState * st)
{
	int count = 0;
	const unsigned char *root = st->cp;

	vStringClear (st->name);

	while (isOperator[st->cp[count]])
	{
		vStringPut (st->name, st->cp[count]);
		count++;
	}

	vStringTerminate (st->name);

	st->cp += count;
	if (count <= 1)
	{
		switch (root[0])
		{
		case '|':
			return Tok_Pipe;
		case '=':
			return Tok_EQ;
		default:
			return Tok_Op;
		}
	}
	else if (count == 2 && root[0] == '-' && root[1] == '>')
		return Tok_To;
	else
		return Tok_Op;
}

/* The lexer is in charge of reading the file.
 * Some of sub-lexer (like eatComment) also read file.
 * lexing is finished when the lexer return Tok_EOF */
static ocamlKeyword lex (lexingState * st)
{
	int retType;
	/* handling data input here */
	while (st->cp == NULL || st->cp[0] == '\0')
	{
		st->cp = fileReadLine ();
		if (st->cp == NULL)
			return Tok_EOF;
	}

	if (isAlpha (*st->cp))
	{
		readIdentifier (st);
		retType = lookupKeyword (vStringValue (st->name), Lang_Ocaml);

		if (retType == -1)	/* If it's not a keyword */
		{
			return OcaIDENTIFIER;
		}
		else
		{
			return retType;
		}
	}
	else if (isNum (*st->cp))
		return eatNumber (st);
	else if (isSpace (*st->cp))
	{
		eatWhiteSpace (st);
		return lex (st);
	}
	/* OCaml permit the definition of our own operators
	 * so here we check all the consecuting chars which
	 * are operators to discard them. */
	else if (isOperator[*st->cp])
		return eatOperator (st);
	else
		switch (*st->cp)
		{
		case '(':
			if (st->cp[1] == '*')	/* ergl, a comment */
			{
				eatComment (st);
				return lex (st);
			}
			else
			{
				st->cp++;
				return Tok_PARL;
			}

		case ')':
			st->cp++;
			return Tok_PARR;
		case '[':
			st->cp++;
			return Tok_BRL;
		case ']':
			st->cp++;
			return Tok_BRR;
		case '{':
			st->cp++;
			return Tok_CurlL;
		case '}':
			st->cp++;
			return Tok_CurlR;
		case '\'':
			st->cp++;
			return Tok_Prime;
		case ',':
			st->cp++;
			return Tok_comma;
		case '=':
			st->cp++;
			return Tok_EQ;
		case ';':
			st->cp++;
			return Tok_semi;
		case '"':
			eatString (st);
			return Tok_Val;
		case '_':
			st->cp++;
			return Tok_Val;
		case '#':
			st->cp++;
			return Tok_Sharp;
		case '\\':
			st->cp++;
			return Tok_Backslash;

		default:
			st->cp++;
			break;
		}

	/* default return if nothing is recognized,
	 * shouldn't happen, but at least, it will
	 * be handled without destroying the parsing. */
	return Tok_Val;
}

/*//////////////////////////////////////////////////////////////////////
//// Parsing                                    */
typedef void (*parseNext) (vString * const ident, ocaToken what);

/********** Helpers */
/* This variable hold the 'parser' which is going to
 * handle the next token */
parseNext toDoNext;

/* Special variable used by parser eater to
 * determine which action to put after their
 * job is finished. */
parseNext comeAfter;

/* If a token put an end to current delcaration/
 * statement */
ocaToken terminatingToken;

/* Token to be searched by the different
 * parser eater. */
ocaToken waitedToken;

/* name of the last class, used for
 * context stacking. */
vString *lastClass;

vString *voidName;

typedef enum _sContextKind {
	ContextStrong,
	ContextSoft
} contextKind;

typedef enum _sContextType {
	ContextType,
	ContextModule,
	ContextClass,
	ContextValue,
	ContextFunction,
	ContextMethod,
	ContextBlock
} contextType;

typedef struct _sOcamlContext {
	contextKind kind;	/* well if the context is strong or not */
	contextType type;
	parseNext callback;	/* what to do when a context is pop'd */
	vString *contextName;	/* name, if any, of the surrounding context */
} ocamlContext;

/* context stack, can be used to output scope information
 * into the tag file. */
ocamlContext stack[OCAML_MAX_STACK_SIZE];
/* current position in the tag */
int stackIndex;

/* special function, often recalled, so putting it here */
static void globalScope (vString * const ident, ocaToken what);

/* Return : index of the last named context if one
 *          is found, -1 otherwise */
static int getLastNamedIndex ( void )
{
	int i;

	for (i = stackIndex - 1; i >= 0; --i)
	{
		if (stack[i].contextName->buffer &&
			strlen (stack[i].contextName->buffer) > 0)
		{
			return i;
		}
	}

	return -1;
}

static const char *contextDescription (contextType t)
{
	switch (t)
	{
	case ContextFunction:
		return "function";
	case ContextMethod:
		return "method";
	case ContextValue:
		return "value";
	case ContextModule:
		return "Module";
	case ContextType:
		return "type";
	case ContextClass:
		return "class";
	case ContextBlock:
		return "begin/end";
	}

	return NULL;
}

static char contextTypeSuffix (contextType t)
{
	switch (t)
	{
	case ContextFunction:
	case ContextMethod:
	case ContextValue:
	case ContextModule:
		return '/';
	case ContextType:
		return '.';
	case ContextClass:
		return '#';
	case ContextBlock:
		return ' ';
	}

	return '$';
}

/* Push a new context, handle null string */
static void pushContext (contextKind kind, contextType type, parseNext after,
        vString const *contextName)
{
	int parentIndex;

	if (stackIndex >= OCAML_MAX_STACK_SIZE)
	{
		verbose ("OCaml Maximum depth reached");
		return;
	}


	stack[stackIndex].kind = kind;
	stack[stackIndex].type = type;
	stack[stackIndex].callback = after;

	parentIndex = getLastNamedIndex ();
	if (contextName == NULL)
	{
		vStringClear (stack[stackIndex++].contextName);
		return;
	}

	if (parentIndex >= 0)
	{
		vStringCopy (stack[stackIndex].contextName,
			stack[parentIndex].contextName);
		vStringPut (stack[stackIndex].contextName,
			contextTypeSuffix (stack[parentIndex].type));

		vStringCat (stack[stackIndex].contextName, contextName);
	}
	else
		vStringCopy (stack[stackIndex].contextName, contextName);

	stackIndex++;
}

static void pushStrongContext (vString * name, contextType type)
{
	pushContext (ContextStrong, type, &globalScope, name);
}

static void pushSoftContext (parseNext continuation,
	vString * name, contextType type)
{
	pushContext (ContextSoft, type, continuation, name);
}

static void pushEmptyContext (parseNext continuation)
{
	pushContext (ContextSoft, ContextValue, continuation, NULL);
}

/* unroll the stack until the last named context.
 * then discard it. Used to handle the :
 * let f x y = ...
 * in ...
 * where the context is reseted after the in. Context may have
 * been really nested before that. */
static void popLastNamed ( void )
{
	int i = getLastNamedIndex ();

	if (i >= 0)
	{
		stackIndex = i;
		toDoNext = stack[i].callback;
		vStringClear (stack[i].contextName);
	}
	else
	{
		/* ok, no named context found... 
		 * (should not happen). */
		stackIndex = 0;
		toDoNext = &globalScope;
	}
}

/* pop a context without regarding it's content
 * (beside handling empty stack case) */
static void popSoftContext ( void )
{
	if (stackIndex <= 0)
	{
		toDoNext = &globalScope;
	}
	else
	{
		stackIndex--;
		toDoNext = stack[stackIndex].callback;
		vStringClear (stack[stackIndex].contextName);
	}
}

/* Reset everything until the last global space.
 * a strong context can be :
 * - module
 * - class definition
 * - the initial global space
 * - a _global_ delcaration (let at global scope or in a module).
 * Created to exit quickly deeply nested context */
static contextType popStrongContext ( void )
{
	int i;

	for (i = stackIndex - 1; i >= 0; --i)
	{
		if (stack[i].kind == ContextStrong)
		{
			stackIndex = i;
			toDoNext = stack[i].callback;
			vStringClear (stack[i].contextName);
			return stack[i].type;
		}
	}
	/* ok, no strong context found... */
	stackIndex = 0;
	toDoNext = &globalScope;
	return -1;
}

/* Ignore everything till waitedToken and jump to comeAfter.
 * If the "end" keyword is encountered break, doesn't remember
 * why though. */
static void tillToken (vString * const UNUSED (ident), ocaToken what)
{
	if (what == waitedToken)
		toDoNext = comeAfter;
	else if (what == OcaKEYWORD_end)
	{
		popStrongContext ();
		toDoNext = &globalScope;
	}
}

/* Ignore everything till a waitedToken is seen, but
 * take care of balanced parentheses/bracket use */
static void contextualTillToken (vString * const UNUSED (ident), ocaToken what)
{
	static int parentheses = 0;
	static int bracket = 0;
	static int curly = 0;

	switch (what)
	{
	case Tok_PARL:
		parentheses--;
		break;
	case Tok_PARR:
		parentheses++;
		break;
	case Tok_CurlL:
		curly--;
		break;
	case Tok_CurlR:
		curly++;
		break;
	case Tok_BRL:
		bracket--;
		break;
	case Tok_BRR:
		bracket++;
		break;

	default:	/* other token are ignored */
		break;
	}

	if (what == waitedToken && parentheses == 0 && bracket == 0 && curly == 0)
		toDoNext = comeAfter;

	else if (what == OcaKEYWORD_end)
	{
		popStrongContext ();
		toDoNext = &globalScope;
	}
}

/* Wait for waitedToken and jump to comeAfter or let
 * the globalScope handle declarations */
static void tillTokenOrFallback (vString * const ident, ocaToken what)
{
	if (what == waitedToken)
		toDoNext = comeAfter;
	else
		globalScope (ident, what);
}

/* ignore token till waitedToken, or give up if find
 * terminatingToken. Use globalScope to handle new
 * declarations. */
static void tillTokenOrTerminatingOrFallback (vString * const ident,
	ocaToken what)
{
	if (what == waitedToken)
		toDoNext = comeAfter;
	else if (what == terminatingToken)
		toDoNext = globalScope;
	else
		globalScope (ident, what);
}

/* ignore the next token in the stream and jump to the
 * given comeAfter state */
static void ignoreToken (vString * const UNUSED (ident), ocaToken UNUSED (what))
{
	toDoNext = comeAfter;
}

/********** Grammar */
/* the purpose of each function is detailled near their
 * implementation */

static void killCurrentState ( void )
{

	/* Tracking the kind of previous strong
	 * context, if it doesn't match with a
	 * really strong entity, repop */
	switch (popStrongContext ())
	{

	case ContextValue:
		popStrongContext ();
		break;
	case ContextFunction:
		popStrongContext ();
		break;
	case ContextMethod:
		popStrongContext ();
		break;

	case ContextType:
		popStrongContext();
		break;
	case ContextBlock:
		break;
	case ContextModule:
		break;
	case ContextClass:
		break;
	default:
		/* nothing more */
		break;
	}
}

/* used to prepare tag for OCaml, just in case their is a need to
 * add additional information to the tag. */
static void prepareTag (tagEntryInfo * tag, vString const *name, ocamlKind kind)
{
	int parentIndex;

	initTagEntry (tag, vStringValue (name));
	tag->kindName = OcamlKinds[kind].name;
	tag->kind = OcamlKinds[kind].letter;

	parentIndex = getLastNamedIndex ();
	if (parentIndex >= 0)
	{
		tag->extensionFields.scope[0] =
			contextDescription (stack[parentIndex].type);
		tag->extensionFields.scope[1] =
			vStringValue (stack[parentIndex].contextName);
	}
}

/* Used to centralise tag creation, and be able to add
 * more information to it in the future */
static void addTag (vString * const ident, int kind)
{
	tagEntryInfo toCreate;
	prepareTag (&toCreate, ident, kind);
	makeTagEntry (&toCreate);
}

boolean needStrongPoping = FALSE;
static void requestStrongPoping ( void )
{
	needStrongPoping = TRUE;
}

static void cleanupPreviousParser ( void )
{
	if (needStrongPoping)
	{
		needStrongPoping = FALSE;
		popStrongContext ();
	}
}

/* Due to some circular dependencies, the following functions
 * must be forward-declared. */
static void letParam (vString * const ident, ocaToken what);
static void localScope (vString * const ident, ocaToken what);
static void mayRedeclare (vString * const ident, ocaToken what);
static void typeSpecification (vString * const ident, ocaToken what);

/*
 * Parse a record type
 * type ident = // parsed previously
 *  {
 *      ident1: type1;
 *      ident2: type2;
 *  }
 */
static void typeRecord (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case OcaIDENTIFIER:
		addTag (ident, K_RECORDFIELD);
		terminatingToken = Tok_CurlR;
		waitedToken = Tok_semi;
		comeAfter = &typeRecord;
		toDoNext = &tillTokenOrTerminatingOrFallback;
		break;

	case OcaKEYWORD_mutable:
		/* ignore it */
		break;

	case Tok_CurlR:
		popStrongContext ();
		toDoNext = &globalScope;
		break;

	default:	/* don't care */
		break;
	}
}

/* handle :
 * exception ExceptionName ... */
static void exceptionDecl (vString * const ident, ocaToken what)
{
	if (what == OcaIDENTIFIER)
	{
		addTag (ident, K_EXCEPTION);
	}
	/* don't know what to do on else... */

	toDoNext = &globalScope;
}

tagEntryInfo tempTag;
vString *tempIdent;

/* Ensure a constructor is not a type path beginning
 * with a module */
static void constructorValidation (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_Op:	/* if we got a '.' which is an operator */
		toDoNext = &globalScope;
		popStrongContext ();
		needStrongPoping = FALSE;
		break;

	case OcaKEYWORD_of:	/* OK, it must be a constructor :) */
		makeTagEntry (&tempTag);
		vStringClear (tempIdent);
		toDoNext = &tillTokenOrFallback;
		comeAfter = &typeSpecification;
		waitedToken = Tok_Pipe;
		break;

	case Tok_Pipe:	/* OK, it was a constructor :)  */
		makeTagEntry (&tempTag);
		vStringClear (tempIdent);
		toDoNext = &typeSpecification;
		break;

	default:	/* and mean that we're not facing a module name */
		makeTagEntry (&tempTag);
		vStringClear (tempIdent);
		toDoNext = &tillTokenOrFallback;
		comeAfter = &typeSpecification;
		waitedToken = Tok_Pipe;

		/* nothing in the context, discard it */
		popStrongContext ();

		/* to be sure we use this token */
		globalScope (ident, what);
	}
}


/* Parse beginning of type definition
 * type 'avar ident =
 * or
 * type ('var1, 'var2) ident =
 */
static void typeDecl (vString * const ident, ocaToken what)
{

	switch (what)
	{
		/* parameterized */
	case Tok_Prime:
		comeAfter = &typeDecl;
		toDoNext = &ignoreToken;
		break;
		/* LOTS of parameters */
	case Tok_PARL:
		comeAfter = &typeDecl;
		waitedToken = Tok_PARR;
		toDoNext = &tillToken;
		break;

	case OcaIDENTIFIER:
		addTag (ident, K_TYPE);
		pushStrongContext (ident, ContextType);
		requestStrongPoping ();
		waitedToken = Tok_EQ;
		comeAfter = &typeSpecification;
		toDoNext = &tillTokenOrFallback;
		break;

	default:
		globalScope (ident, what);
	}
}

/* Parse type of kind
 * type bidule = Ctor1 of ...
 *             | Ctor2
 *             | Ctor3 of ...
 * or
 * type bidule = | Ctor1 of ... | Ctor2
 *
 * when type bidule = { ... } is detected,
 * let typeRecord handle it. */
static void typeSpecification (vString * const ident, ocaToken what)
{

	switch (what)
	{
	case OcaIDENTIFIER:
		if (isUpperAlpha (ident->buffer[0]))
		{
			/* here we handle type aliases of type
			 * type foo = AnotherModule.bar
			 * AnotherModule can mistakenly be took
			 * for a constructor. */
			vStringCopy (tempIdent, ident);
			prepareTag (&tempTag, tempIdent, K_CONSTRUCTOR);
			toDoNext = &constructorValidation;
		}
		else
		{
			toDoNext = &tillTokenOrFallback;
			comeAfter = &typeSpecification;
			waitedToken = Tok_Pipe;
		}
		break;

	case OcaKEYWORD_and:
		toDoNext = &typeDecl;
		break;

	case Tok_BRL:	/* the '[' & ']' are ignored to accommodate */
	case Tok_BRR:	/* with the revised syntax */
	case Tok_Pipe:
		/* just ignore it */
		break;

	case Tok_CurlL:
		toDoNext = &typeRecord;
		break;

	default:	/* don't care */
		break;
	}
}


static boolean dirtySpecialParam = FALSE;


/* parse the ~label and ~label:type parameter */
static void parseLabel (vString * const ident, ocaToken what)
{
	static int parCount = 0;

	switch (what)
	{
	case OcaIDENTIFIER:
		if (!dirtySpecialParam)
		{

			if (exportLocalInfo)
				addTag (ident, K_VAR);

			dirtySpecialParam = TRUE;
		}
		break;

	case Tok_PARL:
		parCount++;
		break;

	case Tok_PARR:
		parCount--;
		if (parCount == 0)
			toDoNext = &letParam;
		break;

	case Tok_Op:
		if (ident->buffer[0] == ':')
		{
			toDoNext = &ignoreToken;
			comeAfter = &letParam;
		}
		else if (parCount == 0 && dirtySpecialParam)
		{
			toDoNext = &letParam;
			letParam (ident, what);
		}
		break;

	default:
		if (parCount == 0 && dirtySpecialParam)
		{
			toDoNext = &letParam;
			letParam (ident, what);
		}
		break;
	}
}


/* Optional argument with syntax like this :
 * ?(foo = value) */
static void parseOptionnal (vString * const ident, ocaToken what)
{
	static int parCount = 0;


	switch (what)
	{
	case OcaIDENTIFIER:
		if (!dirtySpecialParam)
		{
			if (exportLocalInfo)
				addTag (ident, K_VAR);

			dirtySpecialParam = TRUE;

			if (parCount == 0)
				toDoNext = &letParam;
		}
		break;

	case Tok_PARL:
		parCount++;
		break;

	case Tok_PARR:
		parCount--;
		if (parCount == 0)
			toDoNext = &letParam;
		break;

	default:	/* don't care */
		break;
	}
}


/** handle let inside functions (so like it's name
 * say : local let */
static void localLet (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_PARL:
		/* We ignore this token to be able to parse such
		 * declarations :
		 * let (ident : type) = ...
		 */
		break;

	case OcaKEYWORD_rec:
		/* just ignore to be able to parse such declarations:
		 * let rec ident = ... */
		break;

	case Tok_Op:
		/* we are defining a new operator, it's a
		 * function definition */
		if (exportLocalInfo)
			addTag (ident, K_FUNCTION);

		pushSoftContext (mayRedeclare, ident, ContextFunction);
		toDoNext = &letParam;
		break;

		/* Can be a weiiird binding, or an '_' */
	case Tok_Val:
		if (exportLocalInfo)
			addTag (ident, K_VAR);
		pushSoftContext (mayRedeclare, ident, ContextValue);
		toDoNext = &letParam;
		break;

	case OcaIDENTIFIER:
		if (exportLocalInfo)
			addTag (ident, K_VAR);
		pushSoftContext (mayRedeclare, ident, ContextValue);
		toDoNext = &letParam;
		break;

	case OcaKEYWORD_end:
		popStrongContext ();
		break;

	default:
		toDoNext = &localScope;
		break;
	}
}

/* parse :
 * | pattern pattern -> ...
 * or
 * pattern apttern apttern -> ...
 * we ignore all identifiers declared in the pattern,
 * because their scope is likely to be even more limited
 * than the let definitions.
 * Used after a match ... with, or a function ... or fun ...
 * because their syntax is similar.  */
static void matchPattern (vString * const UNUSED (ident), ocaToken what)
{
	switch (what)
	{
	case Tok_To:
		pushEmptyContext (&matchPattern);
		toDoNext = &mayRedeclare;
		break;


	case OcaKEYWORD_in:
		popLastNamed ();
		break;

	default:
		break;
	}
}

/* Used at the beginning of a new scope (begin of a
 * definition, parenthesis...) to catch inner let
 * definition that may be in. */
static void mayRedeclare (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case OcaKEYWORD_let:
	case OcaKEYWORD_val:
		toDoNext = localLet;
		break;

	case OcaKEYWORD_object:
		vStringClear (lastClass);
		pushContext (ContextStrong, ContextClass,
			&localScope, NULL /*voidName */ );
		needStrongPoping = FALSE;
		toDoNext = &globalScope;
		break;

	case OcaKEYWORD_for:
	case OcaKEYWORD_while:
		toDoNext = &tillToken;
		waitedToken = OcaKEYWORD_do;
		comeAfter = &mayRedeclare;
		break;

	case OcaKEYWORD_try:
		toDoNext = &mayRedeclare;
		pushSoftContext (matchPattern, ident, ContextFunction);
		break;

	case OcaKEYWORD_fun:
		toDoNext = &matchPattern;
		break;

		/* Handle the special ;; from the OCaml
		 * Top level */
	case Tok_semi:
	default:
		toDoNext = &localScope;
		localScope (ident, what);
	}
}

/* parse :
 * p1 p2 ... pn = ...
 * or
 * ?(p1=v) p2 ~p3 ~pn:ja ... = ... */
static void letParam (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_EQ:
		toDoNext = &mayRedeclare;
		break;

	case OcaIDENTIFIER:
		if (exportLocalInfo)
			addTag (ident, K_VAR);
		break;

	case Tok_Op:
		switch (ident->buffer[0])
		{
		case ':':
			/*popSoftContext(); */
			/* we got a type signature */
			comeAfter = &mayRedeclare;
			toDoNext = &tillTokenOrFallback;
			waitedToken = Tok_EQ;
			break;

			/* parse something like
			 * ~varname:type 
			 * or
			 * ~varname
			 * or
			 * ~(varname: long type) */
		case '~':
			toDoNext = &parseLabel;
			dirtySpecialParam = FALSE;
			break;

			/* Optional argument with syntax like this :
			 * ?(bla = value) 
			 * or
			 * ?bla */
		case '?':
			toDoNext = &parseOptionnal;
			dirtySpecialParam = FALSE;
			break;

		default:
			break;
		}
		break;

	default:	/* don't care */
		break;
	}
}


/* parse object ...
 * used to be sure the class definition is not a type
 * alias */
static void classSpecif (vString * const UNUSED (ident), ocaToken what)
{
	switch (what)
	{
	case OcaKEYWORD_object:
		pushStrongContext (lastClass, ContextClass);
		toDoNext = &globalScope;
		break;

	default:
		vStringClear (lastClass);
		toDoNext = &globalScope;
	}
}

/* Handle a method ... class declaration.
 * nearly a copy/paste of globalLet. */
static void methodDecl (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_PARL:
		/* We ignore this token to be able to parse such
		 * declarations :
		 * let (ident : type) = ...  */
		break;

	case OcaKEYWORD_mutable:
	case OcaKEYWORD_virtual:
	case OcaKEYWORD_rec:
		/* just ignore to be able to parse such declarations:
		 * let rec ident = ... */
		break;

	case OcaIDENTIFIER:
		addTag (ident, K_METHOD);
		/* Normal pushing to get good subs */
		pushStrongContext (ident, ContextMethod);
		/*pushSoftContext( globalScope, ident, ContextMethod ); */
		toDoNext = &letParam;
		break;

	case OcaKEYWORD_end:
		popStrongContext ();
		break;

	default:
		toDoNext = &globalScope;
		break;
	}
}

/* name of the last module, used for
 * context stacking. */
vString *lastModule;


/* parse
 * ... struct (* new global scope *) end
 * or
 * ... sig (* new global scope *) end
 * or
 * functor ... -> moduleSpecif
 */
static void moduleSpecif (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case OcaKEYWORD_functor:
		toDoNext = &contextualTillToken;
		waitedToken = Tok_To;
		comeAfter = &moduleSpecif;
		break;

	case OcaKEYWORD_struct:
	case OcaKEYWORD_sig:
		pushStrongContext (lastModule, ContextModule);
		toDoNext = &globalScope;
		break;

	case Tok_PARL:	/* ( */
		toDoNext = &contextualTillToken;
		comeAfter = &globalScope;
		waitedToken = Tok_PARR;
		contextualTillToken (ident, what);
		break;

	default:
		vStringClear (lastModule);
		toDoNext = &globalScope;
	}
}

/* parse :
 * module name = ...
 * then pass the token stream to moduleSpecif */
static void moduleDecl (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case OcaKEYWORD_type:
		/* just ignore it, name come after */
		break;

	case OcaIDENTIFIER:
		addTag (ident, K_MODULE);
		vStringCopy (lastModule, ident);
		waitedToken = Tok_EQ;
		comeAfter = &moduleSpecif;
		toDoNext = &contextualTillToken;
		break;

	default:	/* don't care */
		break;
	}
}

/* parse :
 * class name = ...
 * or
 * class virtual ['a,'b] classname = ... */
static void classDecl (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case OcaIDENTIFIER:
		addTag (ident, K_CLASS);
		vStringCopy (lastClass, ident);
		toDoNext = &contextualTillToken;
		waitedToken = Tok_EQ;
		comeAfter = &classSpecif;
		break;

	case Tok_BRL:
		toDoNext = &tillToken;
		waitedToken = Tok_BRR;
		comeAfter = &classDecl;
		break;

	default:
		break;
	}
}

/* Handle a global
 * let ident ...
 * or
 * let rec ident ... */
static void globalLet (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_PARL:
		/* We ignore this token to be able to parse such
		 * declarations :
		 * let (ident : type) = ...
		 */
		break;

	case OcaKEYWORD_mutable:
	case OcaKEYWORD_virtual:
	case OcaKEYWORD_rec:
		/* just ignore to be able to parse such declarations:
		 * let rec ident = ... */
		break;

	case Tok_Op:
		/* we are defining a new operator, it's a
		 * function definition */
		addTag (ident, K_FUNCTION);
		pushStrongContext (ident, ContextFunction);
		toDoNext = &letParam;
		break;

	case OcaIDENTIFIER:
		addTag (ident, K_VAR);
		pushStrongContext (ident, ContextValue);
		requestStrongPoping ();
		toDoNext = &letParam;
		break;

	case OcaKEYWORD_end:
		popStrongContext ();
		break;

	default:
		toDoNext = &globalScope;
		break;
	}
}

/* Handle the "strong" top levels, all 'big' declarations
 * happen here */
static void globalScope (vString * const UNUSED (ident), ocaToken what)
{
	/* Do not touch, this is used only by the global scope
	 * to handle an 'and' */
	static parseNext previousParser = NULL;

	switch (what)
	{
	case OcaKEYWORD_and:
		cleanupPreviousParser ();
		toDoNext = previousParser;
		break;

	case OcaKEYWORD_type:
		cleanupPreviousParser ();
		toDoNext = &typeDecl;
		previousParser = &typeDecl;
		break;

	case OcaKEYWORD_class:
		cleanupPreviousParser ();
		toDoNext = &classDecl;
		previousParser = &classDecl;
		break;

	case OcaKEYWORD_module:
		cleanupPreviousParser ();
		toDoNext = &moduleDecl;
		previousParser = &moduleDecl;
		break;

	case OcaKEYWORD_end:
		needStrongPoping = FALSE;
		killCurrentState ();
		/*popStrongContext(); */
		break;

	case OcaKEYWORD_method:
		cleanupPreviousParser ();
		toDoNext = &methodDecl;
		/* and is not allowed in methods */
		break;

		/* val is mixed with let as global
		 * to be able to handle mli & new syntax */
	case OcaKEYWORD_val:
	case OcaKEYWORD_let:
		cleanupPreviousParser ();
		toDoNext = &globalLet;
		previousParser = &globalLet;
		break;

	case OcaKEYWORD_exception:
		cleanupPreviousParser ();
		toDoNext = &exceptionDecl;
		previousParser = NULL;
		break;

		/* must be a #line directive, discard the
		 * whole line. */
	case Tok_Sharp:
		/* ignore */
		break;

	default:
		/* we don't care */
		break;
	}
}

/* Parse expression. Well ignore it is more the case,
 * ignore all tokens except "shocking" keywords */
static void localScope (vString * const ident, ocaToken what)
{
	switch (what)
	{
	case Tok_Pipe:
	case Tok_PARR:
	case Tok_BRR:
	case Tok_CurlR:
		popSoftContext ();
		break;

		/* Everything that `begin` has an `end`
		 * as end is overloaded and signal many end
		 * of things, we add an empty strong context to
		 * avoid problem with the end.
		 */
	case OcaKEYWORD_begin:
		pushContext (ContextStrong, ContextBlock, &mayRedeclare, NULL);
		toDoNext = &mayRedeclare;
		break;

	case OcaKEYWORD_in:
		popLastNamed ();
		break;

		/* Ok, we got a '{', which is much likely to create
		 * a record. We cannot treat it like other [ && (,
		 * because it may contain the 'with' keyword and screw
		 * everything else. */
	case Tok_CurlL:
		toDoNext = &contextualTillToken;
		waitedToken = Tok_CurlR;
		comeAfter = &localScope;
		contextualTillToken (ident, what);
		break;

		/* Yeah imperative feature of OCaml,
		 * a ';' like in C */
	case Tok_semi:
		toDoNext = &mayRedeclare;
		break;

	case Tok_PARL:
	case Tok_BRL:
		pushEmptyContext (&localScope);
		toDoNext = &mayRedeclare;
		break;

	case OcaKEYWORD_and:
		popLastNamed ();
		toDoNext = &localLet;
		break;

	case OcaKEYWORD_else:
	case OcaKEYWORD_then:
		popSoftContext ();
		pushEmptyContext (&localScope);
		toDoNext = &mayRedeclare;
		break;

	case OcaKEYWORD_if:
		pushEmptyContext (&localScope);
		toDoNext = &mayRedeclare;
		break;

	case OcaKEYWORD_match:
		pushEmptyContext (&localScope);
		toDoNext = &mayRedeclare;
		break;

	case OcaKEYWORD_with:
		popSoftContext ();
		toDoNext = &matchPattern;
		pushEmptyContext (&matchPattern);
		break;

	case OcaKEYWORD_end:
		killCurrentState ();
		break;


	case OcaKEYWORD_fun:
		comeAfter = &mayRedeclare;
		toDoNext = &tillToken;
		waitedToken = Tok_To;
		break;

	case OcaKEYWORD_done:
	case OcaKEYWORD_val:
		/* doesn't care */
		break;

	default:
		requestStrongPoping ();
		globalScope (ident, what);
		break;
	}
}

/*////////////////////////////////////////////////////////////////
//// Deal with the system                                       */
/* in OCaml the file name is the module name used in the language
 * with it first letter put in upper case */
static void computeModuleName ( void )
{
	/* in Ocaml the file name define a module.
	 * so we define a module =)
	 */
	const char *filename = getSourceFileName ();
	int beginIndex = 0;
	int endIndex = strlen (filename) - 1;
	vString *moduleName = vStringNew ();

	while (filename[endIndex] != '.' && endIndex > 0)
		endIndex--;

	/* avoid problem with path in front of filename */
	beginIndex = endIndex;
	while (beginIndex > 0)
	{
		if (filename[beginIndex] == '\\' || filename[beginIndex] == '/')
		{
			beginIndex++;
			break;
		}

		beginIndex--;
	}

	vStringNCopyS (moduleName, &filename[beginIndex], endIndex - beginIndex);
	vStringTerminate (moduleName);

	if (isLowerAlpha (moduleName->buffer[0]))
		moduleName->buffer[0] += ('A' - 'a');

	makeSimpleTag (moduleName, OcamlKinds, K_MODULE);
	vStringDelete (moduleName);
}

/* Allocate all string of the context stack */
static void initStack ( void )
{
	int i;
	for (i = 0; i < OCAML_MAX_STACK_SIZE; ++i)
		stack[i].contextName = vStringNew ();
}

static void clearStack ( void )
{
	int i;
	for (i = 0; i < OCAML_MAX_STACK_SIZE; ++i)
		vStringDelete (stack[i].contextName);
}

static void findOcamlTags (void)
{
	vString *name = vStringNew ();
	lexingState st;
	ocaToken tok;

	computeModuleName ();
	initStack ();
	tempIdent = vStringNew ();
	lastModule = vStringNew ();
	lastClass = vStringNew ();
	voidName = vStringNew ();
	vStringCopyS (voidName, "_");

	st.name = vStringNew ();
	st.cp = fileReadLine ();
	toDoNext = &globalScope;
	tok = lex (&st);
	while (tok != Tok_EOF)
	{
		(*toDoNext) (st.name, tok);
		tok = lex (&st);
	}

	vStringDelete (name);
	vStringDelete (voidName);
	vStringDelete (tempIdent);
	vStringDelete (lastModule);
	vStringDelete (lastClass);
	clearStack ();
}

static void ocamlInitialize (const langType language)
{
	Lang_Ocaml = language;

	initOperatorTable ();
	initKeywordHash ();
}

extern parserDefinition *OcamlParser (void)
{
	static const char *const extensions[] = { "ml", "mli", NULL };
	parserDefinition *def = parserNew ("OCaml");
	def->kinds = OcamlKinds;
	def->kindCount = KIND_COUNT (OcamlKinds);
	def->extensions = extensions;
	def->parser = findOcamlTags;
	def->initialize = ocamlInitialize;

	return def;
}
