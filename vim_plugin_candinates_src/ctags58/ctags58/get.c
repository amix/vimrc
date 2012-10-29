/*
*   $Id: get.c 559 2007-06-17 03:30:09Z elliotth $
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains the high level source read functions (preprocessor
*   directives are handled within this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "get.h"
#include "options.h"
#include "read.h"
#include "vstring.h"

/*
*   MACROS
*/
#define stringMatch(s1,s2)		(strcmp (s1,s2) == 0)
#define isspacetab(c)			((c) == SPACE || (c) == TAB)

/*
*   DATA DECLARATIONS
*/
typedef enum { COMMENT_NONE, COMMENT_C, COMMENT_CPLUS } Comment;

enum eCppLimits {
	MaxCppNestingLevel = 20,
	MaxDirectiveName = 10
};

/*  Defines the one nesting level of a preprocessor conditional.
 */
typedef struct sConditionalInfo {
	boolean ignoreAllBranches;  /* ignoring parent conditional branch */
	boolean singleBranch;       /* choose only one branch */
	boolean branchChosen;       /* branch already selected */
	boolean ignoring;           /* current ignore state */
} conditionalInfo;

enum eState {
	DRCTV_NONE,    /* no known directive - ignore to end of line */
	DRCTV_DEFINE,  /* "#define" encountered */
	DRCTV_HASH,    /* initial '#' read; determine directive */
	DRCTV_IF,      /* "#if" or "#ifdef" encountered */
	DRCTV_PRAGMA,  /* #pragma encountered */
	DRCTV_UNDEF    /* "#undef" encountered */
};

/*  Defines the current state of the pre-processor.
 */
typedef struct sCppState {
	int		ungetch, ungetch2;   /* ungotten characters, if any */
	boolean resolveRequired;     /* must resolve if/else/elif/endif branch */
	boolean hasAtLiteralStrings; /* supports @"c:\" strings */
	struct sDirective {
		enum eState state;       /* current directive being processed */
		boolean	accept;          /* is a directive syntactically permitted? */
		vString * name;          /* macro name */
		unsigned int nestLevel;  /* level 0 is not used */
		conditionalInfo ifdef [MaxCppNestingLevel];
	} directive;
} cppState;

/*
*   DATA DEFINITIONS
*/

/*  Use brace formatting to detect end of block.
 */
static boolean BraceFormat = FALSE;

static cppState Cpp = {
	'\0', '\0',  /* ungetch characters */
	FALSE,       /* resolveRequired */
	FALSE,       /* hasAtLiteralStrings */
	{
		DRCTV_NONE,  /* state */
		FALSE,       /* accept */
		NULL,        /* tag name */
		0,           /* nestLevel */
		{ {FALSE,FALSE,FALSE,FALSE} }  /* ifdef array */
	}  /* directive */
};

/*
*   FUNCTION DEFINITIONS
*/

extern boolean isBraceFormat (void)
{
	return BraceFormat;
}

extern unsigned int getDirectiveNestLevel (void)
{
	return Cpp.directive.nestLevel;
}

extern void cppInit (const boolean state, const boolean hasAtLiteralStrings)
{
	BraceFormat = state;

	Cpp.ungetch         = '\0';
	Cpp.ungetch2        = '\0';
	Cpp.resolveRequired = FALSE;
	Cpp.hasAtLiteralStrings = hasAtLiteralStrings;

	Cpp.directive.state     = DRCTV_NONE;
	Cpp.directive.accept    = TRUE;
	Cpp.directive.nestLevel = 0;

	Cpp.directive.ifdef [0].ignoreAllBranches = FALSE;
	Cpp.directive.ifdef [0].singleBranch = FALSE;
	Cpp.directive.ifdef [0].branchChosen = FALSE;
	Cpp.directive.ifdef [0].ignoring     = FALSE;

	if (Cpp.directive.name == NULL)
		Cpp.directive.name = vStringNew ();
	else
		vStringClear (Cpp.directive.name);
}

extern void cppTerminate (void)
{
	if (Cpp.directive.name != NULL)
	{
		vStringDelete (Cpp.directive.name);
		Cpp.directive.name = NULL;
	}
}

extern void cppBeginStatement (void)
{
	Cpp.resolveRequired = TRUE;
}

extern void cppEndStatement (void)
{
	Cpp.resolveRequired = FALSE;
}

/*
*   Scanning functions
*
*   This section handles preprocessor directives.  It strips out all
*   directives and may emit a tag for #define directives.
*/

/*  This puts a character back into the input queue for the source File.
 *  Up to two characters may be ungotten.
 */
extern void cppUngetc (const int c)
{
	Assert (Cpp.ungetch2 == '\0');
	Cpp.ungetch2 = Cpp.ungetch;
	Cpp.ungetch = c;
}

/*  Reads a directive, whose first character is given by "c", into "name".
 */
static boolean readDirective (int c, char *const name, unsigned int maxLength)
{
	unsigned int i;

	for (i = 0  ;  i < maxLength - 1  ;  ++i)
	{
		if (i > 0)
		{
			c = fileGetc ();
			if (c == EOF  ||  ! isalpha (c))
			{
				fileUngetc (c);
				break;
			}
		}
		name [i] = c;
	}
	name [i] = '\0';  /* null terminate */

	return (boolean) isspacetab (c);
}

/*  Reads an identifier, whose first character is given by "c", into "tag",
 *  together with the file location and corresponding line number.
 */
static void readIdentifier (int c, vString *const name)
{
	vStringClear (name);
	do
	{
		vStringPut (name, c);
	} while (c = fileGetc (), (c != EOF  &&  isident (c)));
	fileUngetc (c);
	vStringTerminate (name);
}

static conditionalInfo *currentConditional (void)
{
	return &Cpp.directive.ifdef [Cpp.directive.nestLevel];
}

static boolean isIgnore (void)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring;
}

static boolean setIgnore (const boolean ignore)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring = ignore;
}

static boolean isIgnoreBranch (void)
{
	conditionalInfo *const ifdef = currentConditional ();

	/*  Force a single branch if an incomplete statement is discovered
	 *  en route. This may have allowed earlier branches containing complete
	 *  statements to be followed, but we must follow no further branches.
	 */
	if (Cpp.resolveRequired  &&  ! BraceFormat)
		ifdef->singleBranch = TRUE;

	/*  We will ignore this branch in the following cases:
	 *
	 *  1.  We are ignoring all branches (conditional was within an ignored
	 *        branch of the parent conditional)
	 *  2.  A branch has already been chosen and either of:
	 *      a.  A statement was incomplete upon entering the conditional
	 *      b.  A statement is incomplete upon encountering a branch
	 */
	return (boolean) (ifdef->ignoreAllBranches ||
					 (ifdef->branchChosen  &&  ifdef->singleBranch));
}

static void chooseBranch (void)
{
	if (! BraceFormat)
	{
		conditionalInfo *const ifdef = currentConditional ();

		ifdef->branchChosen = (boolean) (ifdef->singleBranch ||
										Cpp.resolveRequired);
	}
}

/*  Pushes one nesting level for an #if directive, indicating whether or not
 *  the branch should be ignored and whether a branch has already been chosen.
 */
static boolean pushConditional (const boolean firstBranchChosen)
{
	const boolean ignoreAllBranches = isIgnore ();  /* current ignore */
	boolean ignoreBranch = FALSE;

	if (Cpp.directive.nestLevel < (unsigned int) MaxCppNestingLevel - 1)
	{
		conditionalInfo *ifdef;

		++Cpp.directive.nestLevel;
		ifdef = currentConditional ();

		/*  We take a snapshot of whether there is an incomplete statement in
		 *  progress upon encountering the preprocessor conditional. If so,
		 *  then we will flag that only a single branch of the conditional
		 *  should be followed.
		 */
		ifdef->ignoreAllBranches = ignoreAllBranches;
		ifdef->singleBranch      = Cpp.resolveRequired;
		ifdef->branchChosen      = firstBranchChosen;
		ifdef->ignoring = (boolean) (ignoreAllBranches || (
				! firstBranchChosen  &&  ! BraceFormat  &&
				(ifdef->singleBranch || !Option.if0)));
		ignoreBranch = ifdef->ignoring;
	}
	return ignoreBranch;
}

/*  Pops one nesting level for an #endif directive.
 */
static boolean popConditional (void)
{
	if (Cpp.directive.nestLevel > 0)
		--Cpp.directive.nestLevel;

	return isIgnore ();
}

static void makeDefineTag (const char *const name)
{
	const boolean isFileScope = (boolean) (! isHeaderFile ());

	if (includingDefineTags () &&
		(! isFileScope  ||  Option.include.fileScope))
	{
		tagEntryInfo e;
		initTagEntry (&e, name);
		e.lineNumberEntry = (boolean) (Option.locate != EX_PATTERN);
		e.isFileScope  = isFileScope;
		e.truncateLine = TRUE;
		e.kindName     = "macro";
		e.kind         = 'd';
		makeTagEntry (&e);
	}
}

static void directiveDefine (const int c)
{
	if (isident1 (c))
	{
		readIdentifier (c, Cpp.directive.name);
		if (! isIgnore ())
			makeDefineTag (vStringValue (Cpp.directive.name));
	}
	Cpp.directive.state = DRCTV_NONE;
}

static void directivePragma (int c)
{
	if (isident1 (c))
	{
		readIdentifier (c, Cpp.directive.name);
		if (stringMatch (vStringValue (Cpp.directive.name), "weak"))
		{
			/* generate macro tag for weak name */
			do
			{
				c = fileGetc ();
			} while (c == SPACE);
			if (isident1 (c))
			{
				readIdentifier (c, Cpp.directive.name);
				makeDefineTag (vStringValue (Cpp.directive.name));
			}
		}
	}
	Cpp.directive.state = DRCTV_NONE;
}

static boolean directiveIf (const int c)
{
	DebugStatement ( const boolean ignore0 = isIgnore (); )
	const boolean ignore = pushConditional ((boolean) (c != '0'));

	Cpp.directive.state = DRCTV_NONE;
	DebugStatement ( debugCppNest (TRUE, Cpp.directive.nestLevel);
	                 if (ignore != ignore0) debugCppIgnore (ignore); )

	return ignore;
}

static boolean directiveHash (const int c)
{
	boolean ignore = FALSE;
	char directive [MaxDirectiveName];
	DebugStatement ( const boolean ignore0 = isIgnore (); )

	readDirective (c, directive, MaxDirectiveName);
	if (stringMatch (directive, "define"))
		Cpp.directive.state = DRCTV_DEFINE;
	else if (stringMatch (directive, "undef"))
		Cpp.directive.state = DRCTV_UNDEF;
	else if (strncmp (directive, "if", (size_t) 2) == 0)
		Cpp.directive.state = DRCTV_IF;
	else if (stringMatch (directive, "elif")  ||
			stringMatch (directive, "else"))
	{
		ignore = setIgnore (isIgnoreBranch ());
		if (! ignore  &&  stringMatch (directive, "else"))
			chooseBranch ();
		Cpp.directive.state = DRCTV_NONE;
		DebugStatement ( if (ignore != ignore0) debugCppIgnore (ignore); )
	}
	else if (stringMatch (directive, "endif"))
	{
		DebugStatement ( debugCppNest (FALSE, Cpp.directive.nestLevel); )
		ignore = popConditional ();
		Cpp.directive.state = DRCTV_NONE;
		DebugStatement ( if (ignore != ignore0) debugCppIgnore (ignore); )
	}
	else if (stringMatch (directive, "pragma"))
		Cpp.directive.state = DRCTV_PRAGMA;
	else
		Cpp.directive.state = DRCTV_NONE;

	return ignore;
}

/*  Handles a pre-processor directive whose first character is given by "c".
 */
static boolean handleDirective (const int c)
{
	boolean ignore = isIgnore ();

	switch (Cpp.directive.state)
	{
		case DRCTV_NONE:    ignore = isIgnore ();        break;
		case DRCTV_DEFINE:  directiveDefine (c);         break;
		case DRCTV_HASH:    ignore = directiveHash (c);  break;
		case DRCTV_IF:      ignore = directiveIf (c);    break;
		case DRCTV_PRAGMA:  directivePragma (c);         break;
		case DRCTV_UNDEF:   directiveDefine (c);         break;
	}
	return ignore;
}

/*  Called upon reading of a slash ('/') characters, determines whether a
 *  comment is encountered, and its type.
 */
static Comment isComment (void)
{
	Comment comment;
	const int next = fileGetc ();

	if (next == '*')
		comment = COMMENT_C;
	else if (next == '/')
		comment = COMMENT_CPLUS;
	else
	{
		fileUngetc (next);
		comment = COMMENT_NONE;
	}
	return comment;
}

/*  Skips over a C style comment. According to ANSI specification a comment
 *  is treated as white space, so we perform this substitution.
 */
int skipOverCComment (void)
{
	int c = fileGetc ();

	while (c != EOF)
	{
		if (c != '*')
			c = fileGetc ();
		else
		{
			const int next = fileGetc ();

			if (next != '/')
				c = next;
			else
			{
				c = SPACE;  /* replace comment with space */
				break;
			}
		}
	}
	return c;
}

/*  Skips over a C++ style comment.
 */
static int skipOverCplusComment (void)
{
	int c;

	while ((c = fileGetc ()) != EOF)
	{
		if (c == BACKSLASH)
			fileGetc ();  /* throw away next character, too */
		else if (c == NEWLINE)
			break;
	}
	return c;
}

/*  Skips to the end of a string, returning a special character to
 *  symbolically represent a generic string.
 */
static int skipToEndOfString (boolean ignoreBackslash)
{
	int c;

	while ((c = fileGetc ()) != EOF)
	{
		if (c == BACKSLASH && ! ignoreBackslash)
			fileGetc ();  /* throw away next character, too */
		else if (c == DOUBLE_QUOTE)
			break;
	}
	return STRING_SYMBOL;  /* symbolic representation of string */
}

/*  Skips to the end of the three (possibly four) 'c' sequence, returning a
 *  special character to symbolically represent a generic character.
 *  Also detects Vera numbers that include a base specifier (ie. 'b1010).
 */
static int skipToEndOfChar (void)
{
	int c;
	int count = 0, veraBase = '\0';

	while ((c = fileGetc ()) != EOF)
	{
	    ++count;
		if (c == BACKSLASH)
			fileGetc ();  /* throw away next character, too */
		else if (c == SINGLE_QUOTE)
			break;
		else if (c == NEWLINE)
		{
			fileUngetc (c);
			break;
		}
		else if (count == 1  &&  strchr ("DHOB", toupper (c)) != NULL)
			veraBase = c;
		else if (veraBase != '\0'  &&  ! isalnum (c))
		{
			fileUngetc (c);
			break;
		}
	}
	return CHAR_SYMBOL;  /* symbolic representation of character */
}

/*  This function returns the next character, stripping out comments,
 *  C pre-processor directives, and the contents of single and double
 *  quoted strings. In short, strip anything which places a burden upon
 *  the tokenizer.
 */
extern int cppGetc (void)
{
	boolean directive = FALSE;
	boolean ignore = FALSE;
	int c;

	if (Cpp.ungetch != '\0')
	{
		c = Cpp.ungetch;
		Cpp.ungetch = Cpp.ungetch2;
		Cpp.ungetch2 = '\0';
		return c;  /* return here to avoid re-calling debugPutc () */
	}
	else do
	{
		c = fileGetc ();
process:
		switch (c)
		{
			case EOF:
				ignore    = FALSE;
				directive = FALSE;
				break;

			case TAB:
			case SPACE:
				break;  /* ignore most white space */

			case NEWLINE:
				if (directive  &&  ! ignore)
					directive = FALSE;
				Cpp.directive.accept = TRUE;
				break;

			case DOUBLE_QUOTE:
				Cpp.directive.accept = FALSE;
				c = skipToEndOfString (FALSE);
				break;

			case '#':
				if (Cpp.directive.accept)
				{
					directive = TRUE;
					Cpp.directive.state  = DRCTV_HASH;
					Cpp.directive.accept = FALSE;
				}
				break;

			case SINGLE_QUOTE:
				Cpp.directive.accept = FALSE;
				c = skipToEndOfChar ();
				break;

			case '/':
			{
				const Comment comment = isComment ();

				if (comment == COMMENT_C)
					c = skipOverCComment ();
				else if (comment == COMMENT_CPLUS)
				{
					c = skipOverCplusComment ();
					if (c == NEWLINE)
						fileUngetc (c);
				}
				else
					Cpp.directive.accept = FALSE;
				break;
			}

			case BACKSLASH:
			{
				int next = fileGetc ();

				if (next == NEWLINE)
					continue;
				else if (next == '?')
					cppUngetc (next);
				else
					fileUngetc (next);
				break;
			}

			case '?':
			{
				int next = fileGetc ();
				if (next != '?')
					fileUngetc (next);
				else
				{
					next = fileGetc ();
					switch (next)
					{
						case '(':          c = '[';       break;
						case ')':          c = ']';       break;
						case '<':          c = '{';       break;
						case '>':          c = '}';       break;
						case '/':          c = BACKSLASH; goto process;
						case '!':          c = '|';       break;
						case SINGLE_QUOTE: c = '^';       break;
						case '-':          c = '~';       break;
						case '=':          c = '#';       goto process;
						default:
							fileUngetc (next);
							cppUngetc ('?');
							break;
					}
				}
			} break;

			default:
				if (c == '@' && Cpp.hasAtLiteralStrings)
				{
					int next = fileGetc ();
					if (next == DOUBLE_QUOTE)
					{
						Cpp.directive.accept = FALSE;
						c = skipToEndOfString (TRUE);
						break;
					}
				}
				Cpp.directive.accept = FALSE;
				if (directive)
					ignore = handleDirective (c);
				break;
		}
	} while (directive || ignore);

	DebugStatement ( debugPutc (DEBUG_CPP, c); )
	DebugStatement ( if (c == NEWLINE)
				debugPrintf (DEBUG_CPP, "%6ld: ", getInputLineNumber () + 1); )

	return c;
}

/* vi:set tabstop=4 shiftwidth=4: */
