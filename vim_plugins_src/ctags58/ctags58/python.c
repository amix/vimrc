/*
*   $Id: python.c 720 2009-07-07 03:55:23Z dhiebert $
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Python language
*   files.
*/
/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "options.h"
#include "read.h"
#include "main.h"
#include "vstring.h"
#include "routines.h"
#include "debug.h"

/*
*   DATA DECLARATIONS
*/
typedef struct NestingLevel NestingLevel;
typedef struct NestingLevels NestingLevels;

struct NestingLevel
{
	int indentation;
	vString *name;
	int type;
};

struct NestingLevels
{
	NestingLevel *levels;
	int n;					/* number of levels in use */
	int allocated;
};

typedef enum {
	K_CLASS, K_FUNCTION, K_MEMBER, K_VARIABLE, K_IMPORT
} pythonKind;

/*
*   DATA DEFINITIONS
*/
static kindOption PythonKinds[] = {
	{TRUE, 'c', "class",    "classes"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'm', "member",   "class members"},
    {TRUE, 'v', "variable", "variables"},
    {TRUE, 'i', "namespace", "imports"}
};

static char const * const singletriple = "'''";
static char const * const doubletriple = "\"\"\"";

/*
*   FUNCTION DEFINITIONS
*/

static NestingLevels *nestingLevelsNew (void)
{
	NestingLevels *nls = xCalloc (1, NestingLevels);
	return nls;
}

static void nestingLevelsFree (NestingLevels *nls)
{
	int i;
	for (i = 0; i < nls->allocated; i++)
		vStringDelete(nls->levels[i].name);
	if (nls->levels) eFree(nls->levels);
	eFree(nls);
}

static void nestingLevelsPush (NestingLevels *nls,
	const vString *name, int type)
{
	NestingLevel *nl = NULL;

	if (nls->n >= nls->allocated)
	{
		nls->allocated++;
		nls->levels = xRealloc(nls->levels,
			nls->allocated, NestingLevel);
		nls->levels[nls->n].name = vStringNew();
	}
	nl = &nls->levels[nls->n];
	nls->n++;

	vStringCopy(nl->name, name);
	nl->type = type;
}

#if 0
static NestingLevel *nestingLevelsGetCurrent (NestingLevels *nls)
{
	Assert (nls != NULL);

	if (nls->n < 1)
		return NULL;

	return &nls->levels[nls->n - 1];
}

static void nestingLevelsPop (NestingLevels *nls)
{
	const NestingLevel *nl = nestingLevelsGetCurrent(nls);

	Assert (nl != NULL);
	vStringClear(nl->name);
	nls->n--;
}
#endif

static boolean isIdentifierFirstCharacter (int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentifierCharacter (int c)
{
	return (boolean) (isalnum (c) || c == '_');
}

/* Given a string with the contents of a line directly after the "def" keyword,
 * extract all relevant information and create a tag.
 */
static void makeFunctionTag (vString *const function,
	vString *const parent, int is_class_parent, const char *arglist __unused__)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (function));

	tag.kindName = "function";
	tag.kind = 'f';
	/* tag.extensionFields.arglist = arglist; */

	if (vStringLength (parent) > 0)
	{
		if (is_class_parent)
		{
			tag.kindName = "member";
			tag.kind = 'm';
			tag.extensionFields.scope [0] = "class";
			tag.extensionFields.scope [1] = vStringValue (parent);
		}
		else
		{
			tag.extensionFields.scope [0] = "function";
			tag.extensionFields.scope [1] = vStringValue (parent);
		}
	}

	/* If a function starts with __, we mark it as file scope.
	 * FIXME: What is the proper way to signal such attributes?
	 * TODO: What does functions/classes starting with _ and __ mean in python?
	 */
	if (strncmp (vStringValue (function), "__", 2) == 0 &&
		strcmp (vStringValue (function), "__init__") != 0)
	{
		tag.extensionFields.access = "private";
		tag.isFileScope = TRUE;
	}
	else
	{
		tag.extensionFields.access = "public";
	}
	makeTagEntry (&tag);
}

/* Given a string with the contents of the line directly after the "class"
 * keyword, extract all necessary information and create a tag.
 */
static void makeClassTag (vString *const class, vString *const inheritance,
	vString *const parent, int is_class_parent)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (class));
	tag.kindName = "class";
	tag.kind = 'c';
	if (vStringLength (parent) > 0)
	{
		if (is_class_parent)
		{
			tag.extensionFields.scope [0] = "class";
			tag.extensionFields.scope [1] = vStringValue (parent);
		}
		else
		{
			tag.extensionFields.scope [0] = "function";
			tag.extensionFields.scope [1] = vStringValue (parent);
		}
	}
	tag.extensionFields.inheritance = vStringValue (inheritance);
	makeTagEntry (&tag);
}

static void makeVariableTag (vString *const var, vString *const parent)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (var));
	tag.kindName = "variable";
	tag.kind = 'v';
	if (vStringLength (parent) > 0)
	{
		tag.extensionFields.scope [0] = "class";
		tag.extensionFields.scope [1] = vStringValue (parent);
	}
	makeTagEntry (&tag);
}

/* Skip a single or double quoted string. */
static const char *skipString (const char *cp)
{
	const char *start = cp;
	int escaped = 0;
	for (cp++; *cp; cp++)
	{
		if (escaped)
			escaped--;
		else if (*cp == '\\')
			escaped++;
		else if (*cp == *start)
			return cp + 1;
	}
	return cp;
}

/* Skip everything up to an identifier start. */
static const char *skipEverything (const char *cp)
{
	for (; *cp; cp++)
	{
		if (*cp == '"' || *cp == '\'')
		{
			cp = skipString(cp);
			if (!*cp) break;
		}
		if (isIdentifierFirstCharacter ((int) *cp))
			return cp;
	}
	return cp;
}

/* Skip an identifier. */
static const char *skipIdentifier (const char *cp)
{
	while (isIdentifierCharacter ((int) *cp))
		cp++;
	return cp;
}

static const char *findDefinitionOrClass (const char *cp)
{
	while (*cp)
	{
		cp = skipEverything (cp);
		if (!strncmp(cp, "def", 3) || !strncmp(cp, "class", 5) ||
			!strncmp(cp, "cdef", 4) || !strncmp(cp, "cpdef", 5))
		{
			return cp;
		}
		cp = skipIdentifier (cp);
	}
	return NULL;
}

static const char *skipSpace (const char *cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

/* Starting at ''cp'', parse an identifier into ''identifier''. */
static const char *parseIdentifier (const char *cp, vString *const identifier)
{
	vStringClear (identifier);
	while (isIdentifierCharacter ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		++cp;
	}
	vStringTerminate (identifier);
	return cp;
}

static void parseClass (const char *cp, vString *const class,
	vString *const parent, int is_class_parent)
{
	vString *const inheritance = vStringNew ();
	vStringClear (inheritance);
	cp = parseIdentifier (cp, class);
	cp = skipSpace (cp);
	if (*cp == '(')
	{
		++cp;
		while (*cp != ')')
		{
			if (*cp == '\0')
			{
				/* Closing parenthesis can be in follow up line. */
				cp = (const char *) fileReadLine ();
				if (!cp) break;
				vStringPut (inheritance, ' ');
				continue;
			}
			vStringPut (inheritance, *cp);
			++cp;
		}
		vStringTerminate (inheritance);
	}
	makeClassTag (class, inheritance, parent, is_class_parent);
	vStringDelete (inheritance);
}

static void parseImports (const char *cp)
{
	const char *pos;
	vString *name, *name_next;

	cp = skipEverything (cp);

	if ((pos = strstr (cp, "import")) == NULL)
		return;

	cp = pos + 6;

	/* continue only if there is some space between the keyword and the identifier */
	if (! isspace (*cp))
		return;

	cp++;
	cp = skipSpace (cp);

	name = vStringNew ();
	name_next = vStringNew ();

	cp = skipEverything (cp);
	while (*cp)
	{
		cp = parseIdentifier (cp, name);

		cp = skipEverything (cp);
		/* we parse the next possible import statement as well to be able to ignore 'foo' in
		 * 'import foo as bar' */
		parseIdentifier (cp, name_next);

		/* take the current tag only if the next one is not "as" */
		if (strcmp (vStringValue (name_next), "as") != 0 &&
			strcmp (vStringValue (name), "as") != 0)
		{
			makeSimpleTag (name, PythonKinds, K_IMPORT);
		}
	}
	vStringDelete (name);
	vStringDelete (name_next);
}

/* modified from get.c getArglistFromStr().
 * warning: terminates rest of string past arglist!
 * note: does not ignore brackets inside strings! */
static char *parseArglist(const char *buf)
{
	char *start, *end;
	int level;
	if (NULL == buf)
		return NULL;
	if (NULL == (start = strchr(buf, '(')))
		return NULL;
	for (level = 1, end = start + 1; level > 0; ++end)
	{
		if ('\0' == *end)
			break;
		else if ('(' == *end)
			++ level;
		else if (')' == *end)
			-- level;
	}
	*end = '\0';
	return strdup(start);
}

static void parseFunction (const char *cp, vString *const def,
	vString *const parent, int is_class_parent)
{
	char *arglist;

	cp = parseIdentifier (cp, def);
	arglist = parseArglist (cp);
	makeFunctionTag (def, parent, is_class_parent, arglist);
	eFree (arglist);
}

/* Get the combined name of a nested symbol. Classes are separated with ".",
 * functions with "/". For example this code:
 * class MyClass:
 *     def myFunction:
 *         def SubFunction:
 *             class SubClass:
 *                 def Method:
 *                     pass
 * Would produce this string:
 * MyClass.MyFunction/SubFunction/SubClass.Method
 */
static boolean constructParentString(NestingLevels *nls, int indent,
	vString *result)
{
	int i;
	NestingLevel *prev = NULL;
	int is_class = FALSE;
	vStringClear (result);
	for (i = 0; i < nls->n; i++)
	{
		NestingLevel *nl = nls->levels + i;
		if (indent <= nl->indentation)
			break;
		if (prev)
		{
			vStringCatS(result, ".");	/* make Geany symbol list grouping work properly */
/*
			if (prev->type == K_CLASS)
				vStringCatS(result, ".");
			else
				vStringCatS(result, "/");
*/
		}
		vStringCat(result, nl->name);
		is_class = (nl->type == K_CLASS);
		prev = nl;
	}
	return is_class;
}

/* Check whether parent's indentation level is higher than the current level and
 * if so, remove it.
 */
static void checkParent(NestingLevels *nls, int indent, vString *parent)
{
	int i;
	NestingLevel *n;

	for (i = 0; i < nls->n; i++)
	{
		n = nls->levels + i;
		/* is there a better way to compare two vStrings? */
		if (strcmp(vStringValue(parent), vStringValue(n->name)) == 0)
		{
			if (n && indent <= n->indentation)
			{
				/* remove this level by clearing its name */
				vStringClear(n->name);
			}
			break;
		}
	}
}

static void addNestingLevel(NestingLevels *nls, int indentation,
	const vString *name, boolean is_class)
{
	int i;
	NestingLevel *nl = NULL;

	for (i = 0; i < nls->n; i++)
	{
		nl = nls->levels + i;
		if (indentation <= nl->indentation) break;
	}
	if (i == nls->n)
	{
		nestingLevelsPush(nls, name, 0);
		nl = nls->levels + i;
	}
	else
	{	/* reuse existing slot */
		nls->n = i + 1;
		vStringCopy(nl->name, name);
	}
	nl->indentation = indentation;
	nl->type = is_class ? K_CLASS : !K_CLASS;
}

/* Return a pointer to the start of the next triple string, or NULL. Store
 * the kind of triple string in "which" if the return is not NULL.
 */
static char const *find_triple_start(char const *string, char const **which)
{
	char const *cp = string;

	for (; *cp; cp++)
	{
		if (*cp == '"' || *cp == '\'')
		{
			if (strncmp(cp, doubletriple, 3) == 0)
			{
				*which = doubletriple;
				return cp;
			}
			if (strncmp(cp, singletriple, 3) == 0)
			{
				*which = singletriple;
				return cp;
			}
			cp = skipString(cp);
			if (!*cp) break;
		}
	}
	return NULL;
}

/* Find the end of a triple string as pointed to by "which", and update "which"
 * with any other triple strings following in the given string.
 */
static void find_triple_end(char const *string, char const **which)
{
	char const *s = string;
	while (1)
	{
		/* Check if the string ends in the same line. */
		s = strstr (s, *which);
		if (!s) break;
		s += 3;
		*which = NULL;
		/* If yes, check if another one starts in the same line. */
		s = find_triple_start(s, which);
		if (!s) break;
		s += 3;
	}
}

static const char *findVariable(const char *line)
{
	/* Parse global and class variable names (C.x) from assignment statements.
	 * Object attributes (obj.x) are ignored.
	 * Assignment to a tuple 'x, y = 2, 3' not supported.
	 * TODO: ignore duplicate tags from reassignment statements. */
	const char *cp, *sp, *eq, *start;

	cp = strstr(line, "=");
	if (!cp)
		return NULL;
	eq = cp + 1;
	while (*eq)
	{
		if (*eq == '=')
			return NULL;	/* ignore '==' operator and 'x=5,y=6)' function lines */
		if (*eq == '(' || *eq == '#')
			break;	/* allow 'x = func(b=2,y=2,' lines and comments at the end of line */
		eq++;
	}

	/* go backwards to the start of the line, checking we have valid chars */
	start = cp - 1;
	while (start >= line && isspace ((int) *start))
		--start;
	while (start >= line && isIdentifierCharacter ((int) *start))
		--start;
	if (!isIdentifierFirstCharacter(*(start + 1)))
		return NULL;
	sp = start;
	while (sp >= line && isspace ((int) *sp))
		--sp;
	if ((sp + 1) != line)	/* the line isn't a simple variable assignment */
		return NULL;
	/* the line is valid, parse the variable name */
	++start;
	return start;
}

/* Skip type declaration that optionally follows a cdef/cpdef */
static const char *skipTypeDecl (const char *cp, boolean *is_class)
{
	const char *lastStart = cp, *ptr = cp;
	int loopCount = 0;
	ptr = skipSpace(cp);
	if (!strncmp("extern", ptr, 6)) {
		ptr += 6;
		ptr = skipSpace(ptr);
		if (!strncmp("from", ptr, 4)) { return NULL; }
	}
	if (!strncmp("class", ptr, 5)) {
		ptr += 5 ;
		*is_class = TRUE;
		ptr = skipSpace(ptr);
		return ptr;
	}
	/* limit so that we don't pick off "int item=obj()" */
	while (*ptr && loopCount++ < 2) {
		while (*ptr && *ptr != '=' && *ptr != '(' && !isspace(*ptr)) ptr++;
		if (!*ptr || *ptr == '=') return NULL;
		if (*ptr == '(') {
		    return lastStart; /* if we stopped on a '(' we are done */
		}
		ptr = skipSpace(ptr);
		lastStart = ptr;
		while (*lastStart == '*') lastStart++;  /* cdef int *identifier */
	}
	return NULL;
}

static void findPythonTags (void)
{
	vString *const continuation = vStringNew ();
	vString *const name = vStringNew ();
	vString *const parent = vStringNew();

	NestingLevels *const nesting_levels = nestingLevelsNew();

	const char *line;
	int line_skip = 0;
	char const *longStringLiteral = NULL;

	while ((line = (const char *) fileReadLine ()) != NULL)
	{
		const char *cp = line, *candidate;
		char const *longstring;
		char const *keyword, *variable;
		int indent;

		cp = skipSpace (cp);

		if (*cp == '\0')  /* skip blank line */
			continue;

		/* Skip comment if we are not inside a multi-line string. */
		if (*cp == '#' && !longStringLiteral)
			continue;

		/* Deal with line continuation. */
		if (!line_skip) vStringClear(continuation);
		vStringCatS(continuation, line);
		vStringStripTrailing(continuation);
		if (vStringLast(continuation) == '\\')
		{
			vStringChop(continuation);
			vStringCatS(continuation, " ");
			line_skip = 1;
			continue;
		}
		cp = line = vStringValue(continuation);
		cp = skipSpace (cp);
		indent = cp - line;
		line_skip = 0;

		checkParent(nesting_levels, indent, parent);

		/* Deal with multiline string ending. */
		if (longStringLiteral)
		{
			find_triple_end(cp, &longStringLiteral);
			continue;
		}

		/* Deal with multiline string start. */
		longstring = find_triple_start(cp, &longStringLiteral);
		if (longstring)
		{
			longstring += 3;
			find_triple_end(longstring, &longStringLiteral);
			/* We don't parse for any tags in the rest of the line. */
			continue;
		}

		/* Deal with def and class keywords. */
		keyword = findDefinitionOrClass (cp);
		if (keyword)
		{
			boolean found = FALSE;
			boolean is_class = FALSE;
			if (!strncmp (keyword, "def ", 4))
			{
				cp = skipSpace (keyword + 3);
				found = TRUE;
			}
			else if (!strncmp (keyword, "class ", 6))
			{
				cp = skipSpace (keyword + 5);
				found = TRUE;
				is_class = TRUE;
			}
			else if (!strncmp (keyword, "cdef ", 5))
		    {
		        cp = skipSpace(keyword + 4);
		        candidate = skipTypeDecl (cp, &is_class);
		        if (candidate)
		        {
		    		found = TRUE;
		    		cp = candidate;
		        }

		    }
    		else if (!strncmp (keyword, "cpdef ", 6))
		    {
		        cp = skipSpace(keyword + 5);
		        candidate = skipTypeDecl (cp, &is_class);
		        if (candidate)
		        {
		    		found = TRUE;
		    		cp = candidate;
		        }
		    }

			if (found)
			{
				boolean is_parent_class;

				is_parent_class =
					constructParentString(nesting_levels, indent, parent);

				if (is_class)
					parseClass (cp, name, parent, is_parent_class);
				else
					parseFunction(cp, name, parent, is_parent_class);

				addNestingLevel(nesting_levels, indent, name, is_class);
			}
		}
		/* Find global and class variables */
		variable = findVariable(line);
		if (variable)
		{
			const char *start = variable;
			boolean parent_is_class;

			vStringClear (name);
			while (isIdentifierCharacter ((int) *start))
			{
				vStringPut (name, (int) *start);
				++start;
			}
			vStringTerminate (name);

			parent_is_class = constructParentString(nesting_levels, indent, parent);
			/* skip variables in methods */
			if (! parent_is_class && vStringLength(parent) > 0)
				continue;

			makeVariableTag (name, parent);
		}
		/* Find and parse imports */
		parseImports(line);
	}
	/* Clean up all memory we allocated. */
	vStringDelete (parent);
	vStringDelete (name);
	vStringDelete (continuation);
	nestingLevelsFree (nesting_levels);
}

extern parserDefinition *PythonParser (void)
{
    static const char *const extensions[] = { "py", "pyx", "pxd", "pxi" ,"scons", NULL };
	parserDefinition *def = parserNew ("Python");
	def->kinds = PythonKinds;
	def->kindCount = KIND_COUNT (PythonKinds);
	def->extensions = extensions;
	def->parser = findPythonTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
