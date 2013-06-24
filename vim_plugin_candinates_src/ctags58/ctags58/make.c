/*
*   $Id: make.c 681 2008-10-12 22:43:00Z dhiebert $
*
*   Copyright (c) 2000-2005, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for makefiles.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "options.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MACRO
} shKind;

static kindOption MakeKinds [] = {
	{ TRUE, 'm', "macro", "macros"}
};

/*
*   FUNCTION DEFINITIONS
*/

static int nextChar (void)
{
	int c = fileGetc ();
	if (c == '\\')
	{
		c = fileGetc ();
		if (c == '\n')
			c = fileGetc ();
	}
	return c;
}

static void skipLine (void)
{
	int c;
	do
		c = nextChar ();
	while (c != EOF  &&  c != '\n');
	if (c == '\n')
		fileUngetc (c);
}

static int skipToNonWhite (void)
{
	int c;
	do
		c = nextChar ();
	while (c != '\n' && isspace (c));
	return c;
}

static boolean isIdentifier (int c)
{
	return (boolean)(c != '\0' && (isalnum (c)  ||  strchr (".-_", c) != NULL));
}

static void readIdentifier (const int first, vString *const id)
{
	int c = first;
	vStringClear (id);
	while (isIdentifier (c))
	{
		vStringPut (id, c);
		c = nextChar ();
	}
	fileUngetc (c);
	vStringTerminate (id);
}

static void skipToMatch (const char *const pair)
{
	const int begin = pair [0], end = pair [1];
	const unsigned long inputLineNumber = getInputLineNumber ();
	int matchLevel = 1;
	int c = '\0';

	while (matchLevel > 0)
	{
		c = nextChar ();
		if (c == begin)
			++matchLevel;
		else if (c == end)
			--matchLevel;
		else if (c == '\n')
			break;
	}
	if (c == EOF)
		verbose ("%s: failed to find match for '%c' at line %lu\n",
				getInputFileName (), begin, inputLineNumber);
}

static void findMakeTags (void)
{
	vString *name = vStringNew ();
	boolean newline = TRUE;
	boolean in_define = FALSE;
	boolean in_rule = FALSE;
	boolean variable_possible = TRUE;
	int c;

	while ((c = nextChar ()) != EOF)
	{
		if (newline)
		{
			if (in_rule)
			{
				if (c == '\t')
				{
					skipLine ();  /* skip rule */
					continue;
				}
				else
					in_rule = FALSE;
			}
			variable_possible = (boolean)(!in_rule);
			newline = FALSE;
		}
		if (c == '\n')
			newline = TRUE;
		else if (isspace (c))
			continue;
		else if (c == '#')
			skipLine ();
		else if (c == '(')
			skipToMatch ("()");
		else if (c == '{')
			skipToMatch ("{}");
		else if (c == ':')
		{
			variable_possible = TRUE;
			in_rule = TRUE;
		}
		else if (variable_possible && isIdentifier (c))
		{
			readIdentifier (c, name);
			if (strcmp (vStringValue (name), "endef") == 0)
				in_define = FALSE;
			else if (in_define)
				skipLine ();
			else if (strcmp (vStringValue (name), "define") == 0  &&
				isIdentifier (c))
			{
				in_define = TRUE;
				c = skipToNonWhite ();
				readIdentifier (c, name);
				makeSimpleTag (name, MakeKinds, K_MACRO);
				skipLine ();
			}
			else {
				if (strcmp(vStringValue (name), "export") == 0 &&
					isIdentifier (c))
				{
					c = skipToNonWhite ();
					readIdentifier (c, name);
				}
				c = skipToNonWhite ();
				if (strchr (":?+", c) != NULL)
				{
					boolean append = (boolean)(c == '+');
					if (c == ':')
						in_rule = TRUE;
					c = nextChar ();
					if (c != '=')
						fileUngetc (c);
					else if (append)
					{
						skipLine ();
						continue;
					}
				}
				if (c == '=')
				{
					makeSimpleTag (name, MakeKinds, K_MACRO);
					in_rule = FALSE;
					skipLine ();
				}
			}
		}
		else
			variable_possible = FALSE;
	}
	vStringDelete (name);
}

extern parserDefinition* MakefileParser (void)
{
	static const char *const patterns [] = { "[Mm]akefile", "GNUmakefile", NULL };
	static const char *const extensions [] = { "mak", "mk", NULL };
	parserDefinition* const def = parserNew ("Make");
	def->kinds      = MakeKinds;
	def->kindCount  = KIND_COUNT (MakeKinds);
	def->patterns   = patterns;
	def->extensions = extensions;
	def->parser     = findMakeTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
