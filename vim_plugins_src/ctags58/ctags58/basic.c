/*
 *   $Id:$
 *
 *   Copyright (c) 2000-2006, Darren Hiebert, Elias Pschernig
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for BlitzBasic
 *   (BlitzMax), PureBasic and FreeBasic language files. For now, this is kept
 *   quite simple - but feel free to ask for more things added any time -
 *   patches are of course most welcome.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h" /* must always come first */

#include <string.h>

#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CONST,
	K_FUNCTION,
	K_LABEL,
	K_TYPE,
	K_VARIABLE,
	K_ENUM
} BasicKind;

typedef struct {
	char const *token;
	BasicKind kind;
	int skip;
} KeyWord;

static kindOption BasicKinds[] = {
	{TRUE, 'c', "constant", "constants"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'l', "label", "labels"},
	{TRUE, 't', "type", "types"},
	{TRUE, 'v', "variable", "variables"},
	{TRUE, 'g', "enum", "enumerations"}
};

static KeyWord blitzbasic_keywords[] = {
	{"const", K_CONST, 0},
	{"global", K_VARIABLE, 0},
	{"dim", K_VARIABLE, 0},
	{"function", K_FUNCTION, 0},
	{"type", K_TYPE, 0},
	{NULL, 0, 0}
};

static KeyWord purebasic_keywords[] = {
	{"newlist", K_VARIABLE, 0},
	{"global", K_VARIABLE, 0},
	{"dim", K_VARIABLE, 0},
	{"procedure", K_FUNCTION, 0},
	{"interface", K_TYPE, 0},
	{"structure", K_TYPE, 0},
	{NULL, 0, 0}
};

static KeyWord freebasic_keywords[] = {
	{"const", K_CONST, 0},
	{"dim as", K_VARIABLE, 1},
	{"dim", K_VARIABLE, 0},
	{"common", K_VARIABLE, 0},
	{"function", K_FUNCTION, 0},
	{"sub", K_FUNCTION, 0},
	{"private sub", K_FUNCTION, 0},
	{"public sub", K_FUNCTION, 0},
	{"private function", K_FUNCTION, 0},
	{"public function", K_FUNCTION, 0},
	{"type", K_TYPE, 0},
	{"enum", K_ENUM, 0},
	{NULL, 0, 0}
};

/*
 *   FUNCTION DEFINITIONS
 */

/* Match the name of a tag (function, variable, type, ...) starting at pos. */
static char const *extract_name (char const *pos, vString * name)
{
	while (isspace (*pos))
		pos++;
	vStringClear (name);
	for (; *pos && !isspace (*pos) && *pos != '(' && *pos != ','; pos++)
		vStringPut (name, *pos);
	vStringTerminate (name);
	return pos;
}

/* Match a keyword starting at p (case insensitive). */
static int match_keyword (const char *p, KeyWord const *kw)
{
	vString *name;
	size_t i;
	int j;
	for (i = 0; i < strlen (kw->token); i++)
	{
		if (tolower (p[i]) != kw->token[i])
			return 0;
	}
	name = vStringNew ();
	p += i;
	for (j = 0; j < 1 + kw->skip; j++)
	{
		p = extract_name (p, name);
	}	
	makeSimpleTag (name, BasicKinds, kw->kind);
	vStringDelete (name);
	return 1;
}

/* Match a "label:" style label. */
static void match_colon_label (char const *p)
{
	char const *end = p + strlen (p) - 1;
	while (isspace (*end))
		end--;
	if (*end == ':')
	{
		vString *name = vStringNew ();
		vStringNCatS (name, p, end - p);
		makeSimpleTag (name, BasicKinds, K_LABEL);
		vStringDelete (name);
	}
}

/* Match a ".label" style label. */
static void match_dot_label (char const *p)
{
	if (*p == '.')
	{
		vString *name = vStringNew ();
		extract_name (p + 1, name);
		makeSimpleTag (name, BasicKinds, K_LABEL);
		vStringDelete (name);
	}
}

static void findBasicTags (void)
{
	const char *line;
	const char *extension = fileExtension (vStringValue (File.name));
	KeyWord *keywords;

	if (strcmp (extension, "bb") == 0)
		keywords = blitzbasic_keywords;
	else if (strcmp (extension, "pb") == 0)
		keywords = purebasic_keywords;
	else
		keywords = freebasic_keywords;

	while ((line = (const char *) fileReadLine ()) != NULL)
	{
		const char *p = line;
		KeyWord const *kw;

		while (isspace (*p))
			p++;

		/* Empty line? */
		if (!*p)
			continue;

		/* In Basic, keywords always are at the start of the line. */
		for (kw = keywords; kw->token; kw++)
			if (match_keyword (p, kw)) break;

		/* Is it a label? */
		if (strcmp (extension, "bb") == 0)
			match_dot_label (p);
		else
			match_colon_label (p);
	}
}

parserDefinition *BasicParser (void)
{
	static char const *extensions[] = { "bas", "bi", "bb", "pb", NULL };
	parserDefinition *def = parserNew ("Basic");
	def->kinds = BasicKinds;
	def->kindCount = KIND_COUNT (BasicKinds);
	def->extensions = extensions;
	def->parser = findBasicTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
