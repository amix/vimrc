/*
*   $Id: sml.c 536 2007-06-02 06:09:00Z elliotth $
*
*   Copyright (c) 2002, Venkatesh Prasad Ranganath and Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for SML language files.
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
 *   DATA DECLARATIONS
 */
typedef enum {
	K_AND = -2,
	K_NONE = -1,
	K_EXCEPTION,
	K_FUNCTION,
	K_FUNCTOR,
	K_SIGNATURE,
	K_STRUCTURE,
	K_TYPE,
	K_VAL
} smlKind;

/*
 *   DATA DEFINITIONS
 */
static kindOption SmlKinds[] = {
	{ TRUE, 'e', "exception", "exception declarations" },
	{ TRUE, 'f', "function",  "function definitions" },
	{ TRUE, 'c', "functor",   "functor definitions" },
	{ TRUE, 's', "signature", "signature declarations" },
	{ TRUE, 'r', "structure", "structure declarations" },
	{ TRUE, 't', "type",      "type definitions" },
	{ TRUE, 'v', "value",     "value bindings" }
};

static struct {
	const char *keyword;
	smlKind kind;
} SmlKeywordTypes [] = {
	{ "abstype",   K_TYPE      },
	{ "and",       K_AND       },
	{ "datatype",  K_TYPE      },
	{ "exception", K_EXCEPTION },
	{ "functor",   K_FUNCTOR   },
	{ "fun",       K_FUNCTION  },
	{ "signature", K_SIGNATURE },
	{ "structure", K_STRUCTURE },
	{ "type",      K_TYPE      },
	{ "val",       K_VAL       }
};

static unsigned int CommentLevel = 0;

/*
 * FUNCTION DEFINITIONS
 */

static void makeSmlTag (smlKind type, vString *name)
{
	tagEntryInfo tag;
	initTagEntry (&tag, vStringValue (name));
	tag.kindName = SmlKinds [type].name;
	tag.kind = SmlKinds [type].letter;
	makeTagEntry (&tag);
}

static const unsigned char *skipSpace (const unsigned char *cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

static boolean isIdentifier (int c)
{
	boolean result = FALSE;
	/* Consider '_' as an delimiter to aid user in tracking it's usage. */
	const char *const alternateIdentifiers = "!%&$#+-<>=/?@\\~'^|*_";
	if (isalnum (c))
		result = TRUE;
	else if (c != '\0'  &&  strchr (alternateIdentifiers, c) != NULL)
		result = TRUE;
	return result;
}

static const unsigned char *parseIdentifier (
		const unsigned char *cp, vString *const identifier)
{
	boolean stringLit = FALSE;
	vStringClear (identifier);
	while (*cp != '\0'  &&  (!isIdentifier ((int) *cp) || stringLit))
	{
		int oneback = *cp;
		cp++;
		if (oneback == '('  &&  *cp == '*'  &&  stringLit == FALSE)
		{
			CommentLevel++;
			return ++cp;
		}
		if (*cp == '"' && oneback != '\\')
		{
			stringLit = TRUE;
			continue;
		}
		if (stringLit && *cp == '"' && oneback != '\\')
			stringLit = FALSE;
	}
	if (strcmp ((const char *) cp, "") == 0 || cp == NULL)
		return cp;

	while (isIdentifier ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		cp++;
	}
	vStringTerminate (identifier);
	return cp;
}

static smlKind findNextIdentifier (const unsigned char **cp)
{
	smlKind result = K_NONE;
	vString *const identifier = vStringNew ();
	unsigned int count = sizeof (SmlKeywordTypes) / sizeof (SmlKeywordTypes [0]);
	unsigned int i;
	*cp = parseIdentifier (*cp, identifier);
	for (i = 0  ;  i < count  &&  result == K_NONE ;  ++i)
	{
		const char *id = vStringValue (identifier);
		if (strcmp (id, SmlKeywordTypes [i].keyword) == 0)
			result = SmlKeywordTypes [i].kind;
	}
	vStringDelete (identifier);
	return result;
}

static void findSmlTags (void)
{
	vString *const identifier = vStringNew ();
	const unsigned char *line;
	smlKind lastTag = K_NONE;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char *cp = skipSpace (line);
		do
		{
			smlKind foundTag;
			if (CommentLevel != 0)
			{
				cp = (const unsigned char *) strstr ((const char *) cp, "*)");
				if (cp == NULL)
					continue;
				else
				{
					--CommentLevel;
					cp += 2;
				}
			}
			foundTag = findNextIdentifier (&cp);
			if (foundTag != K_NONE)
			{
				cp = skipSpace (cp);
				cp = parseIdentifier (cp, identifier);
				if (foundTag == K_AND)
					makeSmlTag (lastTag, identifier);
				else
				{
					makeSmlTag (foundTag, identifier);
					lastTag = foundTag;
				}
			}
			if (strstr ((const char *) cp, "(*") != NULL)
			{
				cp += 2;
				cp = (const unsigned char *) strstr ((const char *) cp, "*)");
				if (cp == NULL)
					++CommentLevel;
			}
		} while (cp != NULL  &&  strcmp ((const char *) cp, "") != 0);
	}
	vStringDelete (identifier);
}

extern parserDefinition *SmlParser (void)
{
	static const char *const extensions[] = { "sml", "sig", NULL };
	parserDefinition *def = parserNew ("SML");
	def->kinds = SmlKinds;
	def->kindCount = KIND_COUNT (SmlKinds);
	def->extensions = extensions;
	def->parser = findSmlTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
