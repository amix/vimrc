/*
*   $Id: scheme.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Scheme language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION, K_SET
} schemeKind;

static kindOption SchemeKinds [] = {
	{ TRUE, 'f', "function", "functions" },
	{ TRUE, 's', "set",      "sets" }
};

/*
*   FUNCTION DEFINITIONS
*/

/* Algorithm adapted from from GNU etags.
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((... (xyzzy ....
 * look for (set! xyzzy
 */
static void readIdentifier (vString *const name, const unsigned char *cp)
{
	const unsigned char *p;
	vStringClear (name);
	/* Go till you get to white space or a syntactic break */
	for (p = cp; *p != '\0' && *p != '(' && *p != ')' && !isspace (*p); p++)
		vStringPut (name, (int) *p);
	vStringTerminate (name);
}

static void findSchemeTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char *cp = line;

		if (cp [0] == '(' &&
			(cp [1] == 'D' || cp [1] == 'd') &&
			(cp [2] == 'E' || cp [2] == 'e') &&
			(cp [3] == 'F' || cp [3] == 'f'))
		{
			while (!isspace (*cp))
				cp++;
			/* Skip over open parens and white space */
			while (*cp != '\0' && (isspace (*cp) || *cp == '('))
				cp++;
			readIdentifier (name, cp);
			makeSimpleTag (name, SchemeKinds, K_FUNCTION);
		}
		if (cp [0] == '(' &&
			(cp [1] == 'S' || cp [1] == 's') &&
			(cp [2] == 'E' || cp [2] == 'e') &&
			(cp [3] == 'T' || cp [3] == 't') &&
			(cp [4] == '!' || cp [4] == '!') &&
			(isspace (cp [5])))
		{
			while (*cp != '\0'  &&  !isspace (*cp))
				cp++;
			/* Skip over white space */
			while (isspace (*cp))
				cp++;
			readIdentifier (name, cp);
			makeSimpleTag (name, SchemeKinds, K_SET);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* SchemeParser (void)
{
	static const char *const extensions [] = {
		"SCM", "SM", "sch", "scheme", "scm", "sm", NULL
	};
	parserDefinition* def = parserNew ("Scheme");
	def->kinds      = SchemeKinds;
	def->kindCount  = KIND_COUNT (SchemeKinds);
	def->extensions = extensions;
	def->parser     = findSchemeTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
