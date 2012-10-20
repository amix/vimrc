/*
*   $Id: awk.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for AWK functions.
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
typedef enum eAwkKinds {
	K_FUNCTION
} awkKind;

static kindOption AwkKinds [] = {
	{ TRUE, 'f', "function", "functions" }
};

/*
*   FUNCTION DEFINITIONS
*/

static void findAwkTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		if (strncmp ((const char*) line, "function", (size_t) 8) == 0  &&
			isspace ((int) line [8]))
		{
			const unsigned char *cp = line + 8;

			while (isspace ((int) *cp))
				++cp;
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			vStringTerminate (name);
			while (isspace ((int) *cp))
				++cp;
			if (*cp == '(')
				makeSimpleTag (name, AwkKinds, K_FUNCTION);
			vStringClear (name);
			if (*cp != '\0')
				++cp;
		}
	}
	vStringDelete (name);
}

extern parserDefinition* AwkParser ()
{
	static const char *const extensions [] = { "awk", "gawk", "mawk", NULL };
	parserDefinition* def = parserNew ("Awk");
	def->kinds      = AwkKinds;
	def->kindCount  = KIND_COUNT (AwkKinds);
	def->extensions = extensions;
	def->parser     = findAwkTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
