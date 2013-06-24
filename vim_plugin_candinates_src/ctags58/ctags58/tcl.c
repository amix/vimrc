/*
*   $Id: tcl.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for TCL scripts.
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
	K_CLASS, K_METHOD, K_PROCEDURE
} tclKind;

static kindOption TclKinds [] = {
	{ TRUE, 'c', "class",     "classes" },
	{ TRUE, 'm', "method",    "methods" },
	{ TRUE, 'p', "procedure", "procedures" }
};

/*
*   FUNCTION DEFINITIONS
*/

static const unsigned char *makeTclTag (
		const unsigned char *cp,
		vString *const name,
		const tclKind kind)
{
	vStringClear (name);
	while ((int) *cp != '\0'  &&  ! isspace ((int) *cp))
	{
		vStringPut (name, (int) *cp);
		++cp;
	}
	vStringTerminate (name);
	makeSimpleTag (name, TclKinds, kind);
	return cp;
}

static boolean match (const unsigned char *line, const char *word)
{
	return (boolean) (strncmp ((const char*) line, word, strlen (word)) == 0);
}

static void findTclTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char *cp;

		while (isspace (line [0])) 
			++line;
		
		if (line [0] == '\0'  ||  line [0] == '#')
			continue;

		/* read first word */
		for (cp = line ; *cp != '\0'  &&  ! isspace ((int) *cp) ; ++cp)
			;
		if (! isspace ((int) *cp))
			continue;
		while (isspace ((int) *cp))
			++cp;
		/* Now `line' points at first word and `cp' points at next word */

		if (match (line, "proc"))
			cp = makeTclTag (cp, name, K_PROCEDURE);
		else if (match (line, "class") || match (line, "itcl::class"))
			cp = makeTclTag (cp, name, K_CLASS);
		else if (match (line, "public") ||
				match (line, "protected") ||
				match (line, "private"))
		{
			if (match (cp, "method"))
			{
				cp += 6;
				while (isspace ((int) *cp))
					++cp;
				cp = makeTclTag (cp, name, K_METHOD);
			}
		}
	}
	vStringDelete (name);
}

extern parserDefinition* TclParser (void)
{
	static const char *const extensions [] = { "tcl", "tk", "wish", "itcl", NULL };
	parserDefinition* def = parserNew ("Tcl");
	def->kinds      = TclKinds;
	def->kindCount  = KIND_COUNT (TclKinds);
	def->extensions = extensions;
	def->parser     = findTclTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
