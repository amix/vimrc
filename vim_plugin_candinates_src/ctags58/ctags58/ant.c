/*
*   $Id$
*
*   Copyright (c) 2008, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Ant language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"

/*
*   FUNCTION DEFINITIONS
*/

static void installAntRegex (const langType language)
{
	addTagRegex (language,
		"^[ \t]*<[ \t]*project.*name=\"([^\"]+)\".*", "\\1", "p,project,projects", NULL);
	addTagRegex (language,
		"^[ \t]*<[ \t]*target.*name=\"([^\"]+)\".*", "\\1", "t,target,targets", NULL);
}

extern parserDefinition* AntParser ()
{
	static const char *const extensions [] = { "build.xml", NULL };
	parserDefinition* const def = parserNew ("Ant");
	def->extensions = extensions;
	def->initialize = installAntRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
