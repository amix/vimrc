/*
*   $Id: rexx.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2001-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for the REXX language
*   (http://www.rexxla.org, http://www2.hursley.ibm.com/rexx).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* always include first */
#include "parse.h"    /* always include */

/*
*   FUNCTION DEFINITIONS
*/

static void installRexxRegex (const langType language)
{
	addTagRegex (language, "^([A-Za-z0-9@#$\\.!?_]+)[ \t]*:",
		"\\1", "s,subroutine,subroutines", NULL);
}

extern parserDefinition* RexxParser (void)
{
	static const char *const extensions [] = { "cmd", "rexx", "rx", NULL };
	parserDefinition* const def = parserNew ("REXX");
	def->extensions = extensions;
	def->initialize = installRexxRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
