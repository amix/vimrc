/*
*   $Id$
*
*   Copyright (c) 2008, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for MATLAB language files.
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

static void installMatLabRegex (const langType language)
{
    /* function [x,y,z] = asdf */
    addTagRegex (language, "^function[ \t]*\\[.*\\][ \t]*=[ \t]*([a-zA-Z0-9_]+)", "\\1", "f,function", NULL);
    /* function x = asdf */
    addTagRegex (language, "^function[ \t]*[a-zA-Z0-9_]+[ \t]*=[ \t]*([a-zA-Z0-9_]+)", "\\1", "f,function", NULL);
    /* function asdf */
    addTagRegex (language, "^function[ \t]*([a-zA-Z0-9_]+)[^=]*$", "\\1", "f,function", NULL);
}

extern parserDefinition* MatLabParser ()
{
	static const char *const extensions [] = { "m", NULL };
	parserDefinition* const def = parserNew ("MatLab");
	def->extensions = extensions;
	def->initialize = installMatLabRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
