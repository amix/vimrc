/*
 *   $Id: slang.c 443 2006-05-30 04:37:13Z darren $
 *
 *   Copyright (c) 2000-2001, Francesc Rocher
 *
 *   Author: Francesc Rocher <f.rocher@computer.org>.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for S-Lang files.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */
#include "parse.h"

/*
 *   FUNCTION DEFINITIONS
 */
static void installSlangRegex (const langType language)
{
	addTagRegex (language,
		"^.*define[ \t]+([A-Z_][A-Z0-9_]*)[^;]*$",
		"\\1", "f,function,functions", "i");
	addTagRegex (language,
		"^[ \t]*implements[ \t]+\\([ \t]*\"([^\"]*)\"[ \t]*\\)[ \t]*;",
		"\\1", "n,namespace,namespaces", NULL);
}

extern parserDefinition* SlangParser (void)
{
	static const char *const extensions [] = { "sl", NULL };
	parserDefinition* const def = parserNew ("SLang");
	def->extensions = extensions;
	def->initialize = installSlangRegex;
	def->regex      = TRUE;
	return def;
}
