/*
*   $Id: lua.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2000-2001, Max Ischenko <mfi@ukr.net>.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Lua language.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "options.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION
} luaKind;

static kindOption LuaKinds [] = {
	{ TRUE, 'f', "function", "functions" }
};

/*
*   FUNCTION DEFINITIONS
*/

/* for debugging purposes */
static void __unused__ print_string (char *p, char *q)
{
	for ( ; p != q; p++)
		fprintf (errout, "%c", *p);
	fprintf (errout, "\n");
}

/*
 * Helper function.
 * Returns 1 if line looks like a line of Lua code.
 *
 * TODO: Recognize UNIX bang notation.
 * (Lua treat first line as a comment if it starts with #!)
 *
 */
static boolean is_a_code_line (const unsigned char *line)
{
	boolean result;
	const unsigned char *p = line;
	while (isspace ((int) *p))
		p++;
	if (p [0] == '\0')
		result = FALSE;
	else if (p [0] == '-' && p [1] == '-')
		result = FALSE;
	else
		result = TRUE;
	return result;
}

static void extract_name (const char *begin, const char *end, vString *name)
{
	if (begin != NULL  &&  end != NULL  &&  begin < end)
	{
		const char *cp;

		while (isspace ((int) *begin))
			begin++;
		while (isspace ((int) *end))
			end--;
		if (begin < end)
		{
			for (cp = begin ; cp != end; cp++)
				vStringPut (name, (int) *cp);
			vStringTerminate (name);

			makeSimpleTag (name, LuaKinds, K_FUNCTION);
			vStringClear (name);
		}
	}
}

static void findLuaTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		const char *p, *q;

		if (! is_a_code_line (line))
			continue;

		p = (const char*) strstr ((const char*) line, "function");
		if (p == NULL)
			continue;

		q = strchr ((const char*) line, '=');
		
		if (q == NULL) {
			p = p + 9;  /* skip the `function' word */
			q = strchr ((const char*) p, '(');
			extract_name (p, q, name);
		} else {
			p = (const char*) &line[0];
			extract_name (p, q, name);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* LuaParser (void)
{
	static const char* const extensions [] = { "lua", NULL };
	parserDefinition* def = parserNew ("Lua");
	def->kinds      = LuaKinds;
	def->kindCount  = KIND_COUNT (LuaKinds);
	def->extensions = extensions;
	def->parser     = findLuaTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
