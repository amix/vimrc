/*
*   $Id: debug.c 558 2007-06-15 19:17:02Z elliotth $
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains debugging functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <ctype.h>
#include <stdarg.h>

#include "debug.h"
#include "options.h"
#include "read.h"

/*
*   FUNCTION DEFINITIONS
*/

#ifdef DEBUG

extern void lineBreak (void) {}  /* provides a line-specified break point */

extern void debugPrintf (
		const enum eDebugLevels level, const char *const format, ... )
{
	va_list ap;

	va_start (ap, format);
	if (debug (level))
		vprintf (format, ap);
	fflush (stdout);
	va_end (ap);
}

extern void debugPutc (const int level, const int c)
{
	if (debug (level)  &&  c != EOF)
	{
		     if (c == STRING_SYMBOL)  printf ("\"string\"");
		else if (c == CHAR_SYMBOL)    printf ("'c'");
		else                          putchar (c);

		fflush (stdout);
	}
}

extern void debugParseNest (const boolean increase, const unsigned int level)
{
	debugPrintf (DEBUG_PARSE, "<*%snesting:%d*>", increase ? "++" : "--", level);
}

extern void debugCppNest (const boolean begin, const unsigned int level)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s level %d*>", begin ? "begin":"end", level);
}

extern void debugCppIgnore (const boolean ignore)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s ignore*>", ignore ? "begin":"end");
}

extern void debugEntry (const tagEntryInfo *const tag)
{
	const char *const scope = tag->isFileScope ? "{fs}" : "";

	if (debug (DEBUG_PARSE))
	{
		printf ("<#%s%s:%s", scope, tag->kindName, tag->name);

		if (tag->extensionFields.scope [0] != NULL  &&
				tag->extensionFields.scope [1] != NULL)
			printf (" [%s:%s]", tag->extensionFields.scope [0],
					tag->extensionFields.scope [1]);

		if (Option.extensionFields.inheritance  &&
				tag->extensionFields.inheritance != NULL)
			printf (" [inherits:%s]", tag->extensionFields.inheritance);

		if (Option.extensionFields.fileScope &&
				tag->isFileScope && ! isHeaderFile ())
			printf (" [file:]");

		if (Option.extensionFields.access  &&
				tag->extensionFields.access != NULL)
			printf (" [access:%s]", tag->extensionFields.access);

		if (Option.extensionFields.implementation  &&
				tag->extensionFields.implementation != NULL)
			printf (" [imp:%s]", tag->extensionFields.implementation);

		if (Option.extensionFields.typeRef  &&
				tag->extensionFields.typeRef [0] != NULL  &&
				tag->extensionFields.typeRef [1] != NULL)
			printf (" [%s:%s]", tag->extensionFields.typeRef [0],
					tag->extensionFields.typeRef [1]);

		printf ("#>");
		fflush (stdout);
	}
}

#endif

/* vi:set tabstop=4 shiftwidth=4: */
