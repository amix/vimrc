/*
*   $Id: args.c 536 2007-06-02 06:09:00Z elliotth $
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for reading command line arguments.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "args.h"
#include "debug.h"
#include "routines.h"

/*
*   FUNCTION DEFINITIONS
*/

static char *nextStringArg (const char** const next)
{
	char* result = NULL;
	const char* start;

	Assert (*next != NULL);
	for (start = *next  ;  isspace ((int) *start)  ;  ++start)
		;
	if (*start == '\0')
		*next = start;
	else
	{
		size_t length;
		const char* end;

		for (end = start ;  *end != '\0'  &&  ! isspace ((int) *end)  ;  ++end)
			;
		length = end - start;
		Assert (length > 0);
		result = xMalloc (length + 1, char);
		strncpy (result, start, length);
		result [length] = '\0';
		*next = end;
	}
	return result;
}

static char* nextStringLine (const char** const next)
{
	char* result = NULL;
	size_t length;
	const char* end;

	Assert (*next != NULL);
	for (end = *next ;  *end != '\n'  &&  *end != '\0' ;  ++end)
		;
	length = end - *next;
	if (length > 0)
	{
		result = xMalloc (length + 1, char);
		strncpy (result, *next, length);
		result [length] = '\0';
	}
	if (*end == '\n')
		++end;
	else if (*end == '\r')
	{
		++end;
		if (*end == '\n')
			++end;
	}
	*next = end;
	return result;
}

static char* nextString (const Arguments* const current, const char** const next)
{
	char* result;
	if (current->lineMode)
		result = nextStringLine (next);
	else
		result = nextStringArg (next);
	return result;
}

static char* nextFileArg (FILE* const fp)
{
	char* result = NULL;
	Assert (fp != NULL);
	if (! feof (fp))
	{
		vString* vs = vStringNew ();
		int c;
		do
			c = fgetc (fp);
		while (isspace (c));

		if (c != EOF)
		{
			do
			{
				vStringPut (vs, c);
				c = fgetc (fp);
			} while (c != EOF  &&  ! isspace (c));
			vStringTerminate (vs);
			Assert (vStringLength (vs) > 0);
			result = xMalloc (vStringLength (vs) + 1, char);
			strcpy (result, vStringValue (vs));
		}
		vStringDelete (vs);
	}
	return result;
}

static char* nextFileLine (FILE* const fp)
{
	char* result = NULL;
	if (! feof (fp))
	{
		vString* vs = vStringNew ();
		int c;

		Assert (fp != NULL);
		c = fgetc (fp);
		while (c != EOF)
		{
			if (c != '\n'  &&  c != '\r')
				vStringPut (vs, c);
			else if (vStringLength (vs) > 0)
				break;
			c = fgetc (fp);
		}
		if (c != EOF  ||  vStringLength (vs) > 0)
		{
			if (c == '\r')
			{
				c = fgetc (fp);
				if (c != '\n')
					c = ungetc (c, fp);
			}
			vStringTerminate (vs);
			vStringStripTrailing (vs);
			result = xMalloc (vStringLength (vs) + 1, char);
			strcpy (result, vStringValue (vs));
		}
		vStringDelete (vs);
	}
	return result;
}

static char* nextFileString (const Arguments* const current, FILE* const fp)
{
	char* result;
	if (current->lineMode)
		result = nextFileLine (fp);
	else
		result = nextFileArg (fp);
	return result;
}

extern Arguments* argNewFromString (const char* const string)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_STRING;
	result->u.stringArgs.string = string;
	result->u.stringArgs.item = string;
	result->u.stringArgs.next = string;
	result->item = nextString (result, &result->u.stringArgs.next);
	return result;
}

extern Arguments* argNewFromArgv (char* const* const argv)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_ARGV;
	result->u.argvArgs.argv = argv;
	result->u.argvArgs.item = result->u.argvArgs.argv;
	result->item = *result->u.argvArgs.item;
	return result;
}

extern Arguments* argNewFromFile (FILE* const fp)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_FILE;
	result->u.fileArgs.fp = fp;
	result->item = nextFileString (result, result->u.fileArgs.fp);
	return result;
}

extern Arguments* argNewFromLineFile (FILE* const fp)
{
	Arguments* result = xMalloc (1, Arguments);
	memset (result, 0, sizeof (Arguments));
	result->type = ARG_FILE;
	result->lineMode = TRUE;
	result->u.fileArgs.fp = fp;
	result->item = nextFileString (result, result->u.fileArgs.fp);
	return result;
}

extern char *argItem (const Arguments* const current)
{
	Assert (current != NULL);
	Assert (! argOff (current));
	return current->item;
}

extern boolean argOff (const Arguments* const current)
{
	Assert (current != NULL);
	return (boolean) (current->item == NULL);
}

extern void argSetWordMode (Arguments* const current)
{
	Assert (current != NULL);
	current->lineMode = FALSE;
}

extern void argSetLineMode (Arguments* const current)
{
	Assert (current != NULL);
	current->lineMode = TRUE;
}

extern void argForth (Arguments* const current)
{
	Assert (current != NULL);
	Assert (! argOff (current));
	switch (current->type)
	{
		case ARG_STRING:
			if (current->item != NULL)
				eFree (current->item);
			current->u.stringArgs.item = current->u.stringArgs.next;
			current->item = nextString (current, &current->u.stringArgs.next);
			break;
		case ARG_ARGV:
			++current->u.argvArgs.item;
			current->item = *current->u.argvArgs.item;
			break;
		case ARG_FILE:
			if (current->item != NULL)
				eFree (current->item);
			current->item = nextFileString (current, current->u.fileArgs.fp);
			break;
		default:
			Assert ("Invalid argument type" == NULL);
			break;
	}
}

extern void argDelete (Arguments* const current)
{
	Assert (current != NULL);
	if (current->type ==  ARG_STRING  &&  current->item != NULL)
		eFree (current->item);
	memset (current, 0, sizeof (Arguments));
	eFree (current);
}

/* vi:set tabstop=4 shiftwidth=4: */
