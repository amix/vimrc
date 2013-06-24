/*
*   $Id: vstring.c 558 2007-06-15 19:17:02Z elliotth $
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions supporting resizeable strings.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <limits.h>  /* to define INT_MAX */
#include <string.h>
#include <ctype.h>

#include "debug.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
static const size_t vStringInitialSize = 32;

/*
*   FUNCTION DEFINITIONS
*/

static void vStringResize (vString *const string, const size_t newSize)
{
	char *const newBuffer = xRealloc (string->buffer, newSize, char);

	string->size = newSize;
	string->buffer = newBuffer;
}

/*
*   External interface
*/

extern boolean vStringAutoResize (vString *const string)
{
	boolean ok = TRUE;

	if (string->size <= INT_MAX / 2)
	{
		const size_t newSize = string->size * 2;

		vStringResize (string, newSize);
	}
	return ok;
}

extern void vStringClear (vString *const string)
{
	string->length = 0;
	string->buffer [0] = '\0';
	DebugStatement ( memset (string->buffer, 0, string->size); )
}

extern void vStringDelete (vString *const string)
{
	if (string != NULL)
	{
		if (string->buffer != NULL)
			eFree (string->buffer);
		eFree (string);
	}
}

extern vString *vStringNew (void)
{
	vString *const string = xMalloc (1, vString);

	string->length = 0;
	string->size   = vStringInitialSize;
	string->buffer = xMalloc (string->size, char);

	vStringClear (string);

	return string;
}

#ifndef VSTRING_PUTC_MACRO
extern void vStringPut (vString *const string, const int c)
{
	if (string->length + 1 == string->size)  /*  check for buffer overflow */
		vStringAutoResize (string);

	string->buffer [string->length] = c;
	if (c != '\0')
		string->buffer [++string->length] = '\0';
}
#endif

extern void vStringCatS (vString *const string, const char *const s)
{
#if 1
	const size_t len = strlen (s);
	while (string->length + len + 1 >= string->size)/*  check for buffer overflow */
		vStringAutoResize (string);
	strcpy (string->buffer + string->length, s);
	string->length += len;
#else
	const char *p = s;
	do
		vStringPut (string, *p);
	while (*p++ != '\0');
#endif
}

extern vString *vStringNewCopy (const vString *const string)
{
	vString *vs = vStringNew ();
	vStringCatS (vs, string->buffer);
	return vs;
}

extern vString *vStringNewInit (const char *const s)
{
	vString *vs = vStringNew ();
	vStringCatS (vs, s);
	return vs;
}

extern void vStringNCatS (
		vString *const string, const char *const s, const size_t length)
{
	const char *p = s;
	size_t remain = length;

	while (*p != '\0'  &&  remain > 0)
	{
		vStringPut (string, *p);
		--remain;
		++p;
	}
	vStringTerminate (string);
}

/*  Strip trailing newline from string.
 */
extern void vStringStripNewline (vString *const string)
{
	const size_t final = string->length - 1;
	if (string->buffer [final] == '\n')
	{
		string->buffer [final] = '\0';
		string->length--;
	}
}

/*  Strip leading white space from string.
 */
extern void vStringStripLeading (vString *const string)
{
	while (isspace ((int) string->buffer [0]) && string->length > 0)
	{
		size_t i;
		for (i = 1  ;  i < string->length  ;  ++i)
			string->buffer [i - 1] = string->buffer [i];
		--string->length;
		string->buffer [string->length] = '\0';
	}
}

/*  Strip trailing white space from string.
 */
extern void vStringStripTrailing (vString *const string)
{
	while (isspace ((int) string->buffer [string->length - 1]) &&
		   string->length > 0)
	{
		string->length--;
		string->buffer [string->length] = '\0';
	}
}

/*  Chop last character from string.
 */
extern void vStringChop (vString *const string)
{
	if (string->length > 0)
	{
		--string->length;
		string->buffer [string->length] = '\0';
	}
}

extern void vStringCopyS (vString *const string, const char *const s)
{
	vStringClear (string);
	vStringCatS (string, s);
}

extern void vStringNCopyS (
		vString *const string, const char *const s, const size_t length)
{
	vStringClear (string);
	vStringNCatS (string, s, length);
}

extern void vStringCopyToLower (vString *const dest, const vString *const src)
{
	const size_t length = src->length;
	const char *s = src->buffer;
	char *d;
	size_t i;

	if (dest->size < src->size)
		vStringResize (dest, src->size);
	d = dest->buffer;
	for (i = 0  ;  i < length  ;  ++i)
	{
		int c = s [i];

		d [i] = tolower (c);
	}
	d [i] = '\0';
}

extern void vStringSetLength (vString *const string)
{
	string->length = strlen (string->buffer);
}

/* vi:set tabstop=4 shiftwidth=4: */
