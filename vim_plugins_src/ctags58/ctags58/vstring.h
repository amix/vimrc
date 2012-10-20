/*
*   $Id: vstring.h 719 2009-07-07 03:46:59Z dhiebert $
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Provides the external interface for resizeable strings.
*/
#ifndef _VSTRING_H
#define _VSTRING_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#if defined(HAVE_STDLIB_H)
# include <stdlib.h>  /* to define size_t */
#endif

/*
*   MACROS
*/
#ifndef DEBUG
# define VSTRING_PUTC_MACRO 1
#endif
#ifdef VSTRING_PUTC_MACRO
#define vStringPut(s,c) \
	(void)(((s)->length + 1 == (s)->size ? vStringAutoResize (s) : 0), \
	((s)->buffer [(s)->length] = (c)), \
	((c) == '\0' ? 0 : ((s)->buffer [++(s)->length] = '\0')))
#endif

#define vStringValue(vs)      ((vs)->buffer)
#define vStringItem(vs,i)     ((vs)->buffer[i])
#define vStringLast(vs)       ((vs)->buffer[(vs)->length - 1])
#define vStringLength(vs)     ((vs)->length)
#define vStringSize(vs)       ((vs)->size)
#define vStringCat(vs,s)      vStringCatS((vs), vStringValue((s)))
#define vStringNCat(vs,s,l)   vStringNCatS((vs), vStringValue((s)), (l))
#define vStringCopy(vs,s)     vStringCopyS((vs), vStringValue((s)))
#define vStringNCopy(vs,s,l)  vStringNCopyS((vs), vStringValue((s)), (l))
#define vStringChar(vs,i)     ((vs)->buffer[i])
#define vStringTerminate(vs)  vStringPut(vs, '\0')
#define vStringLower(vs)      toLowerString((vs)->buffer)
#define vStringUpper(vs)      toUpperString((vs)->buffer)

/*
*   DATA DECLARATIONS
*/

typedef struct sVString {
	size_t  length;  /* size of buffer used */
	size_t  size;    /* allocated size of buffer */
	char   *buffer;  /* location of buffer */
} vString;

/*
*   FUNCTION PROTOTYPES
*/
extern boolean vStringAutoResize (vString *const string);
extern void vStringClear (vString *const string);
extern vString *vStringNew (void);
extern void vStringDelete (vString *const string);
#ifndef VSTRING_PUTC_MACRO
extern void vStringPut (vString *const string, const int c);
#endif
extern void vStringStripNewline (vString *const string);
extern void vStringStripLeading (vString *const string);
extern void vStringChop (vString *const string);
extern void vStringStripTrailing (vString *const string);
extern void vStringCatS (vString *const string, const char *const s);
extern void vStringNCatS (vString *const string, const char *const s, const size_t length);
extern vString *vStringNewCopy (const vString *const string);
extern vString *vStringNewInit (const char *const s);
extern void vStringCopyS (vString *const string, const char *const s);
extern void vStringNCopyS (vString *const string, const char *const s, const size_t length);
extern void vStringCopyToLower (vString *const dest, const vString *const src);
extern void vStringSetLength (vString *const string);

#endif  /* _VSTRING_H */

/* vi:set tabstop=4 shiftwidth=4: */
