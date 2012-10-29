/*
*   $Id: keyword.h 658 2008-04-20 23:21:35Z elliotth $
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to keyword.c
*/
#ifndef _KEYWORD_H
#define _KEYWORD_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "parse.h"

/*
*   FUNCTION PROTOTYPES
*/
extern void addKeyword (const char *const string, langType language, int value);
extern int lookupKeyword (const char *const string, langType language);
extern void freeKeywordTable (void);
#ifdef DEBUG
extern void printKeywordTable (void);
#endif
extern int analyzeToken (vString *const name, langType language);

#endif  /* _KEYWORD_H */

/* vi:set tabstop=4 shiftwidth=4: */
