/*
*   $Id: strlist.h 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to resizable string lists.
*/
#ifndef _STRLIST_H
#define _STRLIST_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef struct sStringList {
	unsigned int max;
	unsigned int count;
	vString    **list;
} stringList;

/*
*   FUNCTION PROTOTYPES
*/
extern stringList *stringListNew (void);
extern void stringListAdd (stringList *const current, vString *string);
extern void stringListRemoveLast (stringList *const current);
extern void stringListCombine (stringList *const current, stringList *const from);
extern stringList* stringListNewFromArgv (const char* const* const list);
extern stringList* stringListNewFromFile (const char* const fileName);
extern void stringListClear (stringList *const current);
extern unsigned int stringListCount (const stringList *const current);
extern vString* stringListItem (const stringList *const current, const unsigned int indx);
extern vString* stringListLast (const stringList *const current);
extern void stringListDelete (stringList *const current);
extern boolean stringListHasInsensitive (const stringList *const current, const char *const string);
extern boolean stringListHas (const stringList *const current, const char *const string);
extern boolean stringListHasTest (const stringList *const current, boolean (*test)(const char *s));
extern boolean stringListRemoveExtension (stringList* const current, const char* const extension);
extern boolean stringListExtensionMatched (const stringList* const list, const char* const extension);
extern boolean stringListFileMatched (const stringList* const list, const char* const str);
extern void stringListPrint (const stringList *const current);

#endif  /* _STRLIST_H */

/* vi:set tabstop=4 shiftwidth=4: */
