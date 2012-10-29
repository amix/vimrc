/*
*   $Id: keyword.c 715 2009-07-06 03:31:00Z dhiebert $
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Manages a keyword hash.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "keyword.h"
#include "options.h"
#include "routines.h"

/*
*   MACROS
*/
#define HASH_EXPONENT 7  /* must be less than 17 */

/*
*   DATA DECLARATIONS
*/
typedef struct sHashEntry {
	struct sHashEntry *next;
	const char *string;
	langType language;
	int value;
} hashEntry;

/*
*   DATA DEFINITIONS
*/
static const unsigned int TableSize = 1 << HASH_EXPONENT;
static hashEntry **HashTable = NULL;

/*
*   FUNCTION DEFINITIONS
*/

static hashEntry **getHashTable (void)
{
	static boolean allocated = FALSE;

	if (! allocated)
	{
		unsigned int i;

		HashTable = xMalloc (TableSize, hashEntry*);

		for (i = 0  ;  i < TableSize  ;  ++i)
			HashTable [i] = NULL;

		allocated = TRUE;
	}
	return HashTable;
}

static hashEntry *getHashTableEntry (unsigned long hashedValue)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry;

	Assert (hashedValue < TableSize);
	entry = table [hashedValue];

	return entry;
}

static unsigned long hashValue (const char *const string)
{
	unsigned long value = 0;
	const unsigned char *p;

	Assert (string != NULL);

	/*  We combine the various words of the multiword key using the method
	 *  described on page 512 of Vol. 3 of "The Art of Computer Programming".
	 */
	for (p = (const unsigned char *) string  ;  *p != '\0'  ;  ++p)
	{
		value <<= 1;
		if (value & 0x00000100L)
			value = (value & 0x000000ffL) + 1L;
		value ^= *p;
	}
	/*  Algorithm from page 509 of Vol. 3 of "The Art of Computer Programming"
	 *  Treats "value" as a 16-bit integer plus 16-bit fraction.
	 */
	value *= 40503L;               /* = 2^16 * 0.6180339887 ("golden ratio") */
	value &= 0x0000ffffL;          /* keep fractional part */
	value >>= 16 - HASH_EXPONENT;  /* scale up by hash size and move down */

	return value;
}

static hashEntry *newEntry (
		const char *const string, langType language, int value)
{
	hashEntry *const entry = xMalloc (1, hashEntry);

	entry->next     = NULL;
	entry->string   = string;
	entry->language = language;
	entry->value    = value;

	return entry;
}

/*  Note that it is assumed that a "value" of zero means an undefined keyword
 *  and clients of this function should observe this. Also, all keywords added
 *  should be added in lower case. If we encounter a case-sensitive language
 *  whose keywords are in upper case, we will need to redesign this.
 */
extern void addKeyword (const char *const string, langType language, int value)
{
	const unsigned long hashedValue = hashValue (string);
	hashEntry *entry = getHashTableEntry (hashedValue);

	if (entry == NULL)
	{
		hashEntry **const table = getHashTable ();
		table [hashedValue] = newEntry (string, language, value);
	}
	else
	{
		hashEntry *prev = NULL;

		while (entry != NULL)
		{
			if (language == entry->language  &&
				strcmp (string, entry->string) == 0)
			{
				Assert (("Already in table" == NULL));
			}
			prev = entry;
			entry = entry->next;
		}
		if (entry == NULL)
		{
			Assert (prev != NULL);
			prev->next = newEntry (string, language, value);
		}
	}
}

extern int lookupKeyword (const char *const string, langType language)
{
	const unsigned long hashedValue = hashValue (string);
	hashEntry *entry = getHashTableEntry (hashedValue);
	int result = -1;

	while (entry != NULL)
	{
		if (language == entry->language  &&  strcmp (string, entry->string) == 0)
		{
			result = entry->value;
			break;
		}
		entry = entry->next;
	}
	return result;
}

extern void freeKeywordTable (void)
{
	if (HashTable != NULL)
	{
		unsigned int i;

		for (i = 0  ;  i < TableSize  ;  ++i)
		{
			hashEntry *entry = HashTable [i];

			while (entry != NULL)
			{
				hashEntry *next = entry->next;
				eFree (entry);
				entry = next;
			}
		}
		eFree (HashTable);
	}
}

extern int analyzeToken (vString *const name, langType language)
{
	vString *keyword = vStringNew ();
	int result;
	vStringCopyToLower (keyword, name);
	result = lookupKeyword (vStringValue (keyword), language);
	vStringDelete (keyword);
	return result;
}

#ifdef DEBUG

static void printEntry (const hashEntry *const entry)
{
	printf ("  %-15s %-7s\n", entry->string, getLanguageName (entry->language));
}

static unsigned int printBucket (const unsigned int i)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry = table [i];
	unsigned int measure = 1;
	boolean first = TRUE;

	printf ("%2d:", i);
	if (entry == NULL)
		printf ("\n");
	else while (entry != NULL)
	{
		if (! first)
			printf ("    ");
		else
		{
			printf (" ");
			first = FALSE;
		}
		printEntry (entry);
		entry = entry->next;
		measure = 2 * measure;
	}
	return measure - 1;
}

extern void printKeywordTable (void)
{
	unsigned long emptyBucketCount = 0;
	unsigned long measure = 0;
	unsigned int i;

	for (i = 0  ;  i < TableSize  ;  ++i)
	{
		const unsigned int pass = printBucket (i);

		measure += pass;
		if (pass == 0)
			++emptyBucketCount;
	}

	printf ("spread measure = %ld\n", measure);
	printf ("%ld empty buckets\n", emptyBucketCount);
}

#endif

/* vi:set tabstop=4 shiftwidth=4: */
