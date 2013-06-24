/*
*   $Id: pascal.c 536 2007-06-02 06:09:00Z elliotth $
*
*   Copyright (c) 2001-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for the Pascal language,
*   including some extensions for Object Pascal.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION, K_PROCEDURE
} pascalKind;

static kindOption PascalKinds [] = {
	{ TRUE, 'f', "function",  "functions"},
	{ TRUE, 'p', "procedure", "procedures"}
};

/*
*   FUNCTION DEFINITIONS
*/

static void createPascalTag (
		tagEntryInfo* const tag, const vString* const name, const int kind)
{
	if (PascalKinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    initTagEntry (tag, vStringValue (name));
	    tag->kindName = PascalKinds [kind].name;
	    tag->kind     = PascalKinds [kind].letter;
	}
	else
	    initTagEntry (tag, NULL);
}

static void makePascalTag (const tagEntryInfo* const tag)
{
	if (tag->name != NULL)
		makeTagEntry (tag);
}

static const unsigned char* dbp;

#define starttoken(c) (isalpha ((int) c) || (int) c == '_')
#define intoken(c)    (isalnum ((int) c) || (int) c == '_' || (int) c == '.')
#define endtoken(c)   (! intoken (c)  &&  ! isdigit ((int) c))

static boolean tail (const char *cp)
{
	boolean result = FALSE;
	register int len = 0;

	while (*cp != '\0' && tolower ((int) *cp) == tolower ((int) dbp [len]))
		cp++, len++;
	if (*cp == '\0' && !intoken (dbp [len]))
	{
		dbp += len;
		result = TRUE;
	}
	return result;
}

/* Algorithm adapted from from GNU etags.
 * Locates tags for procedures & functions.  Doesn't do any type- or
 * var-definitions.  It does look for the keyword "extern" or "forward"
 * immediately following the procedure statement; if found, the tag is
 * skipped.
 */
static void findPascalTags (void)
{
	vString *name = vStringNew ();
	tagEntryInfo tag;
	pascalKind kind = K_FUNCTION;
		/* each of these flags is TRUE iff: */
	boolean incomment = FALSE;  /* point is inside a comment */
	int comment_char = '\0';    /* type of current comment */
	boolean inquote = FALSE;    /* point is inside '..' string */
	boolean get_tagname = FALSE;/* point is after PROCEDURE/FUNCTION
		keyword, so next item = potential tag */
	boolean found_tag = FALSE;  /* point is after a potential tag */
	boolean inparms = FALSE;    /* point is within parameter-list */
	boolean verify_tag = FALSE;
		/* point has passed the parm-list, so the next token will determine
		 * whether this is a FORWARD/EXTERN to be ignored, or whether it is a
		 * real tag
		 */

	dbp = fileReadLine ();
	while (dbp != NULL)
	{
		int c = *dbp++;

		if (c == '\0')  /* if end of line */
		{
			dbp = fileReadLine ();
			if (dbp == NULL  ||  *dbp == '\0')
				continue;
			if (!((found_tag && verify_tag) || get_tagname))
				c = *dbp++;
					/* only if don't need *dbp pointing to the beginning of
					 * the name of the procedure or function
					 */
		}
		if (incomment)
		{
			if (comment_char == '{' && c == '}')
				incomment = FALSE;
			else if (comment_char == '(' && c == '*' && *dbp == ')')
			{
				dbp++;
				incomment = FALSE;
			}
			continue;
		}
		else if (inquote)
		{
			if (c == '\'')
				inquote = FALSE;
			continue;
		}
		else switch (c)
		{
			case '\'':
				inquote = TRUE;  /* found first quote */
				continue;
			case '{':  /* found open { comment */
				incomment = TRUE;
				comment_char = c;
				continue;
			case '(':
				if (*dbp == '*')  /* found open (* comment */
				{
					incomment = TRUE;
					comment_char = c;
					dbp++;
				}
				else if (found_tag)  /* found '(' after tag, i.e., parm-list */
					inparms = TRUE;
				continue;
			case ')':  /* end of parms list */
				if (inparms)
					inparms = FALSE;
				continue;
			case ';':
				if (found_tag && !inparms)  /* end of proc or fn stmt */
				{
					verify_tag = TRUE;
					break;
				}
				continue;
		}
		if (found_tag && verify_tag && *dbp != ' ')
		{
			/* check if this is an "extern" declaration */
			if (*dbp == '\0')
				continue;
			if (tolower ((int) *dbp == 'e'))
			{
				if (tail ("extern"))  /* superfluous, really! */
				{
					found_tag = FALSE;
					verify_tag = FALSE;
				}
			}
			else if (tolower ((int) *dbp) == 'f')
			{
				if (tail ("forward"))  /*  check for forward reference */
				{
					found_tag = FALSE;
					verify_tag = FALSE;
				}
			}
			if (found_tag && verify_tag)  /* not external proc, so make tag */
			{
				found_tag = FALSE;
				verify_tag = FALSE;
				makePascalTag (&tag);
				continue;
			}
		}
		if (get_tagname)  /* grab name of proc or fn */
		{
			const unsigned char *cp;

			if (*dbp == '\0')
				continue;

			/* grab block name */
			while (isspace ((int) *dbp))
				++dbp;
			for (cp = dbp  ;  *cp != '\0' && !endtoken (*cp)  ;  cp++)
				continue;
			vStringNCopyS (name, (const char*) dbp,  cp - dbp);
			createPascalTag (&tag, name, kind);
			dbp = cp;  /* set dbp to e-o-token */
			get_tagname = FALSE;
			found_tag = TRUE;
			/* and proceed to check for "extern" */
		}
		else if (!incomment && !inquote && !found_tag)
		{
			switch (tolower ((int) c))
			{
				case 'c':
					if (tail ("onstructor"))
					{
						get_tagname = TRUE;
						kind = K_PROCEDURE;
					}
					break;
				case 'd':
					if (tail ("estructor"))
					{
						get_tagname = TRUE;
						kind = K_PROCEDURE;
					}
					break;
				case 'p':
					if (tail ("rocedure"))
					{
						get_tagname = TRUE;
						kind = K_PROCEDURE;
					}
					break;
				case 'f':
					if (tail ("unction"))
					{
						get_tagname = TRUE;
						kind = K_FUNCTION;
					}
					break;
			}
		}  /* while not eof */
	}
	vStringDelete (name);
}

extern parserDefinition* PascalParser (void)
{
	static const char *const extensions [] = { "p", "pas", NULL };
	parserDefinition* def = parserNew ("Pascal");
	def->extensions = extensions;
	def->kinds      = PascalKinds;
	def->kindCount  = KIND_COUNT (PascalKinds);
	def->parser     = findPascalTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
