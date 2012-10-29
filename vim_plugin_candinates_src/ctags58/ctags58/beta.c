/*
*   $Id: beta.c 536 2007-06-02 06:09:00Z elliotth $
*
*   Copyright (c) 1999-2000, Mjølner Informatics
*
*   Written by Erik Corry <corry@mjolner.dk>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for BETA language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   MACROS
*/
#define isbident(c) (identarray [(unsigned char) (c)])

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FRAGMENT, K_PATTERN, K_SLOT, K_VIRTUAL
} betaKind;

static kindOption BetaKinds [] = {
	{ TRUE,  'f', "fragment", "fragment definitions"},
	{ FALSE, 'p', "pattern",  "all patterns"},
	{ TRUE,  's', "slot",     "slots (fragment uses)"},
	{ TRUE,  'v', "virtual",  "patterns (virtual or rebound)"}
};

/* [A-Z_a-z0-9] */
static const char identarray [256] = {
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 0-15  */
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 16-31 */
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 32-47    !"#$%&'()*+'-./ */
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,  /* 48-63   0123456789:;<=>? */
0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* 64-79   @ABCDEFGHIJKLMNO */
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,  /* 80-95   PQRSTUVWXYZ [\]^_ */
0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* 96-111  `abcdefghijklmno */
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,  /* 112-127  pqrstuvwxyz{|}~ */
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 128-  */
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}; /* -255  */

/*
*   FUNCTION DEFINITIONS
*/

static void makeBetaTag (const char* const name, const betaKind kind)
{
	if (BetaKinds [kind].enabled)
	{
		tagEntryInfo e;
		initTagEntry (&e, name);
		e.kindName = BetaKinds [kind].name;
		e.kind     = BetaKinds [kind].letter;
		makeTagEntry (&e);
	}
}

static void findBetaTags (void)
{
	vString *line = vStringNew ();
	boolean incomment = FALSE;
	boolean inquote = FALSE;
	boolean dovirtuals = BetaKinds [K_VIRTUAL].enabled;
	boolean dopatterns = BetaKinds [K_PATTERN].enabled;

	do
	{
		boolean foundfragmenthere = FALSE;
		/* find fragment definition (line that starts and ends with --) */
		int last;
		int first;
		int c;

		vStringClear (line);

		while ((c = fileGetc ()) != EOF && c != '\n' && c != '\r')
			vStringPut (line, c);

		vStringTerminate (line);

		last = vStringLength (line) - 1;
		first = 0;
		/* skip white space at start and end of line */
		while (last && isspace ((int) vStringChar (line, last))) last--;
		while (first < last && isspace ((int) vStringChar (line, first))) first++;
		/* if line still has a reasonable length and ... */
		if (last - first > 4 &&
			(vStringChar (line, first)     == '-' && 
			 vStringChar (line, first + 1) == '-' && 
			 vStringChar (line, last)      == '-' && 
			 vStringChar (line, last - 1)  == '-'))
		{
			if (!incomment && !inquote)
			{
				foundfragmenthere = TRUE;
				/* skip past -- and whitespace.  Also skip back past 'dopart'
				   or 'attributes' to the :.  We have to do this because there
				   is no sensible way to include whitespace in a ctags token
				   so the conventional space after the ':' would mess us up */
				last -= 2;
				first += 2;
				while (last && vStringChar (line, last) != ':') last--;
				while (last && (isspace ((int) vStringChar (line, last-1)))) last--;
				while (first < last &&
					   (isspace ((int) vStringChar (line, first)) ||
						vStringChar (line, first) == '-'))
					first++;
				/* If there's anything left it is a fragment title */
				if (first < last - 1)
				{
					vStringChar (line, last) = 0;
					if (strcasecmp ("LIB", vStringValue (line) + first) &&
						strcasecmp ("PROGRAM", vStringValue (line) + first))
					{
						makeBetaTag (vStringValue (line) + first, K_FRAGMENT);
					}
				}
			}
		} else {
			int pos = 0;
			int len = vStringLength (line);
			if (inquote) goto stringtext;
			if (incomment) goto commenttext;
		programtext:
			for ( ; pos < len; pos++)
			{
				if (vStringChar (line, pos) == '\'')
				{
					pos++;
					inquote = TRUE;
					goto stringtext;
				}
				if (vStringChar (line, pos) == '{')
				{
					pos++;
					incomment = TRUE;
					goto commenttext;
				}
				if (vStringChar (line, pos) == '(' && pos < len - 1 &&
					vStringChar (line, pos+1) == '*')
				{
					pos +=2;
					incomment = TRUE;
					goto commenttext;
				}
				/*
				 * SLOT definition looks like this: 
				 * <<SLOT nameofslot: dopart>> 
				 * or
				 * <<SLOT nameofslot: descriptor>> 
				 */
				if (!foundfragmenthere &&
					vStringChar (line, pos) == '<' &&
					pos+1 < len &&
					vStringChar (line, pos+1) == '<' &&
					strstr (vStringValue (line) + pos, ">>"))
				{
					/* Found slot name, get start and end */
					int eoname;
					char c2;
					pos += 2; /* skip past << */
					/* skip past space before SLOT */
					while (pos < len && isspace ((int) vStringChar (line, pos)))
						pos++;
					/* skip past SLOT */
					if (pos+4 <= len &&
						!strncasecmp (vStringValue(line) + pos, "SLOT", (size_t)4))
						pos += 4;
					/* skip past space after SLOT */
					while (pos < len && isspace ((int) vStringChar (line, pos)))
						pos++;
					eoname = pos;
					/* skip to end of name */
					while (eoname < len &&
							(c2 = vStringChar (line, eoname)) != '>' &&
							c2 != ':' &&
							!isspace ((int) c2))
						eoname++;
					if (eoname < len)
					{
						vStringChar (line, eoname) = 0;
						if (strcasecmp ("LIB", vStringValue (line) + pos) &&
							strcasecmp ("PROGRAM", vStringValue (line) + pos) &&
							strcasecmp ("SLOT", vStringValue (line) + pos))
						{
							makeBetaTag (vStringValue (line) + pos, K_SLOT);
						}
					}
					if (eoname+1 < len) {
						pos = eoname + 1;
					} else {
						pos = len;
						continue;
					}
				}
				/* Only patterns that are virtual, extensions of virtuals or
				 * final bindings are normally included so as not to overload
	             * totally.
				 * That means one of the forms name:: name:< or name::<
				 */
				if (!foundfragmenthere &&
					vStringChar (line, pos) == ':' &&
	                (dopatterns ||
					 (dovirtuals &&
					  (vStringChar (line, pos+1) == ':' ||
					   vStringChar (line, pos+1) == '<')
					 )
					)
	               )
				{
					/* Found pattern name, get start and end */
					int eoname = pos;
					int soname;
					while (eoname && isspace ((int) vStringChar (line, eoname-1)))
						eoname--;
				foundanothername:
					/* terminate right after name */
					vStringChar (line, eoname) = 0;
					soname = eoname;
					while (soname &&
						isbident (vStringChar (line, soname-1)))
					{
						soname--;
					}
					if (soname != eoname)
					{
						makeBetaTag (vStringValue (line) + soname, K_PATTERN);
						/* scan back past white space */
						while (soname &&
								isspace ((int) vStringChar (line, soname-1)))
							soname--;
						if (soname && vStringChar (line, soname-1) == ',')
						{
							/* we found a new pattern name before comma */
							eoname = soname;
							goto foundanothername;
						}
					}
				}
			}
			goto endofline;
		commenttext:
			for ( ; pos < len; pos++)
			{
				if (vStringChar (line, pos) == '*' && pos < len - 1 &&
					vStringChar (line, pos+1) == ')')
				{
					pos += 2;
					incomment = FALSE;
					goto programtext;
				}
				if (vStringChar (line, pos) == '}')
				{
					pos++;
					incomment = FALSE;
					goto programtext;
				}
			}
			goto endofline;
		stringtext:
			for ( ; pos < len; pos++)
			{
				if (vStringChar (line, pos) == '\\')
				{
					if (pos < len - 1) pos++;
				}
				else if (vStringChar (line, pos) == '\'')
				{
					pos++;
					/* support obsolete '' syntax */
					if (pos < len && vStringChar (line, pos) == '\'')
					{
						continue;
					}
					inquote = FALSE;
					goto programtext;
				}
			}
		}
		endofline:
		inquote = FALSE;  /* This shouldn't really make a difference */
	} while (!feof (File.fp));
	vStringDelete (line);
}

extern parserDefinition* BetaParser (void)
{
	static const char *const extensions [] = { "bet", NULL };
	parserDefinition* def = parserNew ("BETA");
	def->kinds      = BetaKinds;
	def->kindCount  = KIND_COUNT (BetaKinds);
	def->extensions = extensions;
	def->parser     = findBetaTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
