/*
*   $Id: perl.c 601 2007-08-02 04:45:16Z perlguy0 $
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for PERL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#define TRACE_PERL_C 0
#define TRACE if (TRACE_PERL_C) printf("perl.c:%d: ", __LINE__), printf

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_NONE = -1,
	K_CONSTANT,
	K_FORMAT,
	K_LABEL,
	K_PACKAGE,
	K_SUBROUTINE,
	K_SUBROUTINE_DECLARATION
} perlKind;

static kindOption PerlKinds [] = {
	{ TRUE,  'c', "constant",               "constants" },
	{ TRUE,  'f', "format",                 "formats" },
	{ TRUE,  'l', "label",                  "labels" },
	{ TRUE,  'p', "package",                "packages" },
	{ TRUE,  's', "subroutine",             "subroutines" },
	{ FALSE, 'd', "subroutine declaration", "subroutine declarations" },
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isIdentifier1 (int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentifier (int c)
{
	return (boolean) (isalnum (c) || c == '_');
}

static boolean isPodWord (const char *word)
{
	boolean result = FALSE;
	if (isalpha (*word))
	{
		const char *const pods [] = {
			"head1", "head2", "head3", "head4", "over", "item", "back",
			"pod", "begin", "end", "for"
		};
		const size_t count = sizeof (pods) / sizeof (pods [0]);
		const char *white = strpbrk (word, " \t");
		const size_t len = (white!=NULL) ? (size_t)(white-word) : strlen (word);
		char *const id = (char*) eMalloc (len + 1);
		size_t i;
		strncpy (id, word, len);
		id [len] = '\0';
		for (i = 0  ;  i < count  &&  ! result  ;  ++i)
		{
			if (strcmp (id, pods [i]) == 0)
				result = TRUE;
		}
		eFree (id);
	}
	return result;
}

/*
 * Perl subroutine declaration may look like one of the following:
 *
 *  sub abc;
 *  sub abc :attr;
 *  sub abc (proto);
 *  sub abc (proto) :attr;
 *
 * Note that there may be more than one attribute.  Attributes may
 * have things in parentheses (they look like arguments).  Anything
 * inside of those parentheses goes.  Prototypes may contain semi-colons.
 * The matching end when we encounter (outside of any parentheses) either
 * a semi-colon (that'd be a declaration) or an left curly brace
 * (definition).
 *
 * This is pretty complicated parsing (plus we all know that only perl can
 * parse Perl), so we are only promising best effort here.
 *
 * If we can't determine what this is (due to a file ending, for example),
 * we will return FALSE.
 */
static boolean isSubroutineDeclaration (const unsigned char *cp)
{
	boolean attr = FALSE;
	int nparens = 0;

	do {
		for ( ; *cp; ++cp) {
SUB_DECL_SWITCH:
			switch (*cp) {
				case ':':
					if (nparens)
						break;
					else if (TRUE == attr)
						return FALSE;    /* Invalid attribute name */
					else
						attr = TRUE;
					break;
				case '(':
					++nparens;
					break;
				case ')':
					--nparens;
					break;
				case ' ':
				case '\t':
					break;
				case ';':
					if (!nparens)
						return TRUE;
				case '{':
					if (!nparens)
						return FALSE;
				default:
					if (attr) {
						if (isIdentifier1(*cp)) {
							cp++;
							while (isIdentifier (*cp))
								cp++;
							attr = FALSE;
							goto SUB_DECL_SWITCH; /* Instead of --cp; */
						} else {
							return FALSE;
						}
					} else if (nparens) {
						break;
					} else {
						return FALSE;
					}
			}
		}
	} while (NULL != (cp = fileReadLine ()));

	return FALSE;
}

/* Algorithm adapted from from GNU etags.
 * Perl support by Bart Robinson <lomew@cs.utah.edu>
 * Perl sub names: look for /^ [ \t\n]sub [ \t\n]+ [^ \t\n{ (]+/
 */
static void findPerlTags (void)
{
	vString *name = vStringNew ();
	vString *package = NULL;
	boolean skipPodDoc = FALSE;
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		boolean spaceRequired = FALSE;
		boolean qualified = FALSE;
		const unsigned char *cp = line;
		perlKind kind = K_NONE;
		tagEntryInfo e;

		if (skipPodDoc)
		{
			if (strncmp ((const char*) line, "=cut", (size_t) 4) == 0)
				skipPodDoc = FALSE;
			continue;
		}
		else if (line [0] == '=')
		{
			skipPodDoc = isPodWord ((const char*)line + 1);
			continue;
		}
		else if (strcmp ((const char*) line, "__DATA__") == 0)
			break;
		else if (strcmp ((const char*) line, "__END__") == 0)
			break;
		else if (line [0] == '#')
			continue;

		while (isspace (*cp))
			cp++;

		if (strncmp((const char*) cp, "sub", (size_t) 3) == 0)
		{
			TRACE("this looks like a sub\n");
			cp += 3;
			kind = K_SUBROUTINE;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else if (strncmp((const char*) cp, "use", (size_t) 3) == 0)
		{
			cp += 3;
			if (!isspace(*cp))
				continue;
			while (*cp && isspace (*cp))
				++cp;
			if (strncmp((const char*) cp, "constant", (size_t) 8) != 0)
				continue;
			cp += 8;
			kind = K_CONSTANT;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else if (strncmp((const char*) cp, "package", (size_t) 7) == 0)
		{
			/* This will point to space after 'package' so that a tag
			   can be made */
			const unsigned char *space = cp += 7;

			if (package == NULL)
				package = vStringNew ();
			else
				vStringClear (package);
			while (isspace (*cp))
				cp++;
			while ((int) *cp != ';'  &&  !isspace ((int) *cp))
			{
				vStringPut (package, (int) *cp);
				cp++;
			}
			vStringCatS (package, "::");

			cp = space;	 /* Rewind */
			kind = K_PACKAGE;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else if (strncmp((const char*) cp, "format", (size_t) 6) == 0)
		{
			cp += 6;
			kind = K_FORMAT;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else
		{
			if (isIdentifier1 (*cp))
			{
				const unsigned char *p = cp;
				while (isIdentifier (*p))
					++p;
				while (isspace (*p))
					++p;
				if ((int) *p == ':' && (int) *(p + 1) != ':')
					kind = K_LABEL;
			}
		}
		if (kind != K_NONE)
		{
			TRACE("cp0: %s\n", (const char *) cp);
			if (spaceRequired && *cp && !isspace (*cp))
				continue;

			TRACE("cp1: %s\n", (const char *) cp);
			while (isspace (*cp))
				cp++;

			while (!*cp || '#' == *cp) { /* Gobble up empty lines
				                            and comments */
				cp = fileReadLine ();
				if (!cp)
					goto END_MAIN_WHILE;
				while (isspace (*cp))
					cp++;
			}

			while (isIdentifier (*cp) || (K_PACKAGE == kind && ':' == *cp))
			{
				vStringPut (name, (int) *cp);
				cp++;
			}

			if (K_FORMAT == kind &&
				vStringLength (name) == 0 && /* cp did not advance */
				'=' == *cp)
			{
				/* format's name is optional.  If it's omitted, 'STDOUT'
				   is assumed. */
				vStringCatS (name, "STDOUT");
			}

			vStringTerminate (name);
			TRACE("name: %s\n", name->buffer);

			if (0 == vStringLength(name)) {
				vStringClear(name);
				continue;
			}

			if (K_SUBROUTINE == kind)
			{
				/*
				 * isSubroutineDeclaration() may consume several lines.  So
				 * we record line positions.
				 */
				initTagEntry(&e, vStringValue(name));

				if (TRUE == isSubroutineDeclaration(cp)) {
					if (TRUE == PerlKinds[K_SUBROUTINE_DECLARATION].enabled) {
						kind = K_SUBROUTINE_DECLARATION;
					} else {
						vStringClear (name);
						continue;
					}
				}

				e.kind     = PerlKinds[kind].letter;
				e.kindName = PerlKinds[kind].name;

				makeTagEntry(&e);

				if (Option.include.qualifiedTags && qualified &&
					package != NULL  && vStringLength (package) > 0)
				{
					vString *const qualifiedName = vStringNew ();
					vStringCopy (qualifiedName, package);
					vStringCat (qualifiedName, name);
					e.name = vStringValue(qualifiedName);
					makeTagEntry(&e);
					vStringDelete (qualifiedName);
				}
			} else if (vStringLength (name) > 0)
			{
				makeSimpleTag (name, PerlKinds, kind);
				if (Option.include.qualifiedTags && qualified &&
					K_PACKAGE != kind &&
					package != NULL  && vStringLength (package) > 0)
				{
					vString *const qualifiedName = vStringNew ();
					vStringCopy (qualifiedName, package);
					vStringCat (qualifiedName, name);
					makeSimpleTag (qualifiedName, PerlKinds, kind);
					vStringDelete (qualifiedName);
				}
			}
			vStringClear (name);
		}
	}

END_MAIN_WHILE:
	vStringDelete (name);
	if (package != NULL)
		vStringDelete (package);
}

extern parserDefinition* PerlParser (void)
{
	static const char *const extensions [] = { "pl", "pm", "plx", "perl", NULL };
	parserDefinition* def = parserNew ("Perl");
	def->kinds      = PerlKinds;
	def->kindCount  = KIND_COUNT (PerlKinds);
	def->extensions = extensions;
	def->parser     = findPerlTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
