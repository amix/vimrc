/*
*   $Id: asp.c 711 2009-07-04 16:52:11Z dhiebert $
*
*   Copyright (c) 2000, Patrick Dehne <patrick@steidle.net>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for the ASP (Active
*   Server Pages) web page scripting language.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_CONST, K_CLASS, K_FUNCTION, K_SUB, K_DIM
} aspKind;

static kindOption AspKinds [] = {
	{ TRUE, 'd', "constant",   "constants"},
	{ TRUE, 'c', "class",      "classes"}, 
	{ TRUE, 'f', "function",   "functions"},
	{ TRUE, 's', "subroutine", "subroutines"},
	{ TRUE, 'v', "variable",   "variables"}
};

/*
*   FUNCTION DEFINITIONS
*/

static void findAspTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char *cp = line;

		while (*cp != '\0')
		{
			/* jump over whitespace */
			while (isspace ((int)*cp))
				cp++;

			/* jump over strings */
			if (*cp == '"')
			{
				cp++;
				while (*cp!='"' && *cp!='\0')
					cp++;
			}

			/* jump over comments */ 
			else if (*cp == '\'')
				break;
			
			/* jump over end function/sub lines */
			else if (strncasecmp ((const char*) cp, "end", (size_t) 3)== 0)
			{
				cp += 3;
				if (isspace ((int)*cp))
				{
					while (isspace ((int)*cp))
						++cp;

					if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0)
					{
						cp+=8;
						break;
					}

					else if (strncasecmp ((const char*) cp, "sub", (size_t) 3) == 0)
					{
						cp+=3;
						break;
					}
				}
			}

			/* jump over exit function/sub lines */
			else if (strncasecmp ((const char*) cp, "exit", (size_t) 4)==0)
			{
				cp += 4;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;

					if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0)
					{
						cp+=8;
						break;
					}

					else if (strncasecmp ((const char*) cp, "sub", (size_t) 3) == 0)
					{
						cp+=3;
						break;
					}
				}
			}

			/* class member? */
			else if (strncasecmp ((const char*) cp, "public", (size_t) 6) == 0)
			{
				cp += 6;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0)
					{
						cp+=8;
					    while (isspace ((int) *cp))
						    ++cp;
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_FUNCTION);
					    vStringClear (name);
					}
					else if (strncasecmp ((const char*) cp, "sub", (size_t) 3) == 0)
					{
						cp+=3;
					    while (isspace ((int) *cp))
						    ++cp;
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_SUB);
					    vStringClear (name);
					}
					else {
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_DIM);
					    vStringClear (name);
					}
				}
			}
			else if (strncasecmp ((const char*) cp, "private", (size_t) 7) == 0)
			{
				cp += 7;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0)
					{
						cp+=8;
					    while (isspace ((int) *cp))
						    ++cp;
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_FUNCTION);
					    vStringClear (name);
					}
					else if (strncasecmp ((const char*) cp, "sub", (size_t) 3) == 0)
					{
						cp+=3;
					    while (isspace ((int) *cp))
						    ++cp;
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_SUB);
					    vStringClear (name);
					}
					else {
					    while (isalnum ((int) *cp)  ||  *cp == '_')
					    {
						    vStringPut (name, (int) *cp);
						    ++cp;
					    }
					    vStringTerminate (name);
					    makeSimpleTag (name, AspKinds, K_DIM);
					    vStringClear (name);
					}
				}
			}

			/* function? */
			else if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0)
			{
				cp += 8;

				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					while (isalnum ((int) *cp)  ||  *cp == '_')
					{
						vStringPut (name, (int) *cp);
						++cp;
					}
					vStringTerminate (name);
					makeSimpleTag (name, AspKinds, K_FUNCTION);
					vStringClear (name);
				}
			}

			/* sub? */
			else if (strncasecmp ((const char*) cp, "sub", (size_t) 3) == 0)
			{
				cp += 3;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					while (isalnum ((int) *cp)  ||  *cp == '_')
					{
						vStringPut (name, (int) *cp);
						++cp;
					}
					vStringTerminate (name);
					makeSimpleTag (name, AspKinds, K_SUB);
					vStringClear (name);
				}
			}

			/* dim variable? */
			else if (strncasecmp ((const char*) cp, "dim", (size_t) 3) == 0)
			{
				cp += 3;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					while (isalnum ((int) *cp)  ||  *cp == '_')
					{
						vStringPut (name, (int) *cp);
						++cp;
					}
					vStringTerminate (name);
					makeSimpleTag (name, AspKinds, K_DIM);
					vStringClear (name);
				}
			}

			/* class declaration? */
			else if (strncasecmp ((const char*) cp, "class", (size_t) 5) == 0)
			{
				cp += 5;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					while (isalnum ((int) *cp)  ||  *cp == '_')
					{
						vStringPut (name, (int) *cp);
						++cp;
					}
					vStringTerminate (name);
					makeSimpleTag (name, AspKinds, K_CLASS);
					vStringClear (name);
				}
			}

			/* const declaration? */
			else if (strncasecmp ((const char*) cp, "const", (size_t) 5) == 0)
			{
				cp += 5;
				if (isspace ((int) *cp))
				{
					while (isspace ((int) *cp))
						++cp;
					while (isalnum ((int) *cp)  ||  *cp == '_')
					{
						vStringPut (name, (int) *cp);
						++cp;
					}
					vStringTerminate (name);
					makeSimpleTag (name, AspKinds, K_CONST);
					vStringClear (name);
				}
			}

			/* nothing relevant */
			else if (*cp != '\0')
				cp++;
		}
	}
	vStringDelete (name);
}

extern parserDefinition* AspParser (void)
{
	static const char *const extensions [] = { "asp", "asa", NULL };
	parserDefinition* def = parserNew ("Asp");
	def->kinds      = AspKinds;
	def->kindCount  = KIND_COUNT (AspKinds);
	def->extensions = extensions;
	def->parser     = findAspTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */

