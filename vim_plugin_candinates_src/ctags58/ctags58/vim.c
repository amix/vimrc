/*
*	$Id: vim.c 485 2006-10-24 12:06:19Z dfishburn $
*
*	Copyright (c) 2000-2003, Darren Hiebert
*
*	This source code is released for free distribution under the terms of the
*	GNU General Public License.
*
*	Thanks are due to Jay Glanville for significant improvements.
*
*	This module contains functions for generating tags for user-defined
*	functions for the Vim editor.
*/

/*
*	INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <setjmp.h>
#ifdef DEBUG
#include <stdio.h>
#endif


#include "parse.h"
#include "read.h"
#include "vstring.h"

#if 0
typedef struct sLineInfo {
	tokenType	type;
	keywordId	keyword;
	vString *	string;
	vString *	scope;
	unsigned long lineNumber;
	fpos_t filePosition;
} lineInfo;
#endif

/*
*	DATA DEFINITIONS
*/
typedef enum {
	K_AUGROUP,
	K_COMMAND,
	K_FUNCTION,
	K_MAP,
	K_VARIABLE
} vimKind;

static kindOption VimKinds [] = {
	{ TRUE,  'a', "augroup",  "autocommand groups" },
	{ TRUE,  'c', "command",  "user-defined commands" },
	{ TRUE,  'f', "function", "function definitions" },
	{ TRUE,  'm', "map",      "maps" },
	{ TRUE,  'v', "variable", "variable definitions" },
};

/*
 *	 DATA DECLARATIONS
 */

#if 0
typedef enum eException {
	ExceptionNone, ExceptionEOF 
} exception_t;
#endif

/*
 *	DATA DEFINITIONS
 */

#if 0
static jmp_buf Exception;
#endif

/*
 *	FUNCTION DEFINITIONS
 */

/* This function takes a char pointer, tries to find a scope separator in the
 * string, and if it does, returns a pointer to the character after the colon,
 * and the character defining the scope.
 * If a colon is not found, it returns the original pointer.
 */
static const unsigned char* skipPrefix (const unsigned char* name, int *scope)
{
	const unsigned char* result = name;
	int counter;
	size_t length;
	length = strlen((const char*)name);
	if (scope != NULL)
		*scope = '\0';
	if (length > 3 && name[1] == ':')
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 2;
	}
	else if (length > 5 && strncasecmp ((const char*) name, "<SID>", (size_t) 5) == 0)
	{
		if (scope != NULL)
			*scope = *name;
		result = name + 5;
	}
	else
	{
		/*
		 * Vim7 check for dictionaries or autoload function names
		 */
		counter = 0;
		do
		{
			switch ( name[counter] )
			{
				case '.':
					/* Set the scope to d - Dictionary */
					*scope = 'd';
					break;
				case '#':
					/* Set the scope to a - autoload */
					*scope = 'a';
					break;
			}
			++counter;
		} while (isalnum ((int) name[counter]) ||  
				name[counter] == '_'		   ||  
				name[counter] == '.'		   ||  
				name[counter] == '#'
				);
	}
	return result;
}

static boolean isMap (const unsigned char* line)
{
	/*
	 * There are many different short cuts for specifying a map.
	 * This routine should capture all the permutations.
	 */
	if (
			strncmp ((const char*) line, "map",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "nm",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "nma",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "nmap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "vm",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "vma",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "vmap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "om",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "oma",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "omap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "im",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "ima",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "imap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "lm",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "lma",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "lmap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "cm",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "cma",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "cmap",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "no",       (size_t) 2) == 0 ||
			strncmp ((const char*) line, "nor",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "nore",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "norem",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "norema",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "noremap",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "nno",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "nnor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "nnore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "nnorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "nnorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "nnoremap", (size_t) 8) == 0 ||
			strncmp ((const char*) line, "vno",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "vnor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "vnore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "vnorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "vnorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "vnoremap", (size_t) 8) == 0 ||
			strncmp ((const char*) line, "ono",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "onor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "onore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "onorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "onorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "onoremap", (size_t) 8) == 0 ||
			strncmp ((const char*) line, "ino",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "inor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "inore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "inorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "inorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "inoremap", (size_t) 8) == 0 ||
			strncmp ((const char*) line, "lno",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "lnor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "lnore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "lnorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "lnorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "lnoremap", (size_t) 8) == 0 ||
			strncmp ((const char*) line, "cno",      (size_t) 3) == 0 ||
			strncmp ((const char*) line, "cnor",     (size_t) 4) == 0 ||
			strncmp ((const char*) line, "cnore",    (size_t) 5) == 0 ||
			strncmp ((const char*) line, "cnorem",   (size_t) 6) == 0 ||
			strncmp ((const char*) line, "cnorema",  (size_t) 7) == 0 ||
			strncmp ((const char*) line, "cnoremap", (size_t) 8) == 0 
			)
			return TRUE;

	return FALSE;
}

static const unsigned char * readVimLine (void)
{
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		while (isspace ((int) *line))
			++line;

		if ((int) *line == '"')
			continue;  /* skip comment */

		break;
	}

	return line;
}

static void parseFunction (const unsigned char *line)
{
	vString *name = vStringNew ();
	/* boolean inFunction = FALSE; */
	int scope;

	const unsigned char *cp = line + 1;

	if ((int) *++cp == 'n'	&&	(int) *++cp == 'c'	&&
		(int) *++cp == 't'	&&	(int) *++cp == 'i'	&&
		(int) *++cp == 'o'	&&	(int) *++cp == 'n')
			++cp;
	if ((int) *cp == '!')
		++cp;
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp;

		if (*cp)
		{
			cp = skipPrefix (cp, &scope);
			if (isupper ((int) *cp)  ||  
					scope == 's'  ||  /* script scope */
					scope == '<'  ||  /* script scope */
					scope == 'd'  ||  /* dictionary */
					scope == 'a')	  /* autoload */
			{
				do
				{
					vStringPut (name, (int) *cp);
					++cp;
				} while (isalnum ((int) *cp) ||  *cp == '_' ||	*cp == '.' ||  *cp == '#');
				vStringTerminate (name);
				makeSimpleTag (name, VimKinds, K_FUNCTION);
				vStringClear (name);
			}
		}
	}

	/* TODO - update struct to indicate inside function */
	while ((line = readVimLine ()) != NULL)
	{
		/* 
		 * Vim7 added the for/endfo[r] construct, so we must first
		 * check for an "endfo", before a "endf"
		 */
		if ( (!strncmp ((const char*) line, "endfo", (size_t) 5) == 0) && 
				(strncmp ((const char*) line, "endf", (size_t) 4) == 0)   )
			break;
		/* TODO - call parseVimLine */
	}
	vStringDelete (name);
}

static void parseAutogroup (const unsigned char *line)
{
	vString *name = vStringNew ();

	/* Found Autocommand Group (augroup) */
	const unsigned char *cp = line + 2;
	if ((int) *++cp == 'r' && (int) *++cp == 'o' &&
			(int) *++cp == 'u' && (int) *++cp == 'p')
		++cp;
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp; 

		if (*cp)
		{
			if (strncasecmp ((const char*) cp, "end", (size_t) 3) != 0)
			{	 
				do
				{
					vStringPut (name, (int) *cp);
					++cp;
				} while (isalnum ((int) *cp)  ||  *cp == '_');
				vStringTerminate (name);
				makeSimpleTag (name, VimKinds, K_AUGROUP);
				vStringClear (name);
			}
		}
	}
	vStringDelete (name);
}

static boolean parseCommand (const unsigned char *line)
{
	vString *name = vStringNew ();
	boolean cmdProcessed = TRUE;

	/* 
	 * Found a user-defined command 
	 *
	 * They can have many options preceeded by a dash
	 * command! -nargs=+ -complete Select  :call s:DB_execSql("select " . <q-args>)
	 * The name of the command should be the first word not preceeded by a dash
	 *
	 */
	const unsigned char *cp = line;

	if ( (int) *cp == '\\' ) 
	{
		/*
		 * We are recursively calling this function is the command
		 * has been continued on to the next line
		 *
		 * Vim statements can be continued onto a newline using a \
		 * to indicate the previous line is continuing.
		 *
		 * com -nargs=1 -bang -complete=customlist,EditFileComplete
		 * 			\ EditFile edit<bang> <args>
		 *
		 * If the following lines do not have a line continuation
		 * the command must not be spanning multiple lines and should
		 * be synatically incorrect.
		 */
		if ((int) *cp == '\\')
			++cp;

		while (*cp && isspace ((int) *cp))
			++cp; 
	}
	else if ( (!strncmp ((const char*) line, "comp", (size_t) 4) == 0) && 
		     (!strncmp ((const char*) line, "comc", (size_t) 4) == 0) && 
				(strncmp ((const char*) line, "com", (size_t) 3) == 0) )
	{
		cp += 2;
		if ((int) *++cp == 'm' && (int) *++cp == 'a' &&
				(int) *++cp == 'n' && (int) *++cp == 'd')
			++cp;

		if ((int) *cp == '!')
			++cp;

		while (*cp && isspace ((int) *cp))
			++cp; 
	} 
	else 
	{
		/*
		 * We are recursively calling this function.  If it does not start
		 * with "com" or a line continuation character, we have moved off
		 * the command line and should let the other routines parse this file.
		 */
		cmdProcessed = FALSE;
		goto cleanUp;
	}

	/*
	 * Strip off any spaces and options which are part of the command.
	 * These should preceed the command name.
	 */
	do
	{
		if (isspace ((int) *cp))
		{
			++cp;
		}
		else if (*cp == '-')
		{
			/* 
			 * Read until the next space which sparates options or the name
			 */
			while (*cp && !isspace ((int) *cp))
				++cp; 
		}
	} while ( *cp &&  !isalnum ((int) *cp) );

	if ( ! *cp )
	{
		/*
		 * We have reached the end of the line without finding the command name.
		 * Read the next line and continue processing it as a command.
		 */
		line = readVimLine();
		parseCommand(line);
		goto cleanUp;
	}

	do
	{
		vStringPut (name, (int) *cp);
		++cp;
	} while (isalnum ((int) *cp)  ||  *cp == '_');

	vStringTerminate (name);
	makeSimpleTag (name, VimKinds, K_COMMAND);
	vStringClear (name);

cleanUp:
	vStringDelete (name);

	return cmdProcessed;
}

static void parseLet (const unsigned char *line)
{
	vString *name = vStringNew ();

	/* we've found a variable declared outside of a function!! */
	const unsigned char *cp = line + 3;
	const unsigned char *np = line;
	/* get the name */
	if (isspace ((int) *cp))
	{
		while (*cp && isspace ((int) *cp))
			++cp;

		/* 
		 * Ignore lets which set:
		 *    &  - local buffer vim settings
		 *    @  - registers
		 *    [  - Lists or Dictionaries
		 */
		if (!*cp || *cp == '&' || *cp == '@' || *cp == '[' )
			goto cleanUp;

		/* 
		 * Ignore vim variables which are read only
		 *    v: - Vim variables.
		 */
		np = cp;
		++np;
		if ((int) *cp == 'v' && (int) *np == ':' )
			goto cleanUp;

		/* deal with spaces, $, @ and & */
		while (*cp && *cp != '$' && !isalnum ((int) *cp))
			++cp;

		if (!*cp)
			goto cleanUp;

		/* cp = skipPrefix (cp, &scope); */
		do
		{
			if (!*cp)
				break;

			vStringPut (name, (int) *cp);
			++cp;
		} while (isalnum ((int) *cp)  ||  *cp == '_'  ||  *cp == '#'  ||  *cp == ':'  ||  *cp == '$');
		vStringTerminate (name);
		makeSimpleTag (name, VimKinds, K_VARIABLE);
		vStringClear (name);
	}

cleanUp:
	vStringDelete (name);
}

static boolean parseMap (const unsigned char *line)
{
	vString *name = vStringNew ();

	const unsigned char *cp = line;

	/* Remove map */
	while (*cp && isalnum ((int) *cp))
		++cp;

	if ((int) *cp == '!')
		++cp;

	/*
	 * Maps follow this basic format
	 *     map 
     *    nnoremap <silent> <F8> :Tlist<CR>
     *    map <unique> <Leader>scdt <Plug>GetColumnDataType
     *    inoremap ,,, <esc>diwi<<esc>pa><cr></<esc>pa><esc>kA
     *    inoremap <buffer> ( <C-R>=PreviewFunctionSignature()<LF> 
	 *
	 * The Vim help shows the various special arguments available to a map:
	 * 1.2 SPECIAL ARGUMENTS					*:map-arguments*
     *    <buffer>
	 *    <silent>
	 *    <script>
	 *    <unique>
	 *    <special>
	 *    <expr>
	 *
	 * Strip the special arguments from the map command, this should leave
	 * the map name which we will use as the "name".
	 */
	
	do
	{
		while (*cp && isspace ((int) *cp))
			++cp; 

		if (strncmp ((const char*) cp, "<Leader>", (size_t) 8) == 0)
			break;
	
		if (
				strncmp ((const char*) cp, "<buffer>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<silent>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<script>", (size_t) 8) == 0 ||
				strncmp ((const char*) cp, "<unique>", (size_t) 8) == 0
		   )
		{
			cp += 8;
			continue;
		}
	
		if (strncmp ((const char*) cp, "<expr>", (size_t) 6) == 0)
		{
			cp += 6;
			continue;
		}
	
		if (strncmp ((const char*) cp, "<special>", (size_t) 9) == 0)
		{
			cp += 9;
			continue;
		}
	
		break;
	} while (*cp);

	do
	{
		vStringPut (name, (int) *cp);
		++cp;
	} while (*cp && *cp != ' ');

	vStringTerminate (name);
	makeSimpleTag (name, VimKinds, K_MAP);
	vStringClear (name);

	vStringDelete (name);

	return TRUE;
}

static boolean parseVimLine (const unsigned char *line)
{
	boolean readNextLine = TRUE;

	if ( (!strncmp ((const char*) line, "comp", (size_t) 4) == 0) && 
			(!strncmp ((const char*) line, "comc", (size_t) 4) == 0) && 
			(strncmp ((const char*) line, "com", (size_t) 3) == 0) )
	{
		readNextLine = parseCommand(line);
		/* TODO - Handle parseCommand returning FALSE */
	}

	if (isMap(line))
	{
		parseMap(line);
	}

	if (strncmp ((const char*) line, "fu", (size_t) 2) == 0)
	{
		parseFunction(line);
	}

	if	(strncmp ((const char*) line, "aug", (size_t) 3) == 0)
	{
		parseAutogroup(line);
	}

	if ( strncmp ((const char*) line, "let", (size_t) 3) == 0 )
	{
		parseLet(line);
	}

	return readNextLine;
}

static void parseVimFile (const unsigned char *line)
{
	boolean readNextLine = TRUE;
	line = readVimLine();

	while (line != NULL)
	{
		readNextLine = parseVimLine(line);

		if ( readNextLine )
			line = readVimLine();

	}
}

static void findVimTags (void)
{
	const unsigned char *line;
		/* TODO - change this into a structure */

	line = '\0';

	parseVimFile (line);
}

extern parserDefinition* VimParser (void)
{
	static const char *const extensions [] = { "vim", NULL };
	parserDefinition* def = parserNew ("Vim");
	def->kinds		= VimKinds;
	def->kindCount	= KIND_COUNT (VimKinds);
	def->extensions = extensions;
	def->parser		= findVimTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
