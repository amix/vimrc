/*
*   $Id: options.h 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to option processing.
*/
#ifndef _OPTIONS_H
#define _OPTIONS_H

#if defined(OPTION_WRITE) || defined(VAXC)
# define CONST_OPTION
#else
# define CONST_OPTION const
#endif

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdarg.h>

#include "args.h"
#include "parse.h"
#include "strlist.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

typedef enum { OPTION_NONE, OPTION_SHORT, OPTION_LONG } optionType;

typedef struct sCookedArgs {
	/* private */
	Arguments* args;
	char *shortOptions;
	char simple[2];
	boolean isOption;
	boolean longOption;
	const char* parameter;
	/* public */
	char* item;
} cookedArgs;

typedef enum eLocate {
	EX_MIX,      /* line numbers for defines, patterns otherwise */
	EX_LINENUM,  /* -n  only line numbers in tag file */
	EX_PATTERN   /* -N  only patterns in tag file */
} exCmd;

typedef enum sortType {
	SO_UNSORTED,
	SO_SORTED,
	SO_FOLDSORTED
} sortType;

struct sInclude {
	boolean fileNames;      /* include tags for source file names */
	boolean qualifiedTags;  /* include tags for qualified class members */
	boolean fileScope;      /* include tags of file scope only */
};

struct sExtFields {  /* extension field content control */
	boolean access;
	boolean fileScope;
	boolean implementation;
	boolean inheritance;
	boolean kind;
	boolean kindKey;
	boolean kindLong;
	boolean language;
	boolean lineNumber;
	boolean scope;
	boolean signature;
	boolean typeRef;
};

/*  This stores the command line options.
 */
typedef struct sOptionValues {
	struct sInclude include;/* --extra  extra tag inclusion */
	struct sExtFields extensionFields;/* --fields  extension field control */
	stringList* ignore;     /* -I  name of file containing tokens to ignore */
	boolean append;         /* -a  append to "tags" file */
	boolean backward;       /* -B  regexp patterns search backwards */
	boolean etags;          /* -e  output Emacs style tags file */
	exCmd locate;           /* --excmd  EX command used to locate tag */
	boolean recurse;        /* -R  recurse into directories */
	sortType sorted;        /* -u,--sort  sort tags */
	boolean verbose;        /* -V  verbose */
	boolean xref;           /* -x  generate xref output instead */
	char *fileList;         /* -L  name of file containing names of files */
	char *tagFileName;      /* -o  name of tags file */
	stringList* headerExt;  /* -h  header extensions */
	stringList* etagsInclude;/* --etags-include  list of TAGS files to include*/
	unsigned int tagFileFormat;/* --format  tag file format (level) */
	boolean if0;            /* --if0  examine code within "#if 0" branch */
	boolean kindLong;       /* --kind-long */
	langType language;      /* --lang specified language override */
	boolean followLinks;    /* --link  follow symbolic links? */
	boolean filter;         /* --filter  behave as filter: files in, tags out */
	char* filterTerminator; /* --filter-terminator  string to output */
	boolean tagRelative;    /* --tag-relative file paths relative to tag file */
	boolean printTotals;    /* --totals  print cumulative statistics */
	boolean lineDirectives; /* --linedirectives  process #line directives */
#ifdef DEBUG
	long debugLevel;        /* -D  debugging output */
	unsigned long breakLine;/* -b  source line at which to call lineBreak() */
#endif
} optionValues;

/*
*   GLOBAL VARIABLES
*/
extern CONST_OPTION optionValues		Option;

/*
*   FUNCTION PROTOTYPES
*/
extern void verbose (const char *const format, ...) __printf__ (1, 2);
extern void freeList (stringList** const pString);
extern void setDefaultTagFileName (void);
extern void checkOptions (void);
extern boolean filesRequired (void);
extern void testEtagsInvocation (void);

extern cookedArgs* cArgNewFromString (const char* string);
extern cookedArgs* cArgNewFromArgv (char* const* const argv);
extern cookedArgs* cArgNewFromFile (FILE* const fp);
extern cookedArgs* cArgNewFromLineFile (FILE* const fp);
extern void cArgDelete (cookedArgs* const current);
extern boolean cArgOff (cookedArgs* const current);
extern boolean cArgIsOption (cookedArgs* const current);
extern const char* cArgItem (cookedArgs* const current);
extern void cArgForth (cookedArgs* const current);

extern boolean isExcludedFile (const char* const name);
extern boolean isIncludeFile (const char *const fileName);
extern boolean isIgnoreToken (const char *const name, boolean *const pIgnoreParens, const char **const replacement);
extern void parseOption (cookedArgs* const cargs);
extern void parseOptions (cookedArgs* const cargs);
extern void previewFirstOption (cookedArgs* const cargs);
extern void readOptionConfiguration (void);
extern void initOptions (void);
extern void freeOptionResources (void);

#endif  /* _OPTIONS_H */

/* vi:set tabstop=4 shiftwidth=4: */
