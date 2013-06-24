/*
*   $Id: parse.c 597 2007-07-31 05:35:30Z dhiebert $
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for managing source languages and
*   dispatching files to the appropriate language parser.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parsers.h" 
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
static parserDefinitionFunc* BuiltInParsers[] = { PARSER_LIST };
static parserDefinition** LanguageTable = NULL;
static unsigned int LanguageCount = 0;

/*
*   FUNCTION DEFINITIONS
*/

extern void makeSimpleTag (
		const vString* const name, kindOption* const kinds, const int kind)
{
	if (kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name));

	    e.kindName = kinds [kind].name;
	    e.kind     = kinds [kind].letter;

	    makeTagEntry (&e);
	}
}

/*
*   parserDescription mapping management
*/

extern parserDefinition* parserNew (const char* name)
{
	parserDefinition* result = xCalloc (1, parserDefinition);
	result->name = eStrdup (name);
	return result;
}

extern const char *getLanguageName (const langType language)
{
	const char* result;
	if (language == LANG_IGNORE)
		result = "unknown";
	else
	{
		Assert (0 <= language  &&  language < (int) LanguageCount);
		result = LanguageTable [language]->name;
	}
	return result;
}

extern langType getNamedLanguage (const char *const name)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	Assert (name != NULL);
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		const parserDefinition* const lang = LanguageTable [i];
		if (lang->name != NULL)
			if (strcasecmp (name, lang->name) == 0)
				result = i;
	}
	return result;
}

static langType getExtensionLanguage (const char *const extension)
{
	langType result = LANG_IGNORE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL  &&  stringListExtensionMatched (exts, extension))
			result = i;
	}
	return result;
}

static langType getPatternLanguage (const char *const fileName)
{
	langType result = LANG_IGNORE;
	const char* base = baseFilename (fileName);
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  result == LANG_IGNORE  ;  ++i)
	{
		stringList* const ptrns = LanguageTable [i]->currentPatterns;
		if (ptrns != NULL  &&  stringListFileMatched (ptrns, base))
			result = i;
	}
	return result;
}

#ifdef SYS_INTERPRETER

/*  The name of the language interpreter, either directly or as the argument
 *  to "env".
 */
static vString* determineInterpreter (const char* const cmd)
{
	vString* const interpreter = vStringNew ();
	const char* p = cmd;
	do
	{
		vStringClear (interpreter);
		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		for ( ;  *p != '\0'  &&  ! isspace ((int) *p)  ;  ++p)
			vStringPut (interpreter, (int) *p);
		vStringTerminate (interpreter);
	} while (strcmp (vStringValue (interpreter), "env") == 0);
	return interpreter;
}

static langType getInterpreterLanguage (const char *const fileName)
{
	langType result = LANG_IGNORE;
	FILE* const fp = fopen (fileName, "r");
	if (fp != NULL)
	{
		vString* const vLine = vStringNew ();
		const char* const line = readLine (vLine, fp);
		if (line != NULL  &&  line [0] == '#'  &&  line [1] == '!')
		{
			const char* const lastSlash = strrchr (line, '/');
			const char *const cmd = lastSlash != NULL ? lastSlash+1 : line+2;
			vString* const interpreter = determineInterpreter (cmd);
			result = getExtensionLanguage (vStringValue (interpreter));
			if (result == LANG_IGNORE)
				result = getNamedLanguage (vStringValue (interpreter));
			vStringDelete (interpreter);
		}
		vStringDelete (vLine);
		fclose (fp);
	}
	return result;
}

#endif

extern langType getFileLanguage (const char *const fileName)
{
	langType language = Option.language;
	if (language == LANG_AUTO)
	{
		language = getExtensionLanguage (fileExtension (fileName));
		if (language == LANG_IGNORE)
			language = getPatternLanguage (fileName);
#ifdef SYS_INTERPRETER
		if (language == LANG_IGNORE)
		{
			fileStatus *status = eStat (fileName);
			if (status->isExecutable)
				language = getInterpreterLanguage (fileName);
		}
#endif
	}
	return language;
}

extern void printLanguageMap (const langType language)
{
	boolean first = TRUE;
	unsigned int i;
	stringList* map = LanguageTable [language]->currentPatterns;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		printf ("%s(%s)", (first ? "" : " "),
				vStringValue (stringListItem (map, i)));
		first = FALSE;
	}
	map = LanguageTable [language]->currentExtensions;
	for (i = 0  ;  map != NULL  &&  i < stringListCount (map)  ;  ++i)
	{
		printf ("%s.%s", (first ? "" : " "),
				vStringValue (stringListItem (map, i)));
		first = FALSE;
	}
}

extern void installLanguageMapDefault (const langType language)
{
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentPatterns != NULL)
		stringListDelete (lang->currentPatterns);
	if (lang->currentExtensions != NULL)
		stringListDelete (lang->currentExtensions);

	if (lang->patterns == NULL)
		lang->currentPatterns = stringListNew ();
	else
	{
		lang->currentPatterns =
			stringListNewFromArgv (lang->patterns);
	}
	if (lang->extensions == NULL)
		lang->currentExtensions = stringListNew ();
	else
	{
		lang->currentExtensions =
			stringListNewFromArgv (lang->extensions);
	}
	if (Option.verbose)
		printLanguageMap (language);
	verbose ("\n");
}

extern void installLanguageMapDefaults (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		verbose ("    %s: ", getLanguageName (i));
		installLanguageMapDefault (i);
	}
}

extern void clearLanguageMap (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	stringListClear (LanguageTable [language]->currentPatterns);
	stringListClear (LanguageTable [language]->currentExtensions);
}

extern void addLanguagePatternMap (const langType language, const char* ptrn)
{
	vString* const str = vStringNewInit (ptrn);
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentPatterns == NULL)
		lang->currentPatterns = stringListNew ();
	stringListAdd (lang->currentPatterns, str);
}

extern boolean removeLanguageExtensionMap (const char *const extension)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL  &&  stringListRemoveExtension (exts, extension))
		{
			verbose (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguageExtensionMap (
		const langType language, const char* extension)
{
	vString* const str = vStringNewInit (extension);
	Assert (0 <= language  &&  language < (int) LanguageCount);
	removeLanguageExtensionMap (extension);
	stringListAdd (LanguageTable [language]->currentExtensions, str);
}

extern void enableLanguage (const langType language, const boolean state)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	LanguageTable [language]->enabled = state;
}

extern void enableLanguages (const boolean state)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		enableLanguage (i, state);
}

static void initializeParsers (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		if (LanguageTable [i]->initialize != NULL)
			(LanguageTable [i]->initialize) ((langType) i);
}

extern void initializeParsing (void)
{
	unsigned int builtInCount;
	unsigned int i;

	builtInCount = sizeof (BuiltInParsers) / sizeof (BuiltInParsers [0]);
	LanguageTable = xMalloc (builtInCount, parserDefinition*);

	verbose ("Installing parsers: ");
	for (i = 0  ;  i < builtInCount  ;  ++i)
	{
		parserDefinition* const def = (*BuiltInParsers [i]) ();
		if (def != NULL)
		{
			boolean accepted = FALSE;
			if (def->name == NULL  ||  def->name[0] == '\0')
				error (FATAL, "parser definition must contain name\n");
			else if (def->regex)
			{
#ifdef HAVE_REGEX
				def->parser = findRegexTags;
				accepted = TRUE;
#endif
			}
			else if ((def->parser == NULL)  ==  (def->parser2 == NULL))
				error (FATAL,
		"%s parser definition must define one and only one parsing routine\n",
					   def->name);
			else
				accepted = TRUE;
			if (accepted)
			{
				verbose ("%s%s", i > 0 ? ", " : "", def->name);
				def->id = LanguageCount++;
				LanguageTable [def->id] = def;
			}
		}
	}
	verbose ("\n");
	enableLanguages (TRUE);
	initializeParsers ();
}

extern void freeParserResources (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		parserDefinition* const lang = LanguageTable [i];
		freeList (&lang->currentPatterns);
		freeList (&lang->currentExtensions);
		eFree (lang->name);
		lang->name = NULL;
		eFree (lang);
	}
	if (LanguageTable != NULL)
		eFree (LanguageTable);
	LanguageTable = NULL;
	LanguageCount = 0;
}

/*
*   Option parsing
*/

extern void processLanguageDefineOption (
		const char *const option, const char *const parameter __unused__)
{
#ifdef HAVE_REGEX
	if (parameter [0] == '\0')
		error (WARNING, "No language specified for \"%s\" option", option);
	else if (getNamedLanguage (parameter) != LANG_IGNORE)
		error (WARNING, "Language \"%s\" already defined", parameter);
	else
	{
		unsigned int i = LanguageCount++;
		parserDefinition* const def = parserNew (parameter);
		def->parser            = findRegexTags;
		def->currentPatterns   = stringListNew ();
		def->currentExtensions = stringListNew ();
		def->regex             = TRUE;
		def->enabled           = TRUE;
		def->id                = i;
		LanguageTable = xRealloc (LanguageTable, i + 1, parserDefinition*);
		LanguageTable [i] = def;
	}
#else
	error (WARNING, "regex support not available; required for --%s option",
		   option);
#endif
}

static kindOption *langKindOption (const langType language, const int flag)
{
	unsigned int i;
	kindOption* result = NULL;
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	for (i=0  ;  i < lang->kindCount  &&  result == NULL  ;  ++i)
		if (lang->kinds [i].letter == flag)
			result = &lang->kinds [i];
	return result;
}

static void disableLanguageKinds (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->regex)
		disableRegexKinds (language);
	else
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			lang->kinds [i].enabled = FALSE;
	}
}

static boolean enableLanguageKind (
		const langType language, const int kind, const boolean mode)
{
	boolean result = FALSE;
	if (LanguageTable [language]->regex)
		result = enableRegexKind (language, kind, mode);
	else
	{
		kindOption* const opt = langKindOption (language, kind);
		if (opt != NULL)
		{
			opt->enabled = mode;
			result = TRUE;
		}
	}
	return result;
}

static void processLangKindOption (
		const langType language, const char *const option,
		const char *const parameter)
{
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (*p != '+'  &&  *p != '-')
		disableLanguageKinds (language);
	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;  break;
		case '-': mode = FALSE; break;
		default:
			if (! enableLanguageKind (language, c, mode))
				error (WARNING, "Unsupported parameter '%c' for --%s option",
					c, option);
			break;
	}
}

extern boolean processKindOption (
		const char *const option, const char *const parameter)
{
	boolean handled = FALSE;
	const char* const dash = strchr (option, '-');
	if (dash != NULL  &&
		(strcmp (dash + 1, "kinds") == 0  ||  strcmp (dash + 1, "types") == 0))
	{
		langType language;
		vString* langName = vStringNew ();
		vStringNCopyS (langName, option, dash - option);
		language = getNamedLanguage (vStringValue (langName));
		if (language == LANG_IGNORE)
			error (WARNING, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
		else
			processLangKindOption (language, option, parameter);
		vStringDelete (langName);
		handled = TRUE;
	}
	return handled;
}

static void printLanguageKind (const kindOption* const kind, boolean indent)
{
	const char *const indentation = indent ? "    " : "";
	printf ("%s%c  %s%s\n", indentation, kind->letter,
		kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
		kind->enabled ? "" : " [off]");
}

static void printKinds (langType language, boolean indent)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  lang->regex)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			printLanguageKind (lang->kinds + i, indent);
		printRegexKinds (language, indent);
	}
}

extern void printLanguageKinds (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
			printKinds (i, TRUE);
		}
	}
	else
		printKinds (language, FALSE);
}

static void printMaps (const langType language)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	printf ("%-8s", lang->name);
	if (lang->currentExtensions != NULL)
		for (i = 0  ;  i < stringListCount (lang->currentExtensions)  ;  ++i)
			printf (" *.%s", vStringValue (
						stringListItem (lang->currentExtensions, i)));
	if (lang->currentPatterns != NULL)
		for (i = 0  ;  i < stringListCount (lang->currentPatterns)  ;  ++i)
			printf (" %s", vStringValue (
						stringListItem (lang->currentPatterns, i)));
	putchar ('\n');
}

extern void printLanguageMaps (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printMaps (i);
	}
	else
		printMaps (language);
}

static void printLanguage (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  lang->regex)
		printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
}
	    
extern void printLanguageList (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		printLanguage (i);
}

/*
*   File parsing
*/

static void makeFileTag (const char *const fileName)
{
	if (Option.include.fileNames)
	{
		tagEntryInfo tag;
		initTagEntry (&tag, baseFilename (fileName));

		tag.isFileEntry     = TRUE;
		tag.lineNumberEntry = TRUE;
		tag.lineNumber      = 1;
		tag.kindName        = "file";
		tag.kind            = 'F';

		makeTagEntry (&tag);
	}
}

static boolean createTagsForFile (
		const char *const fileName, const langType language,
		const unsigned int passCount)
{
	boolean retried = FALSE;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (fileOpen (fileName, language))
	{
		const parserDefinition* const lang = LanguageTable [language];
		if (Option.etags)
			beginEtagsFile ();

		makeFileTag (fileName);

		if (lang->parser != NULL)
			lang->parser ();
		else if (lang->parser2 != NULL)
			retried = lang->parser2 (passCount);

		if (Option.etags)
			endEtagsFile (getSourceFileTagPath ());

		fileClose ();
	}

	return retried;
}

static boolean createTagsWithFallback (
		const char *const fileName, const langType language)
{
	const unsigned long numTags	= TagFile.numTags.added;
	fpos_t tagFilePosition;
	unsigned int passCount = 0;
	boolean tagFileResized = FALSE;

	fgetpos (TagFile.fp, &tagFilePosition);
	while (createTagsForFile (fileName, language, ++passCount))
	{
		/*  Restore prior state of tag file.
		 */
		fsetpos (TagFile.fp, &tagFilePosition);
		TagFile.numTags.added = numTags;
		tagFileResized = TRUE;
	}
	return tagFileResized;
}

extern boolean parseFile (const char *const fileName)
{
	boolean tagFileResized = FALSE;
	langType language = Option.language;
	if (Option.language == LANG_AUTO)
		language = getFileLanguage (fileName);
	Assert (language != LANG_AUTO);
	if (language == LANG_IGNORE)
		verbose ("ignoring %s (unknown language)\n", fileName);
	else if (! LanguageTable [language]->enabled)
		verbose ("ignoring %s (language disabled)\n", fileName);
	else
	{
		if (Option.filter)
			openTagFile ();

		tagFileResized = createTagsWithFallback (fileName, language);

		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

		return tagFileResized;
	}
	return tagFileResized;
}

/* vi:set tabstop=4 shiftwidth=4 nowrap: */
