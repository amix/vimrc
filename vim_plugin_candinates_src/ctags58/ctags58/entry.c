/*
*   $Id: entry.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for creating tag entries.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>        /* to define isspace () */
#include <errno.h>

#if defined (HAVE_SYS_TYPES_H)
# include <sys/types.h>	  /* to declare off_t on some hosts */
#endif
#if defined (HAVE_TYPES_H)
# include <types.h>       /* to declare off_t on some hosts */
#endif
#if defined (HAVE_UNISTD_H)
# include <unistd.h>      /* to declare close (), ftruncate (), truncate () */
#endif

/*  These header files provide for the functions necessary to do file
 *  truncation.
 */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_IO_H
# include <io.h>
#endif

#include "debug.h"
#include "ctags.h"
#include "entry.h"
#include "main.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "sort.h"
#include "strlist.h"

/*
*   MACROS
*/
#define PSEUDO_TAG_PREFIX       "!_"

#define includeExtensionFlags()         (Option.tagFileFormat > 1)

/*
 *  Portability defines
 */
#if !defined(HAVE_TRUNCATE) && !defined(HAVE_FTRUNCATE) && !defined(HAVE_CHSIZE)
# define USE_REPLACEMENT_TRUNCATE
#endif

/*  Hack for rediculous practice of Microsoft Visual C++.
 */
#if defined (WIN32) && defined (_MSC_VER)
# define chsize         _chsize
# define open           _open
# define close          _close
# define O_RDWR         _O_RDWR
#endif

/*
*   DATA DEFINITIONS
*/

tagFile TagFile = {
    NULL,               /* tag file name */
    NULL,               /* tag file directory (absolute) */
    NULL,               /* file pointer */
    { 0, 0 },           /* numTags */
    { 0, 0, 0 },        /* max */
    { NULL, NULL, 0 },  /* etags */
    NULL                /* vLine */
};

static boolean TagsToStdout = FALSE;

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_TRUNCATE
extern int truncate (const char *path, off_t length);
#endif

#ifdef NEED_PROTO_FTRUNCATE
extern int ftruncate (int fd, off_t length);
#endif

/*
*   FUNCTION DEFINITIONS
*/

extern void freeTagFileResources (void)
{
	if (TagFile.directory != NULL)
		eFree (TagFile.directory);
	vStringDelete (TagFile.vLine);
}

extern const char *tagFileName (void)
{
	return TagFile.name;
}

/*
*   Pseudo tag support
*/

static void rememberMaxLengths (const size_t nameLength, const size_t lineLength)
{
	if (nameLength > TagFile.max.tag)
		TagFile.max.tag = nameLength;

	if (lineLength > TagFile.max.line)
		TagFile.max.line = lineLength;
}

static void writePseudoTag (
		const char *const tagName,
		const char *const fileName,
		const char *const pattern)
{
	const int length = fprintf (
			TagFile.fp, "%s%s\t%s\t/%s/\n",
			PSEUDO_TAG_PREFIX, tagName, fileName, pattern);
	++TagFile.numTags.added;
	rememberMaxLengths (strlen (tagName), (size_t) length);
}

static void addPseudoTags (void)
{
	if (! Option.xref)
	{
		char format [11];
		const char *formatComment = "unknown format";

		sprintf (format, "%u", Option.tagFileFormat);

		if (Option.tagFileFormat == 1)
			formatComment = "original ctags format";
		else if (Option.tagFileFormat == 2)
			formatComment =
				"extended format; --format=1 will not append ;\" to lines";

		writePseudoTag ("TAG_FILE_FORMAT", format, formatComment);
		writePseudoTag ("TAG_FILE_SORTED",
			Option.sorted == SO_FOLDSORTED ? "2" :
			(Option.sorted == SO_SORTED ? "1" : "0"),
			"0=unsorted, 1=sorted, 2=foldcase");
		writePseudoTag ("TAG_PROGRAM_AUTHOR",  AUTHOR_NAME,  AUTHOR_EMAIL);
		writePseudoTag ("TAG_PROGRAM_NAME",    PROGRAM_NAME, "");
		writePseudoTag ("TAG_PROGRAM_URL",     PROGRAM_URL,  "official site");
		writePseudoTag ("TAG_PROGRAM_VERSION", PROGRAM_VERSION, "");
	}
}

static void updateSortedFlag (
		const char *const line, FILE *const fp, fpos_t startOfLine)
{
	const char *const tab = strchr (line, '\t');

	if (tab != NULL)
	{
		const long boolOffset = tab - line + 1;  /* where it should be */

		if (line [boolOffset] == '0'  ||  line [boolOffset] == '1')
		{
			fpos_t nextLine;

			if (fgetpos (fp, &nextLine) == -1 || fsetpos (fp, &startOfLine) == -1)
				error (WARNING, "Failed to update 'sorted' pseudo-tag");
			else
			{
				fpos_t flagLocation;
				int c, d;

				do
					c = fgetc (fp);
				while (c != '\t'  &&  c != '\n');
				fgetpos (fp, &flagLocation);
				d = fgetc (fp);
				if (c == '\t'  &&  (d == '0'  ||  d == '1')  &&
					d != (int) Option.sorted)
				{
					fsetpos (fp, &flagLocation);
					fputc (Option.sorted == SO_FOLDSORTED ? '2' :
						(Option.sorted == SO_SORTED ? '1' : '0'), fp);
				}
				fsetpos (fp, &nextLine);
			}
		}
	}
}

/*  Look through all line beginning with "!_TAG_FILE", and update those which
 *  require it.
 */
static long unsigned int updatePseudoTags (FILE *const fp)
{
	enum { maxEntryLength = 20 };
	char entry [maxEntryLength + 1];
	unsigned long linesRead = 0;
	fpos_t startOfLine;
	size_t entryLength;
	const char *line;

	sprintf (entry, "%sTAG_FILE", PSEUDO_TAG_PREFIX);
	entryLength = strlen (entry);
	Assert (entryLength < maxEntryLength);

	fgetpos (fp, &startOfLine);
	line = readLine (TagFile.vLine, fp);
	while (line != NULL  &&  line [0] == entry [0])
	{
		++linesRead;
		if (strncmp (line, entry, entryLength) == 0)
		{
			char tab, classType [16];

			if (sscanf (line + entryLength, "%15s%c", classType, &tab) == 2  &&
				tab == '\t')
			{
				if (strcmp (classType, "_SORTED") == 0)
					updateSortedFlag (line, fp, startOfLine);
			}
			fgetpos (fp, &startOfLine);
		}
		line = readLine (TagFile.vLine, fp);
	}
	while (line != NULL)  /* skip to end of file */
	{
		++linesRead;
		line = readLine (TagFile.vLine, fp);
	}
	return linesRead;
}

/*
 *  Tag file management
 */

static boolean isValidTagAddress (const char *const excmd)
{
	boolean isValid = FALSE;

	if (strchr ("/?", excmd [0]) != NULL)
		isValid = TRUE;
	else
	{
		char *address = xMalloc (strlen (excmd) + 1, char);
		if (sscanf (excmd, "%[^;\n]", address) == 1  &&
			strspn (address,"0123456789") == strlen (address))
				isValid = TRUE;
		eFree (address);
	}
	return isValid;
}

static boolean isCtagsLine (const char *const line)
{
	enum fieldList { TAG, TAB1, SRC_FILE, TAB2, EXCMD, NUM_FIELDS };
	boolean ok = FALSE;  /* we assume not unless confirmed */
	const size_t fieldLength = strlen (line) + 1;
	char *const fields = xMalloc (NUM_FIELDS * fieldLength, char);

	if (fields == NULL)
		error (FATAL, "Cannot analyze tag file");
	else
	{
#define field(x)		(fields + ((size_t) (x) * fieldLength))

		const int numFields = sscanf (
			line, "%[^\t]%[\t]%[^\t]%[\t]%[^\r\n]",
			field (TAG), field (TAB1), field (SRC_FILE),
			field (TAB2), field (EXCMD));

		/*  There must be exactly five fields: two tab fields containing
		 *  exactly one tab each, the tag must not begin with "#", and the
		 *  file name should not end with ";", and the excmd must be
		 *  accceptable.
		 *
		 *  These conditions will reject tag-looking lines like:
		 *      int a;        <C-comment>
		 *      #define LABEL <C-comment>
		 */
		if (numFields == NUM_FIELDS   &&
			strlen (field (TAB1)) == 1  &&
			strlen (field (TAB2)) == 1  &&
			field (TAG) [0] != '#'      &&
			field (SRC_FILE) [strlen (field (SRC_FILE)) - 1] != ';'  &&
			isValidTagAddress (field (EXCMD)))
				ok = TRUE;

		eFree (fields);
	}
	return ok;
}

static boolean isEtagsLine (const char *const line)
{
	boolean result = FALSE;
	if (line [0] == '\f')
		result = (boolean) (line [1] == '\n'  ||  line [1] == '\r');
	return result;
}

static boolean isTagFile (const char *const filename)
{
	boolean ok = FALSE;  /* we assume not unless confirmed */
	FILE *const fp = fopen (filename, "rb");

	if (fp == NULL  &&  errno == ENOENT)
		ok = TRUE;
	else if (fp != NULL)
	{
		const char *line = readLine (TagFile.vLine, fp);

		if (line == NULL)
			ok = TRUE;
		else
			ok = (boolean) (isCtagsLine (line) || isEtagsLine (line));
		fclose (fp);
	}
	return ok;
}

extern void copyBytes (FILE* const fromFp, FILE* const toFp, const long size)
{
	enum { BufferSize = 1000 };
	long toRead, numRead;
	char* buffer = xMalloc (BufferSize, char);
	long remaining = size;
	do
	{
		toRead = (0 < remaining && remaining < BufferSize) ?
					remaining : (long) BufferSize;
		numRead = fread (buffer, (size_t) 1, (size_t) toRead, fromFp);
		if (fwrite (buffer, (size_t)1, (size_t)numRead, toFp) < (size_t)numRead)
			error (FATAL | PERROR, "cannot complete write");
		if (remaining > 0)
			remaining -= numRead;
	} while (numRead == toRead  &&  remaining != 0);
	eFree (buffer);
}

extern void copyFile (const char *const from, const char *const to, const long size)
{
	FILE* const fromFp = fopen (from, "rb");
	if (fromFp == NULL)
		error (FATAL | PERROR, "cannot open file to copy");
	else
	{
		FILE* const toFp = fopen (to, "wb");
		if (toFp == NULL)
			error (FATAL | PERROR, "cannot open copy destination");
		else
		{
			copyBytes (fromFp, toFp, size);
			fclose (toFp);
		}
		fclose (fromFp);
	}
}

extern void openTagFile (void)
{
	setDefaultTagFileName ();
	TagsToStdout = isDestinationStdout ();

	if (TagFile.vLine == NULL)
		TagFile.vLine = vStringNew ();

	/*  Open the tags file.
	 */
	if (TagsToStdout)
		TagFile.fp = tempFile ("w", &TagFile.name);
	else
	{
		boolean fileExists;

		setDefaultTagFileName ();
		TagFile.name = eStrdup (Option.tagFileName);
		fileExists = doesFileExist (TagFile.name);
		if (fileExists  &&  ! isTagFile (TagFile.name))
			error (FATAL,
			  "\"%s\" doesn't look like a tag file; I refuse to overwrite it.",
				  TagFile.name);

		if (Option.etags)
		{
			if (Option.append  &&  fileExists)
				TagFile.fp = fopen (TagFile.name, "a+b");
			else
				TagFile.fp = fopen (TagFile.name, "w+b");
		}
		else
		{
			if (Option.append  &&  fileExists)
			{
				TagFile.fp = fopen (TagFile.name, "r+");
				if (TagFile.fp != NULL)
				{
					TagFile.numTags.prev = updatePseudoTags (TagFile.fp);
					fclose (TagFile.fp);
					TagFile.fp = fopen (TagFile.name, "a+");
				}
			}
			else
			{
				TagFile.fp = fopen (TagFile.name, "w");
				if (TagFile.fp != NULL)
					addPseudoTags ();
			}
		}
		if (TagFile.fp == NULL)
		{
			error (FATAL | PERROR, "cannot open tag file");
			exit (1);
		}
	}
	if (TagsToStdout)
		TagFile.directory = eStrdup (CurrentDirectory);
	else
		TagFile.directory = absoluteDirname (TagFile.name);
}

#ifdef USE_REPLACEMENT_TRUNCATE

/*  Replacement for missing library function.
 */
static int replacementTruncate (const char *const name, const long size)
{
	char *tempName = NULL;
	FILE *fp = tempFile ("w", &tempName);
	fclose (fp);
	copyFile (name, tempName, size);
	copyFile (tempName, name, WHOLE_FILE);
	remove (tempName);
	eFree (tempName);

	return 0;
}

#endif

static void sortTagFile (void)
{
	if (TagFile.numTags.added > 0L)
	{
		if (Option.sorted != SO_UNSORTED)
		{
			verbose ("sorting tag file\n");
#ifdef EXTERNAL_SORT
			externalSortTags (TagsToStdout);
#else
			internalSortTags (TagsToStdout);
#endif
		}
		else if (TagsToStdout)
			catFile (tagFileName ());
	}
	if (TagsToStdout)
		remove (tagFileName ());  /* remove temporary file */
}

static void resizeTagFile (const long newSize)
{
	int result;

#ifdef USE_REPLACEMENT_TRUNCATE
	result = replacementTruncate (TagFile.name, newSize);
#else
# ifdef HAVE_TRUNCATE
	result = truncate (TagFile.name, (off_t) newSize);
# else
	const int fd = open (TagFile.name, O_RDWR);

	if (fd == -1)
		result = -1;
	else
	{
#  ifdef HAVE_FTRUNCATE
		result = ftruncate (fd, (off_t) newSize);
#  else
#   ifdef HAVE_CHSIZE
		result = chsize (fd, newSize);
#   endif
#  endif
		close (fd);
	}
# endif
#endif
	if (result == -1)
		fprintf (errout, "Cannot shorten tag file: errno = %d\n", errno);
}

static void writeEtagsIncludes (FILE *const fp)
{
	if (Option.etagsInclude)
	{
		unsigned int i;
		for (i = 0  ;  i < stringListCount (Option.etagsInclude)  ;  ++i)
		{
			vString *item = stringListItem (Option.etagsInclude, i);
			fprintf (fp, "\f\n%s,include\n", vStringValue (item));
		}
	}
}

extern void closeTagFile (const boolean resize)
{
	long desiredSize, size;

	if (Option.etags)
		writeEtagsIncludes (TagFile.fp);
	desiredSize = ftell (TagFile.fp);
	fseek (TagFile.fp, 0L, SEEK_END);
	size = ftell (TagFile.fp);
	fclose (TagFile.fp);
	if (resize  &&  desiredSize < size)
	{
		DebugStatement (
			debugPrintf (DEBUG_STATUS, "shrinking %s from %ld to %ld bytes\n",
				TagFile.name, size, desiredSize); )
		resizeTagFile (desiredSize);
	}
	sortTagFile ();
	eFree (TagFile.name);
	TagFile.name = NULL;
}

extern void beginEtagsFile (void)
{
	TagFile.etags.fp = tempFile ("w+b", &TagFile.etags.name);
	TagFile.etags.byteCount = 0;
}

extern void endEtagsFile (const char *const name)
{
	const char *line;

	fprintf (TagFile.fp, "\f\n%s,%ld\n", name, (long) TagFile.etags.byteCount);
	if (TagFile.etags.fp != NULL)
	{
		rewind (TagFile.etags.fp);
		while ((line = readLine (TagFile.vLine, TagFile.etags.fp)) != NULL)
			fputs (line, TagFile.fp);
		fclose (TagFile.etags.fp);
		remove (TagFile.etags.name);
		eFree (TagFile.etags.name);
		TagFile.etags.fp = NULL;
		TagFile.etags.name = NULL;
	}
}

/*
 *  Tag entry management
 */

/*  This function copies the current line out to a specified file. It has no
 *  effect on the fileGetc () function.  During copying, any '\' characters
 *  are doubled and a leading '^' or trailing '$' is also quoted. End of line
 *  characters (line feed or carriage return) are dropped.
 */
static size_t writeSourceLine (FILE *const fp, const char *const line)
{
	size_t length = 0;
	const char *p;

	/*  Write everything up to, but not including, a line end character.
	 */
	for (p = line  ;  *p != '\0'  ;  ++p)
	{
		const int next = *(p + 1);
		const int c = *p;

		if (c == CRETURN  ||  c == NEWLINE)
			break;

		/*  If character is '\', or a terminal '$', then quote it.
		 */
		if (c == BACKSLASH  ||  c == (Option.backward ? '?' : '/')  ||
			(c == '$'  &&  (next == NEWLINE  ||  next == CRETURN)))
		{
			putc (BACKSLASH, fp);
			++length;
		}
		putc (c, fp);
		++length;
	}
	return length;
}

/*  Writes "line", stripping leading and duplicate white space.
 */
static size_t writeCompactSourceLine (FILE *const fp, const char *const line)
{
	boolean lineStarted = FALSE;
	size_t  length = 0;
	const char *p;
	int c;

	/*  Write everything up to, but not including, the newline.
	 */
	for (p = line, c = *p  ;  c != NEWLINE  &&  c != '\0'  ;  c = *++p)
	{
		if (lineStarted  || ! isspace (c))  /* ignore leading spaces */
		{
			lineStarted = TRUE;
			if (isspace (c))
			{
				int next;

				/*  Consume repeating white space.
				 */
				while (next = *(p+1) , isspace (next)  &&  next != NEWLINE)
					++p;
				c = ' ';  /* force space character for any white space */
			}
			if (c != CRETURN  ||  *(p + 1) != NEWLINE)
			{
				putc (c, fp);
				++length;
			}
		}
	}
	return length;
}

static int writeXrefEntry (const tagEntryInfo *const tag)
{
	const char *const line =
			readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	int length;

	if (Option.tagFileFormat == 1)
		length = fprintf (TagFile.fp, "%-16s %4lu %-16s ", tag->name,
				tag->lineNumber, tag->sourceFileName);
	else
		length = fprintf (TagFile.fp, "%-16s %-10s %4lu %-16s ", tag->name,
				tag->kindName, tag->lineNumber, tag->sourceFileName);

	length += writeCompactSourceLine (TagFile.fp, line);
	putc (NEWLINE, TagFile.fp);
	++length;

	return length;
}

/*  Truncates the text line containing the tag at the character following the
 *  tag, providing a character which designates the end of the tag.
 */
static void truncateTagLine (
		char *const line, const char *const token, const boolean discardNewline)
{
	char *p = strstr (line, token);

	if (p != NULL)
	{
		p += strlen (token);
		if (*p != '\0'  &&  ! (*p == '\n'  &&  discardNewline))
			++p;    /* skip past character terminating character */
		*p = '\0';
	}
}

static int writeEtagsEntry (const tagEntryInfo *const tag)
{
	int length;

	if (tag->isFileEntry)
		length = fprintf (TagFile.etags.fp, "\177%s\001%lu,0\n",
				tag->name, tag->lineNumber);
	else
	{
		long seekValue;
		char *const line =
				readSourceLine (TagFile.vLine, tag->filePosition, &seekValue);

		if (tag->truncateLine)
			truncateTagLine (line, tag->name, TRUE);
		else
			line [strlen (line) - 1] = '\0';

		length = fprintf (TagFile.etags.fp, "%s\177%s\001%lu,%ld\n", line,
				tag->name, tag->lineNumber, seekValue);
	}
	TagFile.etags.byteCount += length;

	return length;
}

static int addExtensionFields (const tagEntryInfo *const tag)
{
	const char* const kindKey = Option.extensionFields.kindKey ? "kind:" : "";
	boolean first = TRUE;
	const char* separator = ";\"";
	const char* const empty = "";
	int length = 0;
/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = FALSE, separator) : empty)

	if (tag->kindName != NULL && (Option.extensionFields.kindLong  ||
		 (Option.extensionFields.kind  && tag->kind == '\0')))
		length += fprintf (TagFile.fp,"%s\t%s%s", sep, kindKey, tag->kindName);
	else if (tag->kind != '\0'  && (Option.extensionFields.kind  ||
			(Option.extensionFields.kindLong  &&  tag->kindName == NULL)))
		length += fprintf (TagFile.fp, "%s\t%s%c", sep, kindKey, tag->kind);

	if (Option.extensionFields.lineNumber)
		length += fprintf (TagFile.fp, "%s\tline:%ld", sep, tag->lineNumber);

	if (Option.extensionFields.language  &&  tag->language != NULL)
		length += fprintf (TagFile.fp, "%s\tlanguage:%s", sep, tag->language);

	if (Option.extensionFields.scope  &&
			tag->extensionFields.scope [0] != NULL  &&
			tag->extensionFields.scope [1] != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				tag->extensionFields.scope [0],
				tag->extensionFields.scope [1]);

	if (Option.extensionFields.typeRef  &&
			tag->extensionFields.typeRef [0] != NULL  &&
			tag->extensionFields.typeRef [1] != NULL)
		length += fprintf (TagFile.fp, "%s\ttyperef:%s:%s", sep,
				tag->extensionFields.typeRef [0],
				tag->extensionFields.typeRef [1]);

	if (Option.extensionFields.fileScope  &&  tag->isFileScope)
		length += fprintf (TagFile.fp, "%s\tfile:", sep);

	if (Option.extensionFields.inheritance  &&
			tag->extensionFields.inheritance != NULL)
		length += fprintf (TagFile.fp, "%s\tinherits:%s", sep,
				tag->extensionFields.inheritance);

	if (Option.extensionFields.access  &&  tag->extensionFields.access != NULL)
		length += fprintf (TagFile.fp, "%s\taccess:%s", sep,
				tag->extensionFields.access);

	if (Option.extensionFields.implementation  &&
			tag->extensionFields.implementation != NULL)
		length += fprintf (TagFile.fp, "%s\timplementation:%s", sep,
				tag->extensionFields.implementation);

	if (Option.extensionFields.signature  &&
			tag->extensionFields.signature != NULL)
		length += fprintf (TagFile.fp, "%s\tsignature:%s", sep,
				tag->extensionFields.signature);

	return length;
#undef sep
}

static int writePatternEntry (const tagEntryInfo *const tag)
{
	char *const line = readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	const int searchChar = Option.backward ? '?' : '/';
	boolean newlineTerminated;
	int length = 0;

	if (tag->truncateLine)
		truncateTagLine (line, tag->name, FALSE);
	newlineTerminated = (boolean) (line [strlen (line) - 1] == '\n');

	length += fprintf (TagFile.fp, "%c^", searchChar);
	length += writeSourceLine (TagFile.fp, line);
	length += fprintf (TagFile.fp, "%s%c", newlineTerminated ? "$":"", searchChar);

	return length;
}

static int writeLineNumberEntry (const tagEntryInfo *const tag)
{
	return fprintf (TagFile.fp, "%lu", tag->lineNumber);
}

static int writeCtagsEntry (const tagEntryInfo *const tag)
{
	int length = fprintf (TagFile.fp, "%s\t%s\t",
		tag->name, tag->sourceFileName);

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (tag);
	else
		length += writePatternEntry (tag);

	if (includeExtensionFlags ())
		length += addExtensionFields (tag);

	length += fprintf (TagFile.fp, "\n");

	return length;
}

extern void makeTagEntry (const tagEntryInfo *const tag)
{
	Assert (tag->name != NULL);
	if (tag->name [0] == '\0')
		error (WARNING, "ignoring null tag in %s", vStringValue (File.name));
	else
	{
		int length = 0;

		DebugStatement ( debugEntry (tag); )
		if (Option.xref)
		{
			if (! tag->isFileEntry)
				length = writeXrefEntry (tag);
		}
		else if (Option.etags)
			length = writeEtagsEntry (tag);
		else
			length = writeCtagsEntry (tag);

		++TagFile.numTags.added;
		rememberMaxLengths (strlen (tag->name), (size_t) length);
		DebugStatement ( fflush (TagFile.fp); )
	}
}

extern void initTagEntry (tagEntryInfo *const e, const char *const name)
{
	Assert (File.source.name != NULL);
	memset (e, 0, sizeof (tagEntryInfo));
	e->lineNumberEntry = (boolean) (Option.locate == EX_LINENUM);
	e->lineNumber      = getSourceLineNumber ();
	e->language        = getSourceLanguageName ();
	e->filePosition    = getInputFilePosition ();
	e->sourceFileName  = getSourceFileTagPath ();
	e->name            = name;
}

/* vi:set tabstop=4 shiftwidth=4: */
