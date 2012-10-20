/*
*   $Id: read.c 708 2009-07-04 05:29:02Z dhiebert $
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains low level source and tag file read functions (newline
*   conversion for source files are performed at this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#define FILE_WRITE
#include "read.h"
#include "debug.h"
#include "entry.h"
#include "main.h"
#include "routines.h"
#include "options.h"

/*
*   DATA DEFINITIONS
*/
inputFile File;  /* globally read through macros */
static fpos_t StartOfLine;  /* holds deferred position of start of line */

/*
*   FUNCTION DEFINITIONS
*/

extern void freeSourceFileResources (void)
{
	if (File.name != NULL)
		vStringDelete (File.name);
	if (File.path != NULL)
		vStringDelete (File.path);
	if (File.source.name != NULL)
		vStringDelete (File.source.name);
	if (File.source.tagPath != NULL)
		eFree (File.source.tagPath);
	if (File.line != NULL)
		vStringDelete (File.line);
}

/*
 *   Source file access functions
 */

static void setInputFileName (const char *const fileName)
{
	const char *const head = fileName;
	const char *const tail = baseFilename (head);

	if (File.name != NULL)
		vStringDelete (File.name);
	File.name = vStringNewInit (fileName);

	if (File.path != NULL)
		vStringDelete (File.path);
	if (tail == head)
		File.path = NULL;
	else
	{
		const size_t length = tail - head - 1;
		File.path = vStringNew ();
		vStringNCopyS (File.path, fileName, length);
	}
}

static void setSourceFileParameters (vString *const fileName)
{
	if (File.source.name != NULL)
		vStringDelete (File.source.name);
	File.source.name = fileName;

	if (File.source.tagPath != NULL)
		eFree (File.source.tagPath);
	if (! Option.tagRelative || isAbsolutePath (vStringValue (fileName)))
		File.source.tagPath = eStrdup (vStringValue (fileName));
	else
		File.source.tagPath =
				relativeFilename (vStringValue (fileName), TagFile.directory);

	if (vStringLength (fileName) > TagFile.max.file)
		TagFile.max.file = vStringLength (fileName);

	File.source.isHeader = isIncludeFile (vStringValue (fileName));
	File.source.language = getFileLanguage (vStringValue (fileName));
}

static boolean setSourceFileName (vString *const fileName)
{
	boolean result = FALSE;
	if (getFileLanguage (vStringValue (fileName)) != LANG_IGNORE)
	{
		vString *pathName;
		if (isAbsolutePath (vStringValue (fileName)) || File.path == NULL)
			pathName = vStringNewCopy (fileName);
		else
			pathName = combinePathAndFile (
					vStringValue (File.path), vStringValue (fileName));
		setSourceFileParameters (pathName);
		result = TRUE;
	}
	return result;
}

/*
 *   Line directive parsing
 */

static int skipWhite (void)
{
	int c;
	do
		c = getc (File.fp);
	while (c == ' '  ||  c == '\t');
	return c;
}

static unsigned long readLineNumber (void)
{
	unsigned long lNum = 0;
	int c = skipWhite ();
	while (c != EOF  &&  isdigit (c))
	{
		lNum = (lNum * 10) + (c - '0');
		c = getc (File.fp);
	}
	ungetc (c, File.fp);
	if (c != ' '  &&  c != '\t')
		lNum = 0;

	return lNum;
}

/* While ANSI only permits lines of the form:
 *   # line n "filename"
 * Earlier compilers generated lines of the form
 *   # n filename
 * GNU C will output lines of the form:
 *   # n "filename"
 * So we need to be fairly flexible in what we accept.
 */
static vString *readFileName (void)
{
	vString *const fileName = vStringNew ();
	boolean quoteDelimited = FALSE;
	int c = skipWhite ();

	if (c == '"')
	{
		c = getc (File.fp);  /* skip double-quote */
		quoteDelimited = TRUE;
	}
	while (c != EOF  &&  c != '\n'  &&
			(quoteDelimited ? (c != '"') : (c != ' '  &&  c != '\t')))
	{
		vStringPut (fileName, c);
		c = getc (File.fp);
	}
	if (c == '\n')
		ungetc (c, File.fp);
	vStringPut (fileName, '\0');

	return fileName;
}

static boolean parseLineDirective (void)
{
	boolean result = FALSE;
	int c = skipWhite ();
	DebugStatement ( const char* lineStr = ""; )

	if (isdigit (c))
	{
		ungetc (c, File.fp);
		result = TRUE;
	}
	else if (c == 'l'  &&  getc (File.fp) == 'i'  &&
			 getc (File.fp) == 'n'  &&  getc (File.fp) == 'e')
	{
		c = getc (File.fp);
		if (c == ' '  ||  c == '\t')
		{
			DebugStatement ( lineStr = "line"; )
			result = TRUE;
		}
	}
	if (result)
	{
		const unsigned long lNum = readLineNumber ();
		if (lNum == 0)
			result = FALSE;
		else
		{
			vString *const fileName = readFileName ();
			if (vStringLength (fileName) == 0)
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld", lineStr, lNum); )
			}
			else if (setSourceFileName (fileName))
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld \"%s\"",
								lineStr, lNum, vStringValue (fileName)); )
			}

			if (Option.include.fileNames && vStringLength (fileName) > 0 &&
				lNum == 1)
			{
				tagEntryInfo tag;
				initTagEntry (&tag, baseFilename (vStringValue (fileName)));

				tag.isFileEntry     = TRUE;
				tag.lineNumberEntry = TRUE;
				tag.lineNumber      = 1;
				tag.kindName        = "file";
				tag.kind            = 'F';

				makeTagEntry (&tag);
			}
			vStringDelete (fileName);
			result = TRUE;
		}
	}
	return result;
}

/*
 *   Source file I/O operations
 */

/*  This function opens a source file, and resets the line counter.  If it
 *  fails, it will display an error message and leave the File.fp set to NULL.
 */
extern boolean fileOpen (const char *const fileName, const langType language)
{
#ifdef VMS
	const char *const openMode = "r";
#else
	const char *const openMode = "rb";
#endif
	boolean opened = FALSE;

	/*	If another file was already open, then close it.
	 */
	if (File.fp != NULL)
	{
		fclose (File.fp);  /* close any open source file */
		File.fp = NULL;
	}

	File.fp = fopen (fileName, openMode);
	if (File.fp == NULL)
		error (WARNING | PERROR, "cannot open \"%s\"", fileName);
	else
	{
		opened = TRUE;

		setInputFileName (fileName);
		fgetpos (File.fp, &StartOfLine);
		fgetpos (File.fp, &File.filePosition);
		File.currentLine  = NULL;
		File.language     = language;
		File.lineNumber   = 0L;
		File.eof          = FALSE;
		File.newLine      = TRUE;

		if (File.line != NULL)
			vStringClear (File.line);

		setSourceFileParameters (vStringNewInit (fileName));
		File.source.lineNumber = 0L;

		verbose ("OPENING %s as %s language %sfile\n", fileName,
				getLanguageName (language),
				File.source.isHeader ? "include " : "");
	}
	return opened;
}

extern void fileClose (void)
{
	if (File.fp != NULL)
	{
		/*  The line count of the file is 1 too big, since it is one-based
		 *  and is incremented upon each newline.
		 */
		if (Option.printTotals)
		{
			fileStatus *status = eStat (vStringValue (File.name));
			addTotals (0, File.lineNumber - 1L, status->size);
		}
		fclose (File.fp);
		File.fp = NULL;
	}
}

extern boolean fileEOF (void)
{
	return File.eof;
}

/*  Action to take for each encountered source newline.
 */
static void fileNewline (void)
{
	File.filePosition = StartOfLine;
	File.newLine = FALSE;
	File.lineNumber++;
	File.source.lineNumber++;
	DebugStatement ( if (Option.breakLine == File.lineNumber) lineBreak (); )
	DebugStatement ( debugPrintf (DEBUG_RAW, "%6ld: ", File.lineNumber); )
}

/*  This function reads a single character from the stream, performing newline
 *  canonicalization.
 */
static int iFileGetc (void)
{
	int	c;
readnext:
	c = getc (File.fp);

	/*	If previous character was a newline, then we're starting a line.
	 */
	if (File.newLine  &&  c != EOF)
	{
		fileNewline ();
		if (c == '#'  &&  Option.lineDirectives)
		{
			if (parseLineDirective ())
				goto readnext;
			else
			{
				fsetpos (File.fp, &StartOfLine);
				c = getc (File.fp);
			}
		}
	}

	if (c == EOF)
		File.eof = TRUE;
	else if (c == NEWLINE)
	{
		File.newLine = TRUE;
		fgetpos (File.fp, &StartOfLine);
	}
	else if (c == CRETURN)
	{
		/* Turn line breaks into a canonical form. The three commonly
		 * used forms if line breaks: LF (UNIX/Mac OS X), CR (Mac OS 9),
		 * and CR-LF (MS-DOS) are converted into a generic newline.
		 */
#ifndef macintosh
		const int next = getc (File.fp);  /* is CR followed by LF? */
		if (next != NEWLINE)
			ungetc (next, File.fp);
		else
#endif
		{
			c = NEWLINE;  /* convert CR into newline */
			File.newLine = TRUE;
			fgetpos (File.fp, &StartOfLine);
		}
	}
	DebugStatement ( debugPutc (DEBUG_RAW, c); )
	return c;
}

extern void fileUngetc (int c)
{
	File.ungetch = c;
}

static vString *iFileGetLine (void)
{
	vString *result = NULL;
	int c;
	if (File.line == NULL)
		File.line = vStringNew ();
	vStringClear (File.line);
	do
	{
		c = iFileGetc ();
		if (c != EOF)
			vStringPut (File.line, c);
		if (c == '\n'  ||  (c == EOF  &&  vStringLength (File.line) > 0))
		{
			vStringTerminate (File.line);
#ifdef HAVE_REGEX
			if (vStringLength (File.line) > 0)
				matchRegex (File.line, File.source.language);
#endif
			result = File.line;
			break;
		}
	} while (c != EOF);
	Assert (result != NULL  ||  File.eof);
	return result;
}

/*  Do not mix use of fileReadLine () and fileGetc () for the same file.
 */
extern int fileGetc (void)
{
	int c;

	/*  If there is an ungotten character, then return it.  Don't do any
	 *  other processing on it, though, because we already did that the
	 *  first time it was read through fileGetc ().
	 */
	if (File.ungetch != '\0')
	{
		c = File.ungetch;
		File.ungetch = '\0';
		return c;  /* return here to avoid re-calling debugPutc () */
	}
	do
	{
		if (File.currentLine != NULL)
		{
			c = *File.currentLine++;
			if (c == '\0')
				File.currentLine = NULL;
		}
		else
		{
			vString* const line = iFileGetLine ();
			if (line != NULL)
				File.currentLine = (unsigned char*) vStringValue (line);
			if (File.currentLine == NULL)
				c = EOF;
			else
				c = '\0';
		}
	} while (c == '\0');
	DebugStatement ( debugPutc (DEBUG_READ, c); )
	return c;
}

extern int fileSkipToCharacter (int c)
{
	int d;
	do
	{
		d = fileGetc ();
	} while (d != EOF && d != c);
	return d;
}

/*  An alternative interface to fileGetc (). Do not mix use of fileReadLine()
 *  and fileGetc() for the same file. The returned string does not contain
 *  the terminating newline. A NULL return value means that all lines in the
 *  file have been read and we are at the end of file.
 */
extern const unsigned char *fileReadLine (void)
{
	vString* const line = iFileGetLine ();
	const unsigned char* result = NULL;
	if (line != NULL)
	{
		result = (const unsigned char*) vStringValue (line);
		vStringStripNewline (line);
		DebugStatement ( debugPrintf (DEBUG_READ, "%s\n", result); )
	}
	return result;
}

/*
 *   Source file line reading with automatic buffer sizing
 */
extern char *readLine (vString *const vLine, FILE *const fp)
{
	char *result = NULL;

	vStringClear (vLine);
	if (fp == NULL)  /* to free memory allocated to buffer */
		error (FATAL, "NULL file pointer");
	else
	{
		boolean reReadLine;

		/*  If reading the line places any character other than a null or a
		 *  newline at the last character position in the buffer (one less
		 *  than the buffer size), then we must resize the buffer and
		 *  reattempt to read the line.
		 */
		do
		{
			char *const pLastChar = vStringValue (vLine) + vStringSize (vLine) -2;
			fpos_t startOfLine;

			fgetpos (fp, &startOfLine);
			reReadLine = FALSE;
			*pLastChar = '\0';
			result = fgets (vStringValue (vLine), (int) vStringSize (vLine), fp);
			if (result == NULL)
			{
				if (! feof (fp))
					error (FATAL | PERROR, "Failure on attempt to read file");
			}
			else if (*pLastChar != '\0'  &&
					 *pLastChar != '\n'  &&  *pLastChar != '\r')
			{
				/*  buffer overflow */
				reReadLine = vStringAutoResize (vLine);
				if (reReadLine)
					fsetpos (fp, &startOfLine);
				else
					error (FATAL | PERROR, "input line too big; out of memory");
			}
			else
			{
				char* eol;
				vStringSetLength (vLine);
				/* canonicalize new line */
				eol = vStringValue (vLine) + vStringLength (vLine) - 1;
				if (*eol == '\r')
					*eol = '\n';
				else if (*(eol - 1) == '\r'  &&  *eol == '\n')
				{
					*(eol - 1) = '\n';
					*eol = '\0';
					--vLine->length;
				}
			}
		} while (reReadLine);
	}
	return result;
}

/*  Places into the line buffer the contents of the line referenced by
 *  "location".
 */
extern char *readSourceLine (
		vString *const vLine, fpos_t location, long *const pSeekValue)
{
	fpos_t orignalPosition;
	char *result;

	fgetpos (File.fp, &orignalPosition);
	fsetpos (File.fp, &location);
	if (pSeekValue != NULL)
		*pSeekValue = ftell (File.fp);
	result = readLine (vLine, File.fp);
	if (result == NULL)
		error (FATAL, "Unexpected end of file: %s", vStringValue (File.name));
	fsetpos (File.fp, &orignalPosition);

	return result;
}

/* vi:set tabstop=4 shiftwidth=4: */
