/*
*   $Id: routines.h 536 2007-06-02 06:09:00Z elliotth $
*
*   Copyright (c) 2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to routines.c
*/
#ifndef _ROUTINES_H
#define _ROUTINES_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

/*
*   MACROS
*/
#define xMalloc(n,Type)    (Type *)eMalloc((size_t)(n) * sizeof (Type))
#define xCalloc(n,Type)    (Type *)eCalloc((size_t)(n), sizeof (Type))
#define xRealloc(p,n,Type) (Type *)eRealloc((p), (n) * sizeof (Type))

/*
 *  Portability macros
 */
#ifndef PATH_SEPARATOR
# if defined (MSDOS_STYLE_PATH)
#  define PATH_SEPARATOR '\\'
# elif defined (QDOS)
#  define PATH_SEPARATOR '_'
# else
#  define PATH_SEPARATOR '/'
# endif
#endif

#if defined (MSDOS_STYLE_PATH) && defined (UNIX_PATH_SEPARATOR)
# define OUTPUT_PATH_SEPARATOR	'/'
#else
# define OUTPUT_PATH_SEPARATOR	PATH_SEPARATOR
#endif

/*
*   DATA DECLARATIONS
*/
#if defined (MSDOS_STYLE_PATH) || defined (VMS)
extern const char *const PathDelimiters;
#endif
extern char *CurrentDirectory;
typedef int errorSelection;
enum eErrorTypes { FATAL = 1, WARNING = 2, PERROR = 4 };

typedef struct {
		/* Name of file for which status is valid */
	char* name;

		/* Does file exist? If not, members below do not contain valid data. */
	boolean exists;

		/* is file path a symbolic link to another file? */
	boolean isSymbolicLink;

		/* Is file (pointed to) a directory? */
	boolean isDirectory;

		/* Is file (pointed to) a normal file? */
	boolean isNormalFile;

		/* Is file (pointed to) executable? */
	boolean isExecutable;

		/* Is file (pointed to) setuid? */
	boolean isSetuid;

		/* Size of file (pointed to) */
	unsigned long size;
} fileStatus; 

/*
*   FUNCTION PROTOTYPES
*/
extern void freeRoutineResources (void);
extern void setExecutableName (const char *const path);
extern const char *getExecutableName (void);
extern const char *getExecutablePath (void);
extern void error (const errorSelection selection, const char *const format, ...) __printf__ (2, 3);

/* Memory allocation functions */
#ifdef NEED_PROTO_MALLOC
extern void *malloc (size_t);
extern void *realloc (void *ptr, size_t);
#endif
extern void *eMalloc (const size_t size);
extern void *eCalloc (const size_t count, const size_t size);
extern void *eRealloc (void *const ptr, const size_t size);
extern void eFree (void *const ptr);

/* String manipulation functions */
extern int struppercmp (const char *s1, const char *s2);
extern int strnuppercmp (const char *s1, const char *s2, size_t n);
#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr);
#endif
extern char* eStrdup (const char* str);
extern void toLowerString (char* str);
extern void toUpperString (char* str);
extern char* newLowerString (const char* str);
extern char* newUpperString (const char* str);

/* File system functions */
extern void setCurrentDirectory (void);
extern fileStatus *eStat (const char *const fileName);
extern void eStatFree (fileStatus *status);
extern boolean doesFileExist (const char *const fileName);
extern boolean isRecursiveLink (const char* const dirName);
extern boolean isSameFile (const char *const name1, const char *const name2);
#if defined(NEED_PROTO_FGETPOS)
extern int fgetpos  (FILE *stream, fpos_t *pos);
extern int fsetpos  (FILE *stream, fpos_t *pos);
#endif
extern const char *baseFilename (const char *const filePath);
extern const char *fileExtension (const char *const fileName);
extern boolean isAbsolutePath (const char *const path);
extern vString *combinePathAndFile (const char *const path, const char *const file);
extern char* absoluteFilename (const char *file);
extern char* absoluteDirname (char *file);
extern char* relativeFilename (const char *file, const char *dir);
extern FILE *tempFile (const char *const mode, char **const pName);

#endif  /* _ROUTINES_H */

/* vi:set tabstop=4 shiftwidth=4: */
