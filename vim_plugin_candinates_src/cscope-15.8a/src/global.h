/*===========================================================================
 Copyright (c) 1998-2000, The Santa Cruz Operation 
 All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 *Redistributions of source code must retain the above copyright notice,
 this list of conditions and the following disclaimer.

 *Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the following disclaimer in the documentation
 and/or other materials provided with the distribution.

 *Neither name of The Santa Cruz Operation nor the names of its contributors
 may be used to endorse or promote products derived from this software
 without specific prior written permission. 

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 DAMAGE. 
 =========================================================================*/

/* $Id: global.h,v 1.38 2012/03/05 19:28:13 nhorman Exp $ */

/*	cscope - interactive C symbol cross-reference
 *
 *	global type, data, and function definitions
 */

#ifndef CSCOPE_GLOBAL_H
#define CSCOPE_GLOBAL_H

#include "config.h"
#include <unistd.h>
#include <sys/types.h>
#include <ctype.h>	/* isalpha, isdigit, etc. */
#include <signal.h>	/* SIGINT and SIGQUIT */
#include <stdio.h>	/* standard I/O package */
#include <stdlib.h>     /* standard library functions */

/* A special "magic" header file required by HP/Compaq NSK (Non-Stop
 * Kernel) to present a more Unix-ish environment ... */
#ifdef HAVE_FLOSS_H
# include <floss.h>
#endif

/* Replace most of the #if BSD stuff. Taken straight from the autoconf
 * manual, with an extension for handling memset(). */
#if STDC_HEADERS
# include <string.h>	/* string functions */
#else
# ifndef HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
char *strchr (), *strrchr ();
# ifndef HAVE_MEMCPY
#  define memcpy(d, s, n) bcopy ((s), (d), (n))
#  define memmove(d, s, n) bcopy ((s), (d), (n))
# endif
# ifndef HAVE_MEMSET
#  ifndef HAVE_MEMORY_H
char	*memset();
#  else 
#   include <memory.h>	/* memset */
#  endif /*V9*/
# endif /* HAVE_MEMSET */
#endif /* STDC_HEADERS */

#include "constants.h"	/* misc. constants */
#include "invlib.h"	/* inverted index library */
#include "library.h"	/* library function return values */

/* Fallback, in case 'configure' failed to do its part of the job */
#ifndef RETSIGTYPE
#if SVR2 || BSD && !sun
#define RETSIGTYPE int
#else
#define RETSIGTYPE void
#endif
#endif /* RETSIGTYPE */

#ifndef HAVE_SIGHANDLER_T
typedef RETSIGTYPE (*sighandler_t)(int);
#endif

#if HAVE_STDARG_H
#include <stdarg.h>
#if !HAVE_VSNPRINTF
int rpl_vsnprintf(char *, size_t, const char *, va_list);
#endif
#if !HAVE_SNPRINTF
int rpl_snprintf(char *, size_t, const char *, ...);
#endif
#if !HAVE_VASPRINTF
int rpl_vasprintf(char **, const char *, va_list);
#endif
#if !HAVE_ASPRINTF
int rpl_asprintf(char **, const char *, ...);
#endif
#endif	/* HAVE_STDARG_H */

/* FIXME: this testing for platforms is foolish. Stop it! */
#if BSD
# undef	tolower		/* BSD toupper and tolower don't test the character */
# undef	toupper
# define	tolower(c)	(isupper(c) ? (c) - 'A' + 'a' : (c))	
# define	toupper(c)	(islower(c) ? (c) - 'a' + 'A' : (c))
# if !sun 
#  if !__FreeBSD__
/* in traditional BSD, *printf() doesn't return the number of bytes
 * written */
#   define PRINTF_RETVAL_BROKEN 1
#  endif /* !FreeBSD */
# endif /* !sun */
#endif

/* Un-comment this if you're on a filesystem that doesn't support
 * filenames longer than 14 characters */
/* HBB FIXME 20030302: should have an autoconf test for this: */
/* #define SHORT_NAMES_ONLY */

/* Just in case autoconf didn't correctly flag HAVE_FIXKEYPAD */
#ifndef HAVE_FIXKEYPAD 
# if SVR2 && !BSD && !V9 && !u3b2 && !sun
#  define HAVE_FIXKEYPAD
# endif
#endif

/* HBB 20020728: if <fcntl.h> is there, #include it here, since that's
 * where the system definitions of O_TEXT should be coming from */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

/* HBB 20020103: Need to force text or binary mode opens on Cygwins,
 * because of their "binary/text mode mount" silliness :-( */
#ifndef O_TEXT
# ifdef _O_TEXT
#  define O_TEXT _O_TEXT
# else
#  define O_TEXT 0x00
# endif
#endif
/* Same for binary mode --- moved here from vp.h */
#ifndef O_BINARY
# ifdef _O_BINARY
#  define O_BINARY _O_BINARY
# else
#  define O_BINARY 0x00
# endif
#endif 

#undef SETMODE
#if O_BINARY || O_TEXT
/* OK, looks like we are on an MSDOS-ish platform ---> define SETMODE
 * to actually do something */
# ifdef HAVE_SETMODE
#  define SETMODE(fildes, mode) setmode(fildes,mode)
# else 
#  ifdef HAVE__SETMODE
#   define SETMODE(fildes, mode) _setmode(fildes,mode)
#  endif
# endif
#endif

/* access(2) parameters. Only make assumptions about their values if
 * <unistd.h> fails to define them. */
#ifdef R_OK
# define	READ	R_OK
#else
# define	READ	4	
#endif
#ifdef W_OK
# define	WRITE	W_OK
#else
# define	WRITE	2
#endif

/* This can happen on only vaguely Unix-ish platforms... */
#ifndef HAVE_LSTAT
# define	lstat(file,buf)	stat(file,buf)
#endif

typedef	enum	{		/* boolean data type */
	NO,
	YES
} BOOL;

typedef	enum	{		/* findinit return code */
	NOERROR,
	NOTSYMBOL,
	REGCMPERROR
} FINDINIT;

typedef	struct {		/* mouse action */
	int	button;
	int	percent;
	int	x1;
	int	y1;
	int	x2;
	int	y2;
} MOUSE;

struct cmd {			/* command history struct */
	struct	cmd *prev, *next;	/* list ptrs */
	int	field;			/* input field number */
	char	*text;			/* input field text */
};

/* digraph data for text compression */
extern	char	dichar1[];	/* 16 most frequent first chars */
extern	char	dichar2[];	/* 8 most frequent second chars 
				   using the above as first chars */
extern	char	dicode1[];	/* digraph first character code */
extern	char	dicode2[];	/* digraph second character code */

/* and some macros to help using dicodes: */
/* Check if a given pair of chars is compressable as a dicode: */
#define IS_A_DICODE(inchar1, inchar2)					   \
  (dicode1[(unsigned char)(inchar1)] && dicode2[(unsigned char)(inchar2)])
/* Combine the pair into a dicode */
#define DICODE_COMPRESS(inchar1, inchar2)		\
  ((0200 - 2) + dicode1[(unsigned char)(inchar1)]	\
   + dicode2[(unsigned char)(inchar2)])

/* main.c global data */
extern	char	*editor, *home, *shell, *lineflag;	/* environment variables */
extern	char	*home;		/* Home directory */
extern 	BOOL	lineflagafterfile;
extern	char	*argv0;		/* command name */
extern	BOOL	compress;	/* compress the characters in the crossref */
extern	BOOL	dbtruncated;	/* database symbols truncated to 8 chars */
extern	int	dispcomponents;	/* file path components to display */
#if CCS
extern	BOOL	displayversion;	/* display the C Compilation System version */
#endif
extern	BOOL	editallprompt;	/* prompt between editing files */
extern	unsigned int fileargc;	/* file argument count */
extern	char	**fileargv;	/* file argument values */
extern	int	fileversion;	/* cross-reference file version */
extern	BOOL	incurses;	/* in curses */
extern	BOOL	invertedindex;	/* the database has an inverted index */
extern	BOOL	isuptodate;	/* consider the crossref up-to-date */
extern	BOOL	kernelmode;	/* don't use DFLT_INCDIR - bad for kernels */
extern	BOOL	linemode;	/* use line oriented user interface */
extern	BOOL	verbosemode;	/* print extra information on line mode */
extern	BOOL	recurse_dir;	/* recurse dirs when searching for src files */
extern	char	*namefile;	/* file of file names */
extern	BOOL	ogs;		/* display OGS book and subsystem names */
extern	char	*prependpath;	/* prepend path to file names */
extern	FILE	*refsfound;	/* references found file */
extern	char	temp1[];	/* temporary file name */
extern	char	temp2[];	/* temporary file name */
extern	long	totalterms;	/* total inverted index terms */
extern	BOOL	trun_syms;	/* truncate symbols to 8 characters */
extern	char	tempstring[TEMPSTRING_LEN + 1]; /* global dummy string buffer */
extern	char	*tmpdir;	/* temporary directory */

/* command.c global data */
extern	BOOL	caseless;	/* ignore letter case when searching */
extern	BOOL	*change;	/* change this line */
extern	BOOL	changing;	/* changing text */
extern	int	selecting;
extern	unsigned int curdispline;
extern	char	newpat[];	/* new pattern */
extern	char	Pattern[];	/* symbol or text pattern */

/* crossref.c global data */
extern	long	dboffset;	/* new database offset */
extern	BOOL	errorsfound;	/* prompt before clearing error messages */
extern	long	lineoffset;	/* source line database offset */
extern	long	npostings;	/* number of postings */
extern	unsigned long symbols;	/* number of symbols */

/* dir.c global data */
extern	char	currentdir[];	/* current directory */
extern	char	**incdirs;	/* #include directories */
extern	char	**srcdirs;	/* source directories */
extern	char	**srcfiles;	/* source files */
extern	unsigned long nincdirs;	/* number of #include directories */
extern	unsigned long nsrcdirs;	/* number of source directories */
extern	unsigned long nsrcfiles; /* number of source files */
extern	unsigned long msrcfiles; /* maximum number of source files */

/* display.c global data */
extern 	int	booklen;	/* OGS book name display field length */
extern	int	*displine;	/* screen line of displayed reference */
extern	unsigned int disprefs;	/* displayed references */
extern	int	fcnlen;		/* function name display field length */
extern	int	field;		/* input field */
extern	int	filelen;	/* file name display field length */
extern	unsigned fldcolumn;	/* input field column */
extern	unsigned int mdisprefs;	/* maximum displayed references */
extern	unsigned int nextline;	/* next line to be shown */
extern	FILE	*nonglobalrefs;	/* non-global references file */
extern	int	numlen;		/* line number display field length */
extern	unsigned int topline;	/* top line of page */
extern	int	bottomline;	/* bottom line of page */
extern	long	searchcount;	/* count of files searched */
extern	int	subsystemlen;	/* OGS subsystem name display field length */
extern	unsigned int totallines; /* total reference lines */
extern	const char dispchars[];	/* display chars for jumping to lines */

/* find.c global data */
extern	char	block[];	/* cross-reference file block */
extern	char	blockmark;	/* mark character to be searched for */
extern	long	blocknumber;	/* block number */
extern	char	*blockp;	/* pointer to current character in block */
extern	int	blocklen;	/* length of disk block read */

/* lookup.c global data */
extern	struct	keystruct {
	char	*text;
	char	delim;
	struct	keystruct *next;
} keyword[];

/* mouse.c global data */
extern	BOOL	mouse;		/* mouse interface */

#if UNIXPC
extern	BOOL	unixpcmouse;		/* UNIX PC mouse interface */
#endif

/* cscope functions called from more than one function or between files */ 

char	*filepath(char *file);
char	*findcalledby(char *pattern);
char	*findcalling(char *pattern);
char	*findallfcns(char *dummy);
char	*finddef(char *pattern);
char	*findfile(char *dummy);
char	*findinclude(char *pattern);
char	*findsymbol(char *pattern);
char	*findassign(char *pattern);
char	*findregexp(char *egreppat);
char	*findstring(char *pattern);
char	*inviewpath(char *file);
char	*lookup(char *ident);
char	*pathcomponents(char *path, int components);
char	*read_block(void);
char	*scanpast(char c);


void	addcmd(int f, char *s);
void	addsrcfile(char *path);
void	askforchar(void);
void	askforreturn(void);
void	atchange(void);
void	atfield(void);
void	cannotwrite(char *file);
void	cannotopen(char *file);
void	clearmsg(void);
void	clearmsg2(void);
void	countrefs(void);
void	crossref(char *srcfile);
void    dispinit(void);
void	display(void);
void	drawscrollbar(int top, int bot);
void	edit(char *file, char *linenum);
void	editall(void);
void	editref(int);
void	entercurses(void);
void	exitcurses(void);
void	findcleanup(void);
void    freesrclist(void);
void    freeinclist(void);
void    freecrossref(void);
void	freefilelist(void);
void	help(void);
void	incfile(char *file, char *type);
void    includedir(char *_dirname);
void    initsymtab(void);
void	makefilelist(void);
void	mousecleanup(void);
void	mousemenu(void);
void	mouseinit(void);
void	mousereinit(void);
void	myexit(int sig);
void	myperror(char *text);
void	ogsnames(char *file, char **subsystem, char **book);
void	progress(char *what, long current, long max);
void	putfilename(char *srcfile);
void	postmsg(char *msg);
void	postmsg2(char *msg);
void	posterr(char *msg,...);
void	postfatal(const char *msg,...);
void	putposting(char *term, int type);
void	fetch_string_from_dbase(char *, size_t);
void	resetcmd(void);
void	seekline(unsigned int line);
void	setfield(void);
void	shellpath(char *out, int limit, char *in);
void    sourcedir(char *dirlist);
void	myungetch(int c);
void	warning(char *text);
void	writestring(char *s);

BOOL	command(int commandc);
BOOL	infilelist(char *file);
BOOL	readrefs(char *filename);
BOOL	search(void);
BOOL	writerefsfound(void);

FINDINIT findinit(char *pattern);
MOUSE	*getmouseaction(char leading_char);
struct	cmd *currentcmd(void);
struct	cmd *prevcmd(void);
struct	cmd *nextcmd(void);

int	egrep(char *file, FILE *output, char *format);
int	mygetline(char p[], char s[], unsigned size, int firstchar, BOOL iscaseless);
int	mygetch(void);
int	hash(char *ss);
int	execute(char *a, ...);
long	dbseek(long offset);


#endif /* CSCOPE_GLOBAL_H */
