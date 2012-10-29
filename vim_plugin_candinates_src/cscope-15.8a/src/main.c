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


/*	cscope - interactive C symbol cross-reference
 *
 *	main functions
 */

#include "global.h"

#include "build.h"
#include "vp.h"
#include "version.h"	/* FILEVERSION and FIXVERSION */
#include "scanner.h" 
#include "alloc.h"

#include <stdlib.h>	/* atoi */
#if defined(USE_NCURSES) && !defined(RENAMED_NCURSES)
#include <ncurses.h>
#else
#include <curses.h>
#endif
#include <sys/types.h>	/* needed by stat.h */
#include <sys/stat.h>	/* stat */
#include <signal.h>
#ifdef HAVE_GETOPT_LONG 
#include <getopt.h>
#endif

/* defaults for unset environment variables */
#define	EDITOR	"vi"
#define HOME	"/"	/* no $HOME --> use root directory */
#define	SHELL	"sh"
#define LINEFLAG "+%s"	/* default: used by vi and emacs */
#define TMPDIR	"/tmp"
#ifndef DFLT_INCDIR
#define DFLT_INCDIR "/usr/include"
#endif

static char const rcsid[] = "$Id: main.c,v 1.55 2011/07/04 13:41:17 nhorman Exp $";

/* note: these digraph character frequencies were calculated from possible 
   printable digraphs in the cross-reference for the C compiler */
char	dichar1[] = " teisaprnl(of)=c";	/* 16 most frequent first chars */
char	dichar2[] = " tnerpla";		/* 8 most frequent second chars 
					   using the above as first chars */
char	dicode1[256];		/* digraph first character code */
char	dicode2[256];		/* digraph second character code */

char	*editor, *shell, *lineflag;	/* environment variables */
char	*home;			/* Home directory */
BOOL	lineflagafterfile;
char	*argv0;			/* command name */
BOOL	compress = YES;		/* compress the characters in the crossref */
BOOL	dbtruncated;		/* database symbols are truncated to 8 chars */
int	dispcomponents = 1;	/* file path components to display */
#if CCS
BOOL	displayversion;		/* display the C Compilation System version */
#endif
BOOL	editallprompt = YES;	/* prompt between editing files */
unsigned int fileargc;		/* file argument count */
char	**fileargv;		/* file argument values */
int	fileversion;		/* cross-reference file version */
BOOL	incurses = NO;		/* in curses */
BOOL	invertedindex;		/* the database has an inverted index */
BOOL	isuptodate;		/* consider the crossref up-to-date */
BOOL	kernelmode;		/* don't use DFLT_INCDIR - bad for kernels */
BOOL	linemode = NO;		/* use line oriented user interface */
BOOL	verbosemode = NO;	/* print extra information on line mode */
BOOL	recurse_dir = NO;	/* recurse dirs when searching for src files */
char	*namefile;		/* file of file names */
BOOL	ogs;			/* display OGS book and subsystem names */
char	*prependpath;		/* prepend path to file names */
FILE	*refsfound;		/* references found file */
char	temp1[PATHLEN + 1];	/* temporary file name */
char	temp2[PATHLEN + 1];	/* temporary file name */
char	tempdirpv[PATHLEN + 1];	/* private temp directory */
long	totalterms;		/* total inverted index terms */
BOOL	trun_syms;		/* truncate symbols to 8 characters */
char	tempstring[TEMPSTRING_LEN + 1]; /* use this as a buffer, instead of 'yytext', 
				 * which had better be left alone */
char	*tmpdir;		/* temporary directory */

static	BOOL	onesearch;		/* one search only in line mode */
static	char	*reflines;		/* symbol reference lines file */

/* Internal prototypes: */
static	void	initcompress(void);
static	void	longusage(void);
static	void	skiplist(FILE *oldrefs);
static	void	usage(void);

#ifdef HAVE_FIXKEYPAD
void	fixkeypad();
#endif

#if defined(KEY_RESIZE) && !defined(__DJGPP__)
void 
sigwinch_handler(int sig, siginfo_t *info, void *unused)
{
    (void) sig;
    (void) info;
    (void) unused;
    if(incurses == YES)
        ungetch(KEY_RESIZE);
}
#endif

#ifdef HAVE_GETOPT_LONG
struct option lopts[] = {
	{"help", 0, NULL, 'h'},
	{"version", 0, NULL, 'V'},
	{0, 0, 0, 0}
};

char ** parse_options(int *argc, char **argv)
{
	int opt;
	int longind;
	char path[PATHLEN + 1];     /* file path */
	char *s;
	int argcc = *argc;
	

	while ((opt = getopt_long(argcc, argv,
	       "hVbcCdeF:f:I:i:kLl0:1:2:3:4:5:6:7:8:9:P:p:qRs:TUuv",
	       lopts, &longind)) != -1) {
		switch(opt) {

		case '?':
			usage();
			myexit(1);
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			/* The input fields numbers for line mode operation */
			field = opt - '0';
			if (strlen(optarg) > PATHLEN) {
				    postfatal("\
					cscope: pattern too long, cannot be > \
					%d characters\n", PATLEN);
			}
			strcpy(Pattern, optarg);	
			break;
		case 'b':	/* only build the cross-reference */
			buildonly = YES;
			linemode  = YES;
			break;
		case 'c':	/* ASCII characters only in crossref */
			compress = NO;
			break;
		case 'C':	/* turn on caseless mode for symbol searches */
			caseless = YES;
			egrepcaseless(caseless); /* simulate egrep -i flag */
			break;
		case 'd':	/* consider crossref up-to-date */
			isuptodate = YES;
			break;
		case 'e':	/* suppress ^E prompt between files */
			editallprompt = NO;
			break;
		case 'h':
			longusage();
			myexit(1);
			break;
		case 'k':	/* ignore DFLT_INCDIR */
			kernelmode = YES;
			break;
		case 'L':
			onesearch = YES;
			/* FALLTHROUGH */
		case 'l':
			linemode = YES;
			break;
		case 'v':
			verbosemode = YES;
			break;
		case 'V':
			fprintf(stderr, "%s: version %d%s\n", argv0,
				FILEVERSION, FIXVERSION);
			myexit(0);
			break;
		case 'q':	/* quick search */
			invertedindex = YES;
			break;
		case 'T':	/* truncate symbols to 8 characters */
			trun_syms = YES;
			break;
		case 'u':	/* unconditionally build the cross-reference */
			unconditional = YES;
			break;
		case 'U':	/* assume some files have changed */
			fileschanged = YES;
			break;
		case 'R':
			recurse_dir = YES;
			break;
		case 'f':	/* alternate cross-reference file */
			reffile = optarg;
			if (strlen(reffile) > sizeof(path) - 3) {
				postfatal("\
					cscope: reffile too long, cannot \
					be > %d characters\n", sizeof(path) - 3);
				/* NOTREACHED */
			}
			strcpy(path, reffile);

			s = path + strlen(path);
			strcpy(s, ".in");
			invname = my_strdup(path);
			strcpy(s, ".po");
			invpost = my_strdup(path);
			break;

		case 'F':	/* symbol reference lines file */
			reflines = optarg;
			break;
		case 'i':	/* file containing file names */
			namefile = optarg;
			break;
		case 'I':	/* #include file directory */
			includedir(optarg);
			break;
		case 'p':	/* file path components to display */
			dispcomponents = atoi(optarg);
			break;
		case 'P':	/* prepend path to file names */
			prependpath = optarg;
			break;
		case 's':	/* additional source file directory */
			sourcedir(optarg);
			break;
		}
	}
	/*
 	 * This adjusts argv so that we only see the remaining 
 	 * args.  Its ugly, but we need to do it so that the rest
 	 * of the main routine doesn't get all confused
 	 */
	*argc = *argc - optind;
	return &argv[optind];
}
#endif

int
main(int argc, char **argv)
{
    FILE *names;			/* name file pointer */
    int	oldnum;			/* number in old cross-ref */
    char path[PATHLEN + 1];	/* file path */
    FILE *oldrefs;	/* old cross-reference file */
    char *s;
    int c;
    unsigned int i;
    pid_t pid;
    struct stat	stat_buf;
#if defined(KEY_RESIZE) && !defined(__DJGPP__)
    struct sigaction winch_action;
#endif
    mode_t orig_umask;
	
    yyin = stdin;
    yyout = stdout;
    /* save the command name for messages */
    argv0 = argv[0];

    /* set the options */
#ifdef HAVE_GETOPT_LONG 
	argv = parse_options(&argc, argv);
#else
    while (--argc > 0 && (*++argv)[0] == '-') {
	/* HBB 20030814: add GNU-style --help and --version options */
	if (strequal(argv[0], "--help")
	    || strequal(argv[0], "-h")) {
	    longusage();
	    myexit(0);
	}
	if (strequal(argv[0], "--version")
	    || strequal(argv[0], "-V")) {
#if CCS
	    displayversion = YES;
#else
	    fprintf(stderr, "%s: version %d%s\n", argv0,
		    FILEVERSION, FIXVERSION);
	    myexit(0);
#endif
	}

	for (s = argv[0] + 1; *s != '\0'; s++) {

	    /* look for an input field number */
	    if (isdigit((unsigned char) *s)) {
		field = *s - '0';
		if (field > 8) {
		    field = 8;
		}
		if (*++s == '\0' && --argc > 0) {
		    s = *++argv;
		}
		if (strlen(s) > PATLEN) {
		    postfatal("\
cscope: pattern too long, cannot be > %d characters\n", PATLEN);
		    /* NOTREACHED */
		}
		strcpy(Pattern, s);
		goto nextarg;
	    }
	    switch (*s) {
	    case '-':	/* end of options */
		--argc;
		++argv;
		goto lastarg;
	    case 'b':	/* only build the cross-reference */
		buildonly = YES;
		linemode  = YES;
		break;
	    case 'c':	/* ASCII characters only in crossref */
		compress = NO;
		break;
	    case 'C':	/* turn on caseless mode for symbol searches */
		caseless = YES;
		egrepcaseless(caseless); /* simulate egrep -i flag */
		break;
	    case 'd':	/* consider crossref up-to-date */
		isuptodate = YES;
		break;
	    case 'e':	/* suppress ^E prompt between files */
		editallprompt = NO;
		break;
	    case 'k':	/* ignore DFLT_INCDIR */
		kernelmode = YES;
		break;
	    case 'L':
		onesearch = YES;
		/* FALLTHROUGH */
	    case 'l':
		linemode = YES;
		break;
	    case 'v':
		verbosemode = YES;
		break;
	    case 'o':	/* display OGS book and subsystem names */
		ogs = YES;
		break;
	    case 'q':	/* quick search */
		invertedindex = YES;
		break;
	    case 'T':	/* truncate symbols to 8 characters */
		trun_syms = YES;
		break;
	    case 'u':	/* unconditionally build the cross-reference */
		unconditional = YES;
		break;
	    case 'U':	/* assume some files have changed */
		fileschanged = YES;
		break;
	    case 'R':
		recurse_dir = YES;
		break;
	    case 'f':	/* alternate cross-reference file */
	    case 'F':	/* symbol reference lines file */
	    case 'i':	/* file containing file names */
	    case 'I':	/* #include file directory */
	    case 'p':	/* file path components to display */
	    case 'P':	/* prepend path to file names */
	    case 's':	/* additional source file directory */
	    case 'S':
		c = *s;
		if (*++s == '\0' && --argc > 0) {
		    s = *++argv;
		}
		if (*s == '\0') {
		    fprintf(stderr, "%s: -%c option: missing or empty value\n", 
			    argv0, c);
		    goto usage;
		}
		switch (c) {
		case 'f':	/* alternate cross-reference file */
		    reffile = s;
		    if (strlen(reffile) > sizeof(path) - 3) {
			  postfatal("\
cscope: reffile too long, cannot be > %d characters\n", sizeof(path) - 3);
			  /* NOTREACHED */
		    }
		    strcpy(path, s);
#ifdef SHORT_NAMES_ONLY
		    /* System V has a 14 character limit */
		    s = mybasename(path);
		    if (strlen(s) > 11) {
			s[11] = '\0';
		    }
#endif
		    s = path + strlen(path);
		    strcpy(s, ".in");
		    invname = my_strdup(path);
		    strcpy(s, ".po");
		    invpost = my_strdup(path);
		    break;
		case 'F':	/* symbol reference lines file */
		    reflines = s;
		    break;
		case 'i':	/* file containing file names */
		    namefile = s;
		    break;
		case 'I':	/* #include file directory */
		    includedir(s);
		    break;
		case 'p':	/* file path components to display */
		    if (*s < '0' || *s > '9' ) {
			fprintf(stderr, "\
%s: -p option: missing or invalid numeric value\n", 
				argv0);
			goto usage;
		    }
		    dispcomponents = atoi(s);
		    break;
		case 'P':	/* prepend path to file names */
		    prependpath = s;
		    break;
		case 's':	/* additional source directory */
		case 'S':
		    sourcedir(s);
		    break;
		}
		goto nextarg;
	    default:
		fprintf(stderr, "%s: unknown option: -%c\n", argv0, 
			*s);
	    usage:
		usage();
		fprintf(stderr, "Try the -h option for more information.\n");
		myexit(1);
	    } /* switch(option letter) */
	} /* for(option) */
    nextarg:	
	;
    } /* while(argv) */

 lastarg:
#endif
    /* read the environment */
    editor = mygetenv("EDITOR", EDITOR);
    editor = mygetenv("VIEWER", editor); /* use viewer if set */
    editor = mygetenv("CSCOPE_EDITOR", editor);	/* has last word */
    home = mygetenv("HOME", HOME);
    shell = mygetenv("SHELL", SHELL);
    lineflag = mygetenv("CSCOPE_LINEFLAG", LINEFLAG);
    lineflagafterfile = getenv("CSCOPE_LINEFLAG_AFTER_FILE") ? 1 : 0;
    tmpdir = mygetenv("TMPDIR", TMPDIR);

    /* XXX remove if/when clearerr() in dir.c does the right thing. */
    if (namefile && strcmp(namefile, "-") == 0 && !buildonly) {
	postfatal("cscope: Must use -b if file list comes from stdin\n");
	/* NOTREACHED */
    }

    /* make sure that tmpdir exists */
    if (lstat (tmpdir, &stat_buf)) {
	fprintf (stderr, "\
cscope: Temporary directory %s does not exist or cannot be accessed\n", 
		 tmpdir);
	fprintf (stderr, "\
cscope: Please create the directory or set the environment variable\n\
cscope: TMPDIR to a valid directory\n");
	myexit(1);
    }

    /* create the temporary file names */
    orig_umask = umask(S_IRWXG|S_IRWXO);
    pid = getpid();
    snprintf(tempdirpv, sizeof(tempdirpv), "%s/cscope.%d", tmpdir, pid);
    if(mkdir(tempdirpv,S_IRWXU)) {
	fprintf(stderr, "\
cscope: Could not create private temp dir %s\n",
		tempdirpv);
	myexit(1);
    }
    umask(orig_umask);

    snprintf(temp1, sizeof(temp1), "%s/cscope.1", tempdirpv);
    snprintf(temp2, sizeof(temp2), "%s/cscope.2", tempdirpv);

    /* if running in the foreground */
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
	/* cleanup on the interrupt and quit signals */
	signal(SIGINT, myexit);
	signal(SIGQUIT, myexit);
    }
    /* cleanup on the hangup signal */
    signal(SIGHUP, myexit);

    /* ditto the TERM signal */
    signal(SIGTERM, myexit);

    /* ignore PIPE signal, so myexit() will have a chance to clean up in
     * linemode, while in curses mode the "|" command can cause a pipe signal
     * too
     */
    signal(SIGPIPE, SIG_IGN);

    /* if the database path is relative and it can't be created */
    if (reffile[0] != '/' && access(".", WRITE) != 0) {

	/* put it in the home directory if the database may not be
	 * up-to-date or doesn't exist in the relative directory,
	 * so a database in the current directory will be
	 * used instead of failing to open a non-existant database in
	 * the home directory
	 */
	snprintf(path, sizeof(path), "%s/%s", home, reffile);
	if (isuptodate == NO || access(path, READ) == 0) {
	    reffile = my_strdup(path);
	    snprintf(path, sizeof(path), "%s/%s", home, invname);
	    invname = my_strdup(path);
	    snprintf(path, sizeof(path), "%s/%s", home, invpost);
	    invpost = my_strdup(path);
	}
    }

    if (linemode == NO) {
	signal(SIGINT, SIG_IGN);	/* ignore interrupts */

#if defined(KEY_RESIZE) && !defined(__DJGPP__)
	winch_action.sa_sigaction = sigwinch_handler;
	sigemptyset(&winch_action.sa_mask);
	winch_action.sa_flags = SA_SIGINFO;
	sigaction(SIGWINCH,&winch_action,NULL);
#endif

	/* initialize the curses display package */
	initscr();	/* initialize the screen */
	entercurses();
#if TERMINFO
	keypad(stdscr, TRUE);	/* enable the keypad */
# ifdef HAVE_FIXKEYPAD
	fixkeypad();	/* fix for getch() intermittently returning garbage */
# endif
#endif /* TERMINFO */
#if UNIXPC
	standend();	/* turn off reverse video */
#endif
	dispinit();	/* initialize display parameters */
	setfield();	/* set the initial cursor position */
	clearmsg();	/* clear any build progress message */
	display();	/* display the version number and input fields */
    }


    /* if the cross-reference is to be considered up-to-date */
    if (isuptodate == YES) {
	if ((oldrefs = vpfopen(reffile, "rb")) == NULL) {
	    postfatal("cscope: cannot open file %s\n", reffile);
	    /* NOTREACHED */
	}
	/* get the crossref file version but skip the current directory */
	if (fscanf(oldrefs, "cscope %d %*s", &fileversion) != 1) {
	    postfatal("cscope: cannot read file version from file %s\n", 
		      reffile);
	    /* NOTREACHED */
	}
	if (fileversion >= 8) {

	    /* override these command line options */
	    compress = YES;
	    invertedindex = NO;

	    /* see if there are options in the database */
	    for (;;) {
		getc(oldrefs);	/* skip the blank */
		if ((c = getc(oldrefs)) != '-') {
		    ungetc(c, oldrefs);
		    break;
		}
		switch (getc(oldrefs)) {
		case 'c':	/* ASCII characters only */
		    compress = NO;
		    break;
		case 'q':	/* quick search */
		    invertedindex = YES;
		    fscanf(oldrefs, "%ld", &totalterms);
		    break;
		case 'T':	/* truncate symbols to 8 characters */
		    dbtruncated = YES;
		    trun_syms = YES;
		    break;
		}
	    }
	    initcompress();
	    seek_to_trailer(oldrefs);
	}
	/* skip the source and include directory lists */
	skiplist(oldrefs);
	skiplist(oldrefs);

	/* get the number of source files */
	if (fscanf(oldrefs, "%lu", &nsrcfiles) != 1) {
	    postfatal("\
cscope: cannot read source file size from file %s\n", reffile);
	    /* NOTREACHED */
	}
	/* get the source file list */
	srcfiles = mymalloc(nsrcfiles * sizeof(char *));
	if (fileversion >= 9) {

	    /* allocate the string space */
	    if (fscanf(oldrefs, "%d", &oldnum) != 1) {
		postfatal("\
cscope: cannot read string space size from file %s\n", reffile);
		/* NOTREACHED */
	    }
	    s = mymalloc(oldnum);
	    getc(oldrefs);	/* skip the newline */
			
	    /* read the strings */
	    if (fread(s, oldnum, 1, oldrefs) != 1) {
		postfatal("\
cscope: cannot read source file names from file %s\n", reffile);
		/* NOTREACHED */
	    }
	    /* change newlines to nulls */
	    for (i = 0; i < nsrcfiles; ++i) {
		srcfiles[i] = s;
		for (++s; *s != '\n'; ++s) {
		    ;
		}
		*s = '\0';
		++s;
	    }
	    /* if there is a file of source file names */
	    if ((namefile != NULL && (names = vpfopen(namefile, "r")) != NULL)
		|| (names = vpfopen(NAMEFILE, "r")) != NULL) {
	
		/* read any -p option from it */
		while (fgets(path, sizeof(path), names) != NULL && *path == '-') {
		    i = path[1];
		    s = path + 2;		/* for "-Ipath" */
		    if (*s == '\0') {	/* if "-I path" */
			fgets(path, sizeof(path), names);
			s = path;
		    }
		    switch (i) {
		    case 'p':	/* file path components to display */
			if (*s < '0' || *s > '9') {
			    posterr("cscope: -p option in file %s: missing or invalid numeric value\n", 								namefile);

			}
			dispcomponents = atoi(s);
		    }
		}
		fclose(names);
	    }
	} else {
	    for (i = 0; i < nsrcfiles; ++i) {
		if (!fgets(path, sizeof(path), oldrefs) ) {
		    postfatal("\
cscope: cannot read source file name from file %s\n", 
			      reffile);
		    /* NOTREACHED */
		}
		srcfiles[i] = my_strdup(path);
	    }
	}
	fclose(oldrefs);
    } else {
	/* save the file arguments */
	fileargc = argc;
	fileargv = argv;
	
	/* get source directories from the environment */
	if ((s = getenv("SOURCEDIRS")) != NULL) {
	    sourcedir(s);
	}
	/* make the source file list */
	srcfiles = mymalloc(msrcfiles * sizeof(char *));
	makefilelist();
	if (nsrcfiles == 0) {
	    postfatal("cscope: no source files found\n");
	    /* NOTREACHED */
	}
	/* get include directories from the environment */
	if ((s = getenv("INCLUDEDIRS")) != NULL) {
	    includedir(s);
	}
	/* add /usr/include to the #include directory list,
	   but not in kernelmode... kernels tend not to use it. */
	if (kernelmode == NO) {
	    if (NULL != (s = getenv("INCDIR"))) {
		includedir(s);
	    } else {
		includedir(DFLT_INCDIR);
	    }
	}

	/* initialize the C keyword table */
	initsymtab();

	/* Tell build.c about the filenames to create: */
	setup_build_filenames(reffile);

	/* build the cross-reference */
	initcompress();
	if (linemode == NO || verbosemode == YES)    /* display if verbose as well */
	    postmsg("Building cross-reference...");    		    
	build();
	if (linemode == NO )
	    clearmsg();	/* clear any build progress message */
	if (buildonly == YES) {
	    myexit(0);
	}
    }
    opendatabase();

    /* if using the line oriented user interface so cscope can be a 
       subprocess to emacs or samuel */
    if (linemode == YES) {
	if (*Pattern != '\0') {		/* do any optional search */
	    if (search() == YES) {
		/* print the total number of lines in
		 * verbose mode */
		if (verbosemode == YES)
		    printf("cscope: %d lines\n",
			   totallines);

		while ((c = getc(refsfound)) != EOF)
		    putchar(c);
	    }
	}
	if (onesearch == YES)
	    myexit(0);
		
	for (;;) {
	    char buf[PATLEN + 2];
			
	    printf(">> ");
	    fflush(stdout);
	    if (fgets(buf, sizeof(buf), stdin) == NULL) {
		myexit(0);
	    }
	    /* remove any trailing newline character */
	    if (*(s = buf + strlen(buf) - 1) == '\n') {
		*s = '\0';
	    }
	    switch (*buf) {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':	/* samuel only */
		field = *buf - '0';
		strcpy(Pattern, buf + 1);
		search();
		printf("cscope: %d lines\n", totallines);
		while ((c = getc(refsfound)) != EOF) {
		    putchar(c);
		}
		break;

	    case 'c':	/* toggle caseless mode */
	    case ctrl('C'):
		if (caseless == NO) {
		    caseless = YES;
		} else {
		    caseless = NO;
		}
		egrepcaseless(caseless);
		break;

	    case 'r':	/* rebuild database cscope style */
	    case ctrl('R'):
		freefilelist();
		makefilelist();
		/* FALLTHROUGH */

	    case 'R':	/* rebuild database samuel style */
		rebuild();
		putchar('\n');
		break;

	    case 'C':	/* clear file names */
		freefilelist();
		putchar('\n');
		break;

	    case 'F':	/* add a file name */
		strcpy(path, buf + 1);
		if (infilelist(path) == NO &&
		    (s = inviewpath(path)) != NULL) {
		    addsrcfile(s);
		}
		putchar('\n');
		break;

	    case 'q':	/* quit */
	    case ctrl('D'):
	    case ctrl('Z'):
		myexit(0);

	    default:
		fprintf(stderr, "cscope: unknown command '%s'\n", buf);
		break;
	    }
	}
	/* NOTREACHED */
    }
    /* pause before clearing the screen if there have been error messages */
    if (errorsfound == YES) {
	errorsfound = NO;
	askforreturn();
    }
    /* do any optional search */
    if (*Pattern != '\0') {
	atfield();		/* move to the input field */
	command(ctrl('Y'));	/* search */
    } else if (reflines != NULL) {
	/* read any symbol reference lines file */
	readrefs(reflines);
    }
    display();		/* update the display */

    for (;;) {
	if (!selecting)
	    atfield();	/* move to the input field */

	/* exit if the quit command is entered */
	if ((c = mygetch()) == EOF || c == ctrl('D')) {
	    break;
	}
	if (c == ctrl('Z')) {
#ifdef SIGTSTP
	    kill(0, SIGTSTP);
	    continue;
#else
	    break;
#endif
	}
	/* execute the commmand, updating the display if necessary */
	if (command(c) == YES) {
	    display();
	}

	if (selecting) {
	    move(displine[curdispline], 0);
	    refresh();
	}
    }
    /* cleanup and exit */
    myexit(0);
    /* NOTREACHED */
    return 0;		/* avoid warning... */
}

void
cannotopen(char *file)
{
    posterr("Cannot open file %s", file);
}

/* FIXME MTE - should use postfatal here */
void
cannotwrite(char *file)
{
    char	msg[MSGLEN + 1];

    snprintf(msg, sizeof(msg), "Removed file %s because write failed", file);

    myperror(msg);	/* display the reason */

    unlink(file);
    myexit(1);	/* calls exit(2), which closes files */
}


/* set up the digraph character tables for text compression */
static void
initcompress(void)
{
    int	i;
	
    if (compress == YES) {
	for (i = 0; i < 16; ++i) {
	    dicode1[(unsigned char) (dichar1[i])] = i * 8 + 1;
	}
	for (i = 0; i < 8; ++i) {
	    dicode2[(unsigned char) (dichar2[i])] = i + 1;
	}
    }
}

/* skip the list in the cross-reference file */

static void
skiplist(FILE *oldrefs)
{
    int	i;
	
    if (fscanf(oldrefs, "%d", &i) != 1) {
	postfatal("cscope: cannot read list size from file %s\n", reffile);
	/* NOTREACHED */
    }
    while (--i >= 0) {
	if (fscanf(oldrefs, "%*s") != 0) {
	    postfatal("cscope: cannot read list name from file %s\n", reffile);
	    /* NOTREACHED */
	}
    }
}


/* enter curses mode */
void
entercurses(void)
{
    incurses = YES;
#ifndef __MSDOS__ /* HBB 20010313 */
    nonl();		    /* don't translate an output \n to \n\r */
#endif
    raw();			/* single character input */
    noecho();			/* don't echo input characters */
    clear();			/* clear the screen */
    mouseinit();		/* initialize any mouse interface */
    drawscrollbar(topline, nextline);
}


/* exit curses mode */
void
exitcurses(void)
{
	/* clear the bottom line */
	move(LINES - 1, 0);
	clrtoeol();
	refresh();

	/* exit curses and restore the terminal modes */
	endwin();
	incurses = NO;

	/* restore the mouse */
	mousecleanup();
	fflush(stdout);
}


/* normal usage message */
static void
usage(void)
{
	fprintf(stderr, "Usage: cscope [-bcCdehklLqRTuUvV] [-f file] [-F file] [-i file] [-I dir] [-s dir]\n");
	fprintf(stderr, "              [-p number] [-P path] [-[0-8] pattern] [source files]\n");
}


/* long usage message */
static void
longusage(void)
{
	usage();
	fprintf(stderr, "\
\n\
-b            Build the cross-reference only.\n\
-C            Ignore letter case when searching.\n\
-c            Use only ASCII characters in the cross-ref file (don't compress).\n\
-d            Do not update the cross-reference.\n\
-e            Suppress the <Ctrl>-e command prompt between files.\n\
-F symfile    Read symbol reference lines from symfile.\n\
-f reffile    Use reffile as cross-ref file name instead of %s.\n",
		REFFILE);
	fprintf(stderr, "\
-h            This help screen.\n\
-I incdir     Look in incdir for any #include files.\n\
-i namefile   Browse through files listed in namefile, instead of %s\n",
		NAMEFILE);
	fprintf(stderr, "\
-k            Kernel Mode - don't use %s for #include files.\n",
		DFLT_INCDIR);
	fputs("\
-L            Do a single search with line-oriented output.\n\
-l            Line-oriented interface.\n\
-num pattern  Go to input field num (counting from 0) and find pattern.\n\
-P path       Prepend path to relative file names in pre-built cross-ref file.\n\
-p n          Display the last n file path components.\n\
-q            Build an inverted index for quick symbol searching.\n\
-R            Recurse directories for files.\n\
-s dir        Look in dir for additional source  files.\n\
-T            Use only the first eight characters to match against C symbols.\n\
-U            Check file time stamps.\n\
-u            Unconditionally build the cross-reference file.\n\
-v            Be more verbose in line mode.\n\
-V            Print the version number.\n\
\n\
Please see the manpage for more information.\n",
	      stderr);
}

/* cleanup and exit */

void
myexit(int sig)
{
	/* HBB 20010313; close file before unlinking it. Unix may not care
	 * about that, but DOS absolutely needs it */
	if (refsfound != NULL)
		fclose(refsfound);
	
	/* remove any temporary files */
	if (temp1[0] != '\0') {
		unlink(temp1);
		unlink(temp2);
		rmdir(tempdirpv);		
	}
	/* restore the terminal to its original mode */
	if (incurses == YES) {
		exitcurses();
	}
	/* dump core for debugging on the quit signal */
	if (sig == SIGQUIT) {
		abort();
	}
	/* HBB 20000421: be nice: free allocated data */
	freefilelist();
	freeinclist();
	freesrclist();
	freecrossref();
	free_newbuildfiles();

	exit(sig);
}
