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
 *	build cross-reference file
 */

#include "global.h"

#include "build.h"
#include "scanner.h"
#include "alloc.h"

#include <stdlib.h>
#include <sys/stat.h>

static char const rcsid[] = "$Id: crossref.c,v 1.15 2009/08/28 14:28:27 nhorman Exp $";


/* convert long to a string */
#define	ltobase(value)	n = value; \
			s = buf + (sizeof(buf) - 1); \
			*s = '\0'; \
			digits = 1; \
			while (n >= BASE) { \
				++digits; \
				i = n; \
				n /= BASE; \
				*--s = i - n * BASE + '!'; \
			} \
			*--s = n + '!';

#define	SYMBOLINC	20	/* symbol list size increment */

long	dboffset;		/* new database offset */
BOOL	errorsfound;		/* prompt before clearing messages */
long	lineoffset;		/* source line database offset */
long	npostings;		/* number of postings */
int	nsrcoffset;             /* number of file name database offsets */
long	*srcoffset;             /* source file name database offsets */
unsigned long symbols;		/* number of symbols */

static	char	*filename;	/* file name for warning messages */
static	long	fcnoffset;	/* function name database offset */
static	long	macrooffset;	/* macro name database offset */
static	unsigned long msymbols = SYMBOLINC; /* maximum number of symbols */

struct	symbol {	/* symbol data */
    int	type;		/* type */
    unsigned int first;		/* index of first character in text */
    unsigned int last;		/* index of last+1 character in text */
    unsigned int length;	/* symbol length */
    unsigned int fcn_level;	/* function level of the symbol */
};
static struct symbol *symbol;

static	void	putcrossref(void);
static	void	savesymbol(int token, int num);

void
crossref(char *srcfile)
{
    unsigned int i;
    unsigned int length;	/* symbol length */
    unsigned int entry_no;	/* function level of the symbol */
    int token;                  /* current token */
    struct stat st;

    if (! ((stat(srcfile, &st) == 0)
	   && S_ISREG(st.st_mode))) {
	cannotopen(srcfile);
	errorsfound = YES;
	return;
    }
	
    entry_no = 0;
    /* open the source file */
    if ((yyin = myfopen(srcfile, "r")) == NULL) {
	cannotopen(srcfile);
	errorsfound = YES;
	return;
    }
    filename = srcfile;	/* save the file name for warning messages */
    putfilename(srcfile);	/* output the file name */
    dbputc('\n');
    dbputc('\n');

    /* read the source file */
    initscanner(srcfile);
    fcnoffset = macrooffset = 0;
    symbols = 0;
    if (symbol == NULL) {
	symbol = mymalloc(msymbols * sizeof(struct symbol));
    }
    for (;;) {
		
	/* get the next token */
	switch (token = yylex()) {
	default:
	    /* if requested, truncate C symbols */
	    length = last - first;
	    if (trun_syms == YES && length > 8 &&
		token != INCLUDE && token != NEWFILE) {
		length = 8;
		last = first + 8;
	    }
	    /* see if the token has a symbol */
	    if (length == 0) {
		savesymbol(token, entry_no);
		break;
	    }
	    /* update entry_no if see function entry */
	    if (token == FCNDEF) {
		entry_no++;
	    }
	    /* see if the symbol is already in the list */
	    for (i = 0; i < symbols; ++i) {
		if (length == symbol[i].length
		    && strncmp(my_yytext + first,
			       my_yytext + symbol[i].first,
			       length) == 0 
		    && entry_no == symbol[i].fcn_level
		    && token == symbol[i].type
		    ) {	/* could be a::a() */
		    break;
		}
	    }
	    if (i == symbols) {	/* if not already in list */
		savesymbol(token, entry_no);
	    }
	    break;

	case NEWLINE:	/* end of line containing symbols */
	    entry_no = 0;	/* reset entry_no for each line */
#ifdef USING_LEX
	    --yyleng; 	/* remove the newline */
#endif
	    putcrossref();	/* output the symbols and source line */
	    lineno = myylineno;	/* save the symbol line number */
#ifndef USING_LEX
	    /* HBB 20010425: replaced yyleng-- by this chunk: */
	    if (my_yytext)
		*my_yytext = '\0';
	    my_yyleng = 0;
#endif
	    break;
			
	case LEXERR:	/* Lexer error, abort further parsing of this file */
	case LEXEOF:	/* end of file; last line may not have \n */
			
			/* if there were symbols, output them and the source line */
	    if (symbols > 0) {
		putcrossref();
	    }
	    (void) fclose(yyin);	/* close the source file */

	    /* output the leading tab expected by the next call */
	    dbputc('\t');
	    return;
	}
    }
}

/* save the symbol in the list */

static void
savesymbol(int token, int num)
{
    /* make sure there is room for the symbol */
    if (symbols == msymbols) {
	msymbols += SYMBOLINC;
	symbol = myrealloc(symbol, msymbols * sizeof(struct symbol));
    }
    /* save the symbol */
    symbol[symbols].type = token;
    symbol[symbols].first = first;
    symbol[symbols].last = last;
    symbol[symbols].length = last - first;
    symbol[symbols].fcn_level = num;
    ++symbols;
}

/* output the file name */

void
putfilename(char *srcfile)
{
	/* check for file system out of space */
	/* note: dbputc is not used to avoid lint complaint */
	if (putc(NEWFILE, newrefs) == EOF) {
		cannotwrite(newreffile);
		/* NOTREACHED */
	}
	++dboffset;
	if (invertedindex == YES) {
		srcoffset[nsrcoffset++] = dboffset;
	}
	dbfputs(srcfile);
	fcnoffset = macrooffset = 0;
}

/* output the symbols and source line */

static void
putcrossref(void)
{
    unsigned int i, j;
    unsigned char c;
    BOOL    blank;          /* blank indicator */
    unsigned int symput = 0;     /* symbols output */
    int     type;

    /* output the source line */
    lineoffset = dboffset;
    dboffset += fprintf(newrefs, "%d ", lineno);
#ifdef PRINTF_RETVAL_BROKEN
    dboffset = ftell(newrefs); /* fprintf doesn't return chars written */
#endif

    /* HBB 20010425: added this line: */
    my_yytext[my_yyleng] = '\0';

    blank = NO;
    for (i = 0; i < my_yyleng; ++i) {
		
	/* change a tab to a blank and compress blanks */
	if ((c = my_yytext[i]) == ' ' || c == '\t') {
	    blank = YES;
	} else if (symput < symbols && i == symbol[symput].first) {
	    /* look for the start of a symbol */

	    /* check for compressed blanks */
	    if (blank == YES) {
		blank = NO;
		dbputc(' ');
	    }
	    dbputc('\n');	/* symbols start on a new line */
			
	    /* output any symbol type */
	    if ((type = symbol[symput].type) != IDENT) {
		dbputc('\t');
		dbputc(type);
	    } else {
		type = ' ';
	    }
	    /* output the symbol */
	    j = symbol[symput].last;
	    c = my_yytext[j];
	    my_yytext[j] = '\0';
	    if (invertedindex == YES) {
		putposting(my_yytext + i, type);
	    }
	    writestring(my_yytext + i);
	    dbputc('\n');
	    my_yytext[j] = c;
	    i = j - 1;
	    ++symput;
	} else {
	    /* HBB: try to save some time by early-out handling of 
	     * non-compressed mode */
	    if (compress == NO) {
		if (blank == YES) {
		    dbputc(' ');
		    blank = NO;
		}
		j = i + strcspn(my_yytext+i, "\t ");
		if (symput < symbols
		    && j >= symbol[symput].first)
		    j = symbol[symput].first;
		c = my_yytext[j];
		my_yytext[j] = '\0';
		writestring(my_yytext + i);
		my_yytext[j] = c;
		i = j - 1;
		/* finished this 'i', continue with the blank */
		continue;
	    }

	    /* check for compressed blanks */
	    if (blank == YES) {
		if (dicode2[c]) {
		    c = DICODE_COMPRESS(' ', c);
		} else {
		    dbputc(' ');
		}
	    } else if (IS_A_DICODE(c, my_yytext[i + 1])
		       && symput < symbols
		       && i + 1 != symbol[symput].first) {
		/* compress digraphs */
		c = DICODE_COMPRESS(c, my_yytext[i + 1]);
		++i;
	    }
	    dbputc((int) c);
	    blank = NO;
			
	    /* skip compressed characters */
	    if (c < ' ') {
		++i;
				
		/* skip blanks before a preprocesor keyword */
		/* note: don't use isspace() because \f and \v
		   are used for keywords */
		while ((j = my_yytext[i]) == ' ' || j == '\t') {
		    ++i;
		}
		/* skip the rest of the keyword */
		while (isalpha((unsigned char)my_yytext[i])) {
		    ++i;
		}
		/* skip space after certain keywords */
		if (keyword[c].delim != '\0') {
		    while ((j = my_yytext[i]) == ' ' || j == '\t') {
			++i;
		    }
		}
		/* skip a '(' after certain keywords */
		if (keyword[c].delim == '('
		    && my_yytext[i] == '(') {
		    ++i;
		}
		--i;	/* compensate for ++i in for() */
	    } /* if compressed char */
	} /* else: not a symbol */
    } /* for(i) */

    /* ignore trailing blanks */
    dbputc('\n');
    dbputc('\n');

    /* output any #define end marker */
    /* note: must not be part of #define so putsource() doesn't discard it
       so findcalledbysub() can find it and return */
    if (symput < symbols && symbol[symput].type == DEFINEEND) {
	dbputc('\t');
	dbputc(DEFINEEND);
	dbputc('\n');
	dbputc('\n');	/* mark beginning of next source line */
	macrooffset = 0;
    }
    symbols = 0;
}

/* HBB 20000421: new function, for avoiding memory leaks */
/* free the cross reference symbol table */
void
freecrossref()
{
	if (symbol)
		free(symbol);
	symbol = NULL;
	symbols = 0;
}

/* output the inverted index posting */

void
putposting(char *term, int type)
{
	long	i, n;
	char	*s;
	int	digits;		/* digits output */
	long	offset;		/* function/macro database offset */
	char	buf[11];		/* number buffer */

	/* get the function or macro name offset */
	offset = fcnoffset;
	if (macrooffset != 0) {
		offset = macrooffset;
	}
	/* then update them to avoid negative relative name offset */
	switch (type) {
	case DEFINE:
		macrooffset = dboffset;
		break;
	case DEFINEEND:
		macrooffset = 0;
		return;		/* null term */
	case FCNDEF:
		fcnoffset = dboffset;
		break;
	case FCNEND:
		fcnoffset = 0;
		return;		/* null term */
	}
	/* ignore a null term caused by a enum/struct/union without a tag */
	if (*term == '\0') {
		return;
	}
	/* skip any #include secondary type char (< or ") */
	if (type == INCLUDE) {
		++term;
	}
	/* output the posting, which should be as small as possible to reduce
	   the temp file size and sort time */
	(void) fputs(term, postings);
	(void) putc(' ', postings);

	/* the line offset is padded so postings for the same term will sort
	   in ascending line offset order to order the references as they
	   appear withing a source file */
	ltobase(lineoffset);
	for (i = PRECISION - digits; i > 0; --i) {
		(void) putc('!', postings);
	}
	do {
		(void) putc(*s, postings);
	} while (*++s != '\0');
	
	/* postings are also sorted by type */
	(void) putc(type, postings);
	
	/* function or macro name offset */
	if (offset > 0) {
		(void) putc(' ', postings);
		ltobase(offset);
		do {
			(void) putc(*s, postings);
		} while (*++s != '\0');
	}
	if (putc('\n', postings) == EOF) {
		cannotwrite(temp1);
		/* NOTREACHED */
	}
	++npostings;
}

/* put the string into the new database */

void
writestring(char *s)
{
	unsigned char c;
	int	i;
	
	if (compress == NO) {
		/* Save some I/O overhead by using puts() instead of putc(): */
		dbfputs(s);
		return;
	} 
	/* compress digraphs */
	for (i = 0; (c = s[i]) != '\0'; ++i) {
		if (/* dicode1[c] && dicode2[(unsigned char) s[i + 1]] */
		    IS_A_DICODE(c, s[i + 1])) {
			/* c = (0200 - 2) + dicode1[c] + dicode2[(unsigned char) s[i + 1]]; */
			c = DICODE_COMPRESS(c, s[i + 1]);
			++i;
		}
		dbputc(c);	
	}
}

/* print a warning message with the file name and line number */

void
warning(char *text)
{
	
	(void) fprintf(stderr, "cscope: \"%s\", line %d: warning: %s\n", filename, 
		myylineno, text);
	errorsfound = YES;
}
