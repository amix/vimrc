%{
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
 *	C symbol scanner
 */
#include "global.h"
#include "alloc.h"
#include "scanner.h"
#include "lookup.h"

#include <assert.h>

/* the line counting has been moved from character reading for speed */
/* comments are discarded */

#ifndef FLEX_SCANNER
# error Sorry, this scanner needs flex. It is not usable with AT&T Lex.
#endif

#define	IFLEVELINC	5	/* #if nesting level size increment */
#define YY_NO_TOP_STATE 1

static char const rcsid[] = "$Id: fscanner.l,v 1.17 2012/08/02 21:48:08 broeker Exp $";

int	first;	/* buffer index for first char of symbol */
int	last;	/* buffer index for last char of symbol */
int	lineno;	/* symbol line number */
int	myylineno = 1;

/* HBB 20001007: new variables, emulating yytext in a way that allows 
 * the yymore() simulation, my_yymore(), to be used even in the presence of 
 * yyless(). */
size_t my_yyleng = 0;
char *my_yytext = NULL;

static	BOOL	arraydimension;		/* inside array dimension declaration */
static	BOOL	bplisting;		/* breakpoint listing */
static	int	braces;			/* unmatched left brace count */
static	BOOL	classdef;		/* c++ class definition */
static	BOOL	elseelif;		/* #else or #elif found */
static	BOOL	esudef;			/* enum/struct/union global definition */
static	BOOL	external;		/* external definition */
static	int	externalbraces;		/* external definition outer brace count */
static	BOOL	fcndef;			/* function definition */
static	BOOL	global;			/* file global scope (outside functions) */
static	int	iflevel;		/* #if nesting level */
static	BOOL	initializer;		/* data initializer */
static	int	initializerbraces;	/* data initializer outer brace count */
static	BOOL	lex;			/* lex file */
static	int	miflevel = IFLEVELINC;	/* maximum #if nesting level */
static	int	*maxifbraces;		/* maximum brace count within #if */
static	int	*preifbraces;		/* brace count before #if */
static	int	parens;			/* unmatched left parenthesis count */
static	BOOL	ppdefine;		/* preprocessor define statement */
static	BOOL	pseudoelif;		/* pseudo-#elif */
static	BOOL	oldtype;		/* next identifier is an old type */
static	BOOL	rules;			/* lex/yacc rules */
static	BOOL	sdl;			/* sdl file */
static	BOOL	structfield;		/* structure field declaration */
static	int	tagdef;			/* class/enum/struct/union tag definition */
static	BOOL	template;		/* function template */
static	int	templateparens;		/* function template outer parentheses count */
static	int	typedefbraces = -1;	/* initial typedef brace count */
static	int	token;			/* token found */
static	int 	ident_start;		/* begin of preceding identifier */

/* If this is defined to 1, use flex rules rather than the input
 * function to discard comments. The scanner gains quite a bit of
 * speed this way, because of a large reduction of the number of I/O
 * system/library calls.  The original skipcomment_input() called
 * getc() so often that the call overhead of shared libraries
 * vs. static linking, alone, already caused a sizeable performance
 * hit (up to 40% gross gain on a cscope -cub of its own source
 * dir). */
#define COMMENTS_BY_FLEX 1

#if !COMMENTS_BY_FLEX
static	int	skipcomment_input(void);
static	int	comment(void);
static	int	insidestring_input(int);
#endif

static	void	my_yymore(void);

#if COMMENTS_BY_FLEX
# define skipcomment_input input
#else

# define YY_INPUT(buf,result,max_size)				\
{								\
	int c = skipcomment_input ();				\
	result = (c == EOF) ? YY_NULL : (buf[0] = c, 1);	\
}

#endif /* !COMMENTS_BY_FLEX*/


%}
identifier	[a-zA-Z_$][a-zA-Z_0-9$]*
number		\.?[0-9][.0-9a-fA-FlLuUxX]*
comment		"/*"([^*]*("*"+[^/])?)*"*/"|"//"[^\n]*\n
ws		[ \t\r\v\f]
wsnl		[ \t\r\v\f\n]|{comment}

/* flex options: stack of start conditions, and don't use yywrap() */
%option stack
%option noyywrap

%start SDL
%a 4000
%o 7000

/* exclusive start conditions. not available in AT&T lex -> use flex! */
%x IN_PREPROC WAS_ENDIF WAS_IDENTIFIER WAS_ESU IN_DQUOTE IN_SQUOTE COMMENT

%%

%\{		{	/* lex/yacc C declarations/definitions */
			global = YES;
			goto more;
			/* NOTREACHED */
		}
%\}		{
			global = NO;
			goto more;
			/* NOTREACHED */
		}
^%%		{	/* lex/yacc rules delimiter */
			braces = 0;
			if (rules == NO) {
				/* this %% starts the section containing the rules */
				rules = YES;

				/* Copy yytext to private buffer, to be able to add further
				 * content following it: */
				my_yymore();

				/* simulate a yylex() or yyparse() definition */
				(void) strcat(my_yytext, " /* ");
				first = strlen(my_yytext);
				if (lex == YES) {
					(void) strcat(my_yytext, "yylex");
				} else {	
					/* yacc: yyparse implicitly calls yylex */
					char *s = " yylex()";
					char *cp = s + strlen(s);
					while (--cp >= s) {
						unput(*cp);
					}
					(void) strcat(my_yytext, "yyparse");
				}
				last = strlen(my_yytext);
				(void) strcat(my_yytext, " */");
				my_yyleng = strlen(my_yytext);
				return(FCNDEF);
			} else {
				/* were in the rules section, now comes the closing one */
				rules = NO;
				global = YES;
				last = first;
				my_yymore();
				return(FCNEND);
				/* NOTREACHED */
			}
		}

<SDL>STATE[ \t]+({identifier}|\*)	{ /* sdl state, treat as function def */
			braces = 1;
			fcndef = YES;
			token = FCNDEF;
			goto findident;
			/* NOTREACHED */
		}
<SDL>ENDSTATE[ \t]	{ /* end of an sdl state, treat as end of a function */
			goto endstate;
			/* NOTREACHED */
		}

\{		{	/* count unmatched left braces for fcn def detection */
			++braces;
			
			/* mark an untagged enum/struct/union so its beginning
			   can be found */
			if (tagdef) {
				if (braces == 1) {
					esudef = YES;
				}
				token = tagdef;
				tagdef = '\0';
				last = first;
				my_yymore();
				return(token);
			}
			goto more;
			/* NOTREACHED */
		}

\#{ws}*	{ /* start a preprocessor line */
			if (rules == NO)		/* don't consider CPP for lex/yacc rules */
				BEGIN(IN_PREPROC);
			yyleng = 1;	/* get rid of the blanks, if any */
			goto more;
			/* NOTREACHED */
		}
<IN_PREPROC>endif([^a-zA-Z0-9_$\n].*)?	{	/* #endif */
			/* delay treatment of #endif depending on whether an
			 * #if comes right after it, or not */
			/* HBB 20010619: new pattern allows trailing garbage
			 * after the #endif */
			BEGIN(WAS_ENDIF);
			goto more;
			/* NOTREACHED */
		}
<WAS_ENDIF>\n{wsnl}*#{ws}*if(ndef|def)?{ws}+		{
			/* attempt to correct erroneous brace count caused by:
			 * 
			 * #if ...
			 * 	... {
			 * #endif
			 * #if ...
			 * 	... {
			 * #endif
			 */
			/* the current #if must not have an #else or #elif */
			if (elseelif == YES) {
				goto endif;
				/* NOTREACHED */
			}
			pseudoelif = YES;
			BEGIN(INITIAL);
			yyless(1);	/* rescan all but the line ending */
			yy_set_bol(1);
			goto eol;
			/* NOTREACHED */
		}
<WAS_ENDIF>\n{wsnl}*		 { 	/* an #endif with no #if right after it */
		endif:
			if (iflevel > 0) {
				/* get the maximum brace count for this #if */
				if (braces < maxifbraces[--iflevel]) {
					braces = maxifbraces[iflevel];
				}
			}
			BEGIN(INITIAL);
			yyless(1);
			yy_set_bol(1);
			goto eol;
			/* NOTREACHED */
		}

<IN_PREPROC>ifndef{ws}+	|
<IN_PREPROC>ifdef{ws}+		|
<IN_PREPROC>if{ws}+		{ /* #if directive */
			elseelif = NO;
			if (pseudoelif == YES) {
				pseudoelif = NO;
				goto elif;
				/* NOTREACHED */
			}
			/* make sure there is room for the current brace count */
			if (iflevel == miflevel) {
				miflevel += IFLEVELINC;
				maxifbraces = myrealloc(maxifbraces, miflevel * sizeof(int));
				preifbraces = myrealloc(preifbraces, miflevel * sizeof(int));
			}
			/* push the current brace count */
			preifbraces[iflevel] = braces;
			maxifbraces[iflevel++] = 0;
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}
<IN_PREPROC>else({ws}.*)?	{ /* #else --- eat up whole line */
			elseelif = YES;
			if (iflevel > 0) {
				
				/* save the maximum brace count for this #if */
				if (braces > maxifbraces[iflevel - 1]) {
					maxifbraces[iflevel - 1] = braces;
				}
				/* restore the brace count to before the #if */
				braces = preifbraces[iflevel - 1];
			}
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}
<IN_PREPROC>elif{ws}+	{ /* #elif */
			/* elseelif = YES; --- HBB I doubt this is correct */
		elif:
			if (iflevel > 0) {
				
				/* save the maximum brace count for this #if */
				if (braces > maxifbraces[iflevel - 1]) {
					maxifbraces[iflevel - 1] = braces;
				}
				/* restore the brace count to before the #if */
				braces = preifbraces[iflevel - 1];
			}
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}

<IN_PREPROC>include{ws}*\"[^"\n]+\" |
<IN_PREPROC>include{ws}*<[^>\n]+> 	{ /* #include file */
			char	*s;
			char remember = yytext[yyleng-1];
			
			my_yymore();
			s = strpbrk(my_yytext, "\"<");
			if (!s)
				return(LEXERR);
			my_yytext[my_yyleng-1] = '\0';
			incfile(s + 1, s);
			my_yytext[my_yyleng-1] = remember;
			first = s - my_yytext;
			last = my_yyleng - 1;
			if (compress == YES) {
				my_yytext[0] = '\2';	/* compress the keyword */
			}
			BEGIN(INITIAL);
			return(INCLUDE);
			/* NOTREACHED */
		}

\}		{
			/* could be the last enum member initializer */
			if (braces == initializerbraces) {
				initializerbraces = -1;
				initializer = NO;
			}
			if (--braces <= 0) {
		endstate:
				braces = 0;
				classdef = NO;
			}
			if (braces == 0 || (braces == 1 && classdef == YES)) {

				/* if the end of an enum/struct/union definition */
				if (esudef == YES) {
					esudef = NO;
				}
				/* if the end of the function */
				else if (fcndef == YES) {
					fcndef = NO;
					last = first;
					my_yymore();
					return(FCNEND);
				}
			}
			goto more;
			/* NOTREACHED */
		}

\(		{	/* count unmatched left parentheses for function templates */
			++parens;
			goto more;
			/* NOTREACHED */
		}
\)		{
			if (--parens <= 0) {
				parens = 0;
			}
			/* if the end of a function template */
			if (parens == templateparens) {
				templateparens = -1;
				template = NO;
			}
			goto more;
			/* NOTREACHED */
		}
=		{	/* if a global definition initializer */
			if (!my_yytext)
				return(LEXERR);
			if (global == YES && ppdefine == NO && my_yytext[0] != '#') {
				initializerbraces = braces;
				initializer = YES;
			}
			goto more;
			/* NOTREACHED */
		}
:		{	/* a if global structure field */
			if (!my_yytext)
				return(LEXERR);
			if (global == YES && ppdefine == NO && my_yytext[0] != '#') {
				structfield = YES;
			}
			goto more;
			/* NOTREACHED */
		}
\,		{
			if (braces == initializerbraces) {
				initializerbraces = -1;
				initializer = NO;
			}
			structfield = NO;
			goto more;
			/* NOTREACHED */
		}
;		{	/* if the enum/struct/union was not a definition */
			if (braces == 0) {
				esudef = NO;
			}
			/* if the end of a typedef */
			if (braces == typedefbraces) {
				typedefbraces = -1;
			}
			/* if the end of a external definition */
			if (braces == externalbraces) {
				externalbraces = -1;
				external = NO;
			}
			structfield = NO;
			initializer = NO;
			goto more;
			/* NOTREACHED */
		}
<IN_PREPROC>define{ws}+{identifier}	{
				
			/* preprocessor macro or constant definition */
			ppdefine = YES;
			token = DEFINE;
			if (compress == YES) {
				my_yytext[0] = '\1';	/* compress the keyword */
			}
		findident:
			/* search backwards through yytext[] to find the identifier */
			/* NOTE: this had better be left to flex, by use of
			 * yet another starting condition */
			my_yymore();
			first = my_yyleng - 1;
			while (my_yytext[first] != ' ' && my_yytext[first] != '\t') {
				--first;
			}
			++first;
			last = my_yyleng;
			BEGIN(INITIAL);
			goto definition;
			/* NOTREACHED */
		}
<IN_PREPROC>\n	{   /* unknown preprocessor line */
			BEGIN(INITIAL);
                        ++myylineno;
			goto more;
			/* NOTREACHED */
		}
<IN_PREPROC>.		 |
<IN_PREPROC>{identifier}	{   /* unknown preprocessor line */
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}

class{wsnl}+{identifier}({wsnl}|{identifier}|[():])*\{	{	/* class definition */
			classdef = YES;
			tagdef =  'c';
			yyless(5);		/* eat up 'class', and re-scan */
			yy_set_bol(0);
			goto more;
			/* NOTREACHED */
		}

("enum"|"struct"|"union")	{
			ident_start = first;
			BEGIN(WAS_ESU);
			goto more;
		}
<WAS_ESU>{
({wsnl}+{identifier}){wsnl}*\{		{ /* e/s/u definition */
			tagdef = my_yytext[ident_start];
			BEGIN(WAS_IDENTIFIER);
			goto ident;
		}
{wsnl}*\{		{ /* e/s/u definition without a tag */
			tagdef = my_yytext[ident_start];
			BEGIN(INITIAL);
			if (braces == 0) {
				esudef = YES;
			}
			last = first;
			yyless(0);  /* re-scan all this as normal text */
			tagdef = '\0';
			goto more;
		}
({wsnl}+{identifier})?{wsnl}* 	|   
.|\n										{   /* e/s/u usage */
			BEGIN(WAS_IDENTIFIER);
			goto ident;
		}
}

if{wsnl}*\(	{ 	/* ignore 'if' */
			yyless(2);
			yy_set_bol(0);
			goto more;
}	

{identifier}	{	/* identifier found: do nothing, yet. (!) */
			BEGIN(WAS_IDENTIFIER);
			ident_start = first;
			goto more;
			/* NOTREACHED */
		}

<WAS_IDENTIFIER>{       
{ws}*\(({wsnl}|{identifier}|{number}|[*&[\]=,.:])*\)([()]|{wsnl})*[:a-zA-Z_#{]	{
			/* a function definition */
			/* note: "#define a (b) {" and "#if defined(a)\n#" 
			 * are not fcn definitions! */
			/* warning: "if (...)" must not overflow yytext, 
			 * so the content of function argument definitions 
			 * is restricted, in particular parentheses are 
			 * not allowed */
			/* FIXME HBB 20001003: the above 'not allowed' may well be the
			 * reason for the parsing bug concerning function pointer usage,
			 * I suspect. --- I think my new special-case rule for 'if'
			 * could be helpful in removing that limitation */
			if ((braces == 0 && ppdefine == NO && my_yytext[0] != '#' && rules == NO) ||
			    (braces == 1 && classdef == YES)) {
				fcndef = YES;
				token = FCNDEF;
				goto fcn;
				/* NOTREACHED */
			}
			goto fcncal;
			/* NOTREACHED */
		}
{ws}*\(([*&[\]=,.]|{identifier}|{number}|{wsnl})*		{ 	/* function call */
		fcncal:	if (fcndef == YES || ppdefine == YES || rules == YES) {
				token = FCNCALL;
				goto fcn;
				/* NOTREACHED */
			}
			if (template == NO) {
				templateparens = parens;
				template = YES;
			}
			goto ident;
			/* NOTREACHED */
		}
("*"|{wsnl})+{identifier}		{	/* typedef name or modifier use */
			goto ident;
			/* NOTREACHED */
		}
.|\n	{		/* general identifer usage */
			char	*s;

			if (global == YES && ppdefine == NO && my_yytext[0] != '#' &&
			    external == NO && initializer == NO && 
			    arraydimension == NO && structfield == NO &&
			    template == NO && fcndef == NO) {
				if (esudef == YES) {	
					/* if enum/struct/union */
					token = MEMBERDEF;
				} else {
					token = GLOBALDEF;
				}
			} else {
		ident:
				token = IDENT;
			}
		fcn:
			if (YYSTATE == WAS_IDENTIFIER) {
				/* Position back to the actual identifier: */
				last = first; 
				first = ident_start;
				yyless(0);
				/* HBB 20001008: if the anti-backup-pattern above matched,
				 * and the matched context ended with a \n, then the scanner
				 * believes it's at the start of a new line. But the yyless()
				 * should feeds that \n back into the input, so that's
				 * wrong. --> force 'beginning-of-line' status off. */
				yy_set_bol(0);
				BEGIN(INITIAL);
			} else {
				my_yymore();
				last = my_yyleng;
			}
		definition:

			/* if a long line */
			if (yyleng > STMTMAX) {
				int	c;
				
				/* skip to the end of the line */
				warning("line too long");
				while ((c = skipcomment_input()) > LEXEOF) { 
					if (c == '\n') {
						unput(c);
						break;
					}
				}
			}
			/* truncate a long symbol */
			if (yyleng > PATLEN) {
				warning("symbol too long");
				my_yyleng = first + PATLEN;
				my_yytext[my_yyleng] = '\0';
			}

			/* if found word was a keyword: */
			if ((s = lookup(my_yytext + first)) != NULL) {
				first = my_yyleng;
				
				/* if the start of a typedef */
				if (s == typedeftext) {
					typedefbraces = braces;
					oldtype = YES;
				}
				/* if an enum/struct/union */
				/* (needed for "typedef struct tag name;" so
				   tag isn't marked as the typedef name) */
				else if (s == enumtext || s == structtext || s == uniontext) {
					/* do nothing */
				} else if (s == externtext) {
					/* if an external definition */
					externalbraces = braces;
					external = YES;
				} else if (templateparens == parens && template == YES) {
					/* keyword doesn't start a function 
					 * template */
					templateparens = -1;
					template = NO;
				} else {	
					/* identifier after typedef was a 
					 * keyword */
					oldtype = NO;
				}
			} else {	
				/* not a keyword --> found an identifier */
				/* last = yyleng; */
				
				/* if a class/enum/struct/union tag definition */
				/* FIXME HBB 20001001: why reject "class"? */
				if (tagdef && strnotequal(my_yytext + first, "class")) {
					token = tagdef;
					tagdef = '\0';
					if (braces == 0) {
						esudef = YES;
					}
				} else if (braces == typedefbraces && oldtype == NO &&
				           arraydimension == NO) {
					/* if a typedef name */
					token = TYPEDEF;
				} else {
					oldtype = NO;
				}
				/* my_yymore(); */
				return(token);
				/* NOTREACHED */
			}
		}
}

\[		{	/* array dimension (don't worry or about subscripts) */
			arraydimension = YES;
			goto more;
			/* NOTREACHED */
		}
\]		{
			arraydimension = NO;
			goto more;
			/* NOTREACHED */
		}
\\\n		{	/* preprocessor statement is continued on next line */
			/* save the '\\' to the output file, but not the '\n': */
			yyleng = 1;
			my_yymore();
			goto eol;
			/* NOTREACHED */
		}
\n		{	/* end of the line */
			if (ppdefine == YES) {	/* end of a #define */
				ppdefine = NO;
				yyless(yyleng - 1);
				last = first;
				my_yymore();
				return(DEFINEEND);
			}
			/* skip the first 8 columns of a breakpoint listing line */
			/* and skip the file path in the page header */
			if (bplisting == YES) {
				int	c, i;

				/* FIXME HBB 20001007: should call input() instead */
				switch (skipcomment_input()) {	/* tab and EOF just fall through */
				case ' ':	/* breakpoint number line */
				case '[':
					for (i = 1; i < 8 && skipcomment_input() > LEXEOF; ++i)
						;
					break;
				case '.':	/* header line */
				case '/':
					/* skip to the end of the line */
					while ((c = skipcomment_input()) > LEXEOF) {
						if (c == '\n') {
							unput(c);
							break;
						}
					}
					break;
				case '\n':	/* empty line */
					unput('\n');
					break;
				}
			}
		eol:
			++myylineno;
			first = 0;
			last = 0;
			if (symbols > 0) {
				/* no my_yymore(): \n doesn't need to be in my_yytext */
				return(NEWLINE);
			}
			/* line ended --> flush my_yytext */
			if (my_yytext)
				*my_yytext = '\0';
			my_yyleng = 0;
			lineno = myylineno;
		}

\'		{	/* character constant */
			if (sdl == NO) 
				BEGIN(IN_SQUOTE);
			goto more;
			/* NOTREACHED */
		}
<IN_SQUOTE>\'	{	
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}
\"		{	/* string constant */
			BEGIN(IN_DQUOTE);
			goto more;
			/* NOTREACHED */
		}
<IN_DQUOTE>\"	{	
			BEGIN(INITIAL);
			goto more;
			/* NOTREACHED */
		}
<IN_DQUOTE,IN_SQUOTE>{
\n	  	{	/* syntax error: unexpected EOL */
			BEGIN(INITIAL);
			goto eol;
			/* NOTREACHED */
		}
\\.	|
.		{
			goto more;
			/* NOTREACHED */
		}
\\\n 	{		/* line continuation inside a string! */
			myylineno++;
			goto more;
			/* NOTREACHED */
		}
}
	
^{ws}+		{		/* don't save leading white space */
		}
		
{ws}+\n 	{		/* eat whitespace at end of line */
			unput('\n');
		}

[\t\r\v\f]+	{	/* eat non-blank whitespace sequences, replace
			 * by single blank */
			unput(' ');
		}

{ws}{2,}	{   /* compress sequential whitespace here, not in putcrossref() */
			unput(' ');
 		}

"/*"					yy_push_state(COMMENT);
<COMMENT>{
[^*\n]*			|
"*"+[^*/\n]*	; /* do nothing */
[^*\n]*\n		|
"*"+[^*/\n]*\n	{
			if (ppdefine == NO) {
				goto eol;
			} else {
				++myylineno;
			}
			/* NOTREACHED */
		}
"*"+"/"		{
			/* replace the comment by a single blank */
			unput(' ');
			yy_pop_state();
		}
}		

"//".*\n?		{
			/* C++-style one-line comment */
			goto eol;
			/* NOTREACHED */
		}

{number}				|	/* number */
<SDL>STATE[ \t]+		|   /* ... and other syntax error catchers... */
.						{	/* punctuation and operators */
						more:	
							my_yymore();
							first = my_yyleng;
						}

%%

void
initscanner(char *srcfile)
{
	char	*s;
	
	if (maxifbraces == NULL) {
		maxifbraces = mymalloc(miflevel * sizeof(int));
		preifbraces = mymalloc(miflevel * sizeof(int));
	}
	first = 0;		/* buffer index for first char of symbol */
	last = 0;		/* buffer index for last char of symbol */
	lineno = 1;		/* symbol line number */
	myylineno = 1;		/* input line number */
	arraydimension = NO;	/* inside array dimension declaration */
	bplisting = NO;		/* breakpoint listing */
	braces = 0;		/* unmatched left brace count */
	classdef = NO;		/* c++ class definition */
	elseelif = NO;		/* #else or #elif found */
	esudef = NO;		/* enum/struct/union global definition */
	external = NO;		/* external definition */
	externalbraces = -1;	/* external definition outer brace count */
	fcndef = NO;		/* function definition */
	global = YES;		/* file global scope (outside functions) */
	iflevel = 0;		/* #if nesting level */
	initializer = NO;	/* data initializer */
	initializerbraces = -1;	/* data initializer outer brace count */
	lex = NO;		/* lex file */
	parens = 0;		/* unmatched left parenthesis count */
	ppdefine = NO;		/* preprocessor define statement */
	pseudoelif = NO;	/* pseudo-#elif */
	oldtype = NO;		/* next identifier is an old type */
	rules = NO;		/* lex/yacc rules */
	sdl = NO;		/* sdl file */
	structfield = NO;	/* structure field declaration */
	tagdef = '\0';		/* class/enum/struct/union tag definition */
	template = NO;		/* function template */
	templateparens = -1;	/* function template outer parentheses count */
	typedefbraces = -1;	/* initial typedef braces count */
	ident_start = 0;	/* start of previously found identifier */

	if (my_yytext)
		*my_yytext = '\0';
	my_yyleng = 0;
	
	BEGIN(INITIAL);

	/* if this is not a C file */
	if ((s = strrchr(srcfile, '.')) != NULL) {
		switch (*++s) {	/* this switch saves time on C files */
		case 'b':
			if (strcmp(s, "bp") == 0) {	/* breakpoint listing */
				bplisting = YES;
			}
			break;
		case 'l':
			if (strcmp(s, "l") == 0) {	/* lex */
				lex = YES;
				global = NO;
			}
			break;
		case 's':
			if (strcmp(s, "sd") == 0) {	/* sdl */
				sdl = YES;
				BEGIN(SDL);
			}
			break;
		case 'y':
			if (strcmp(s, "y") == 0) {	/* yacc */
				global = NO;
			}
			break;
		}
	}
}

#if !COMMENTS_BY_FLEX

/* A micro-scanner that serves as the input() function of the
 * scanner. It throws away any comments in the input, correctly
 * avoiding doing this inside string/character constants, and knows
 * about backslash sequences. Now that the main scanner doesn't use
 * yymore() any longer, this could be replaced by lex rules. Left for
 * trying later. */

/* Status variable: If this is non-NUL, it's the character that
* terminates a string we're currently in. */
static int string_terminator = '\0';

/* Helper routine: treat 'c' as a character found inside a
 * string. Check if this character might be the end of that
 * string. Backslashes have to be taken care of, for the sake of
 * "quotes like \"these\" found inside a string". */
static int
insidestring_input(int c)
{
	static BOOL was_backslash = NO;
	
	if ((c == '\\') && (was_backslash == NO)) {
		/* escape character found --> treat next char specially */
		/* FIXME HBB 20001003: need treatment of backslash in the main
		 * scanner, too. It'll get false line counts in case of "\\'",
		 * otherwise --- they can occur as part of a lex pattern */
		was_backslash = YES;
		return c;
	}

	if (((c == '\t') && (lex == YES))
	    /* Note: "\\\n" is removed even inside strings! */
	    || ((c == '\n') && (was_backslash == NO))
		|| (c == EOF)
		|| ((c == string_terminator) && (was_backslash == NO))
	   ) {
		/* Line ended, or end-of-string was found. That is a syntax
		 * error.  To recover, stop treatment as a string constant: */
		string_terminator = '\0';
	} else if (!isprint((unsigned char)c)) {
		/* mask unprintable characters */
		c = ' ';
	}
	
	was_backslash = NO;
	return c;
}

/* Helper function: skip over input until end of comment is found (or
 * we find that it wasn't really comment, in the first place): */
static int
comment(void)
{
	int	c, lastc;

	/* Coming here, we've just read in the opening '/' of a
	 * comment. */
	do {
		if ((c = getc(yyin)) == '*') {	/* C comment */
			lastc = '\0';
			while ((c = getc(yyin)) != EOF
				   /* fewer '/'s --> test them first! */
			       && (c != '/' || lastc != '*')
			      ) { 
				if (c == '\n') {
					/* keep the line number count */
					/* FIXME HBB 20001008: this is not synchronized
					 * properly with myylineno changes by the main
					 * scanner. A strong point in favour of moving
					 * this to lex-code that is, IMHO */
					++myylineno;
				}
				lastc = c;
			}
			/* return a blank for Reiser cpp token concatenation */
			/* FIXME HBB 20001008: what on earth is 'Reiser cpp'? ANSI
			 * C defines cpp to explicitly replace any comment by a
			 * blank. Pre-ANSI cpp's behaved differently, but do we
			 * really want that? If at all, it should only ever be a
			 * non-default option (like gcc's "-traditional-cpp")
			 * */
			if ((c = getc(yyin)) == '_' || isalnum(c)) {
				(void) ungetc(c, yyin);
				c = ' ';
				break;
			}
		} else if (c == '/') {		/* C++ comment */
			while ((c = getc(yyin)) != EOF && c != '\n') {
				; /* do nothing else */
			}
			break;
		} else {					/* not a comment */
			(void) ungetc(c, yyin);
			c = '/';
			break;
			/* NOTREACHED */
		}
		
	/* there may be an immediately following comment */
	} while (c == '/');
	return(c);
}

/* The core of the actual input() function to be used by (f)lex. The
 * calling scheme between this and the actual input() redefinition is
 * a bit different for lex and flex. See the #ifdef FLEX_SCANNER part
 * in the head section. */
static int
skipcomment_input(void)
{
	int	c;

	c = getc (yyin);
	if (string_terminator != '\0') {
		/* don't look for comments inside strings! */
		return insidestring_input(c);
	} else if (c == '/') {
		/* swallow everything until end of comment, if this is one */
		return comment (); 
	} else if (c == '"' || c == '\'') {
		/* a string is beginning here, so switch input method */
		string_terminator = c;
	}
	
	return c;
}

#endif /* !COMMENTS_BY_FLEX */

#define MY_YY_ALLOCSTEP 1000
static void
my_yymore(void)
{
	static size_t yytext_size = 0;
	
	/* my_yytext is an ever-growing buffer. It will not ever
	 * shrink, nor will it be freed at end of program, for now */
	while (my_yyleng + yyleng + 1 >= yytext_size) {
		my_yytext = myrealloc(my_yytext, 
				      		  yytext_size += MY_YY_ALLOCSTEP);
	}
	
	strncpy (my_yytext + my_yyleng, yytext, yyleng+1);
	my_yyleng += yyleng;
}
