/*===========================================================================
 Copyright (c) 2001, The Santa Cruz Operation 
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

/* $Id: scanner.h,v 1.5 2009/08/28 14:28:27 nhorman Exp $ */


#ifndef CSCOPE_SCANNER_H
#define CSCOPE_SCANNER_H

#include <stdio.h>

#undef	YYLMAX		
#define YYLMAX	STMTMAX + PATLEN + 1	/* scanner line buffer size */

/* cross-reference database mark characters (when new ones are added, 
 * update the cscope.out format description in cscope.1)
 */
#define CLASSDEF	'c'
#define	DEFINE		'#'
#define	DEFINEEND	')'
#define ENUMDEF		'e'
#define FCNCALL		'`'
#define FCNDEF		'$'
#define FCNEND		'}'
#define GLOBALDEF	'g'
#define	INCLUDE		'~'
#define MEMBERDEF	'm'
#define NEWFILE		'@'
#define STRUCTDEF	's'
#define TYPEDEF		't'
#define UNIONDEF	'u'

/* other scanner token types */
#define	LEXEOF	0
#define	LEXERR	1
#define	IDENT	2	
#define	NEWLINE	3	

/* scanner.l global data */
extern	int	first;		/* buffer index for first char of symbol */
extern	int	last;		/* buffer index for last char of symbol */
extern	int	lineno;		/* symbol line number */
extern	FILE	*yyin;		/* input file descriptor */
extern	FILE	*yyout;		/* output file */
extern	int	myylineno;	/* input line number */

#ifdef USING_LEX
/* HBB 20010430: if lex is used instead of flex, have to simulate the
 * private copies of yytext and yytext for the world outside scanner.l: */
/* FIXME: there should be a feature test for this! */
#if defined(__OSF1__) || defined(__sun) || defined(_AIX)
extern	char	yytext[];
#else
extern	unsigned char	yytext[];
#endif
extern	int	yyleng;
# define my_yytext yytext
# define my_yyleng yyleng
#else
extern	char	*my_yytext;	/* private copy of input line */
extern	size_t	my_yyleng;	/* ... and current length of it */
#endif

/* The master function exported by scanner.l */
int 	yylex(void);
void	initscanner(char *srcfile);

#endif /* CSCOPE_SCANNER_H ends */
