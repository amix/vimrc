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
 *	keyword look-up routine for the C symbol scanner
 */

#include "global.h"
#include "lookup.h"

static char const rcsid[] = "$Id: lookup.c,v 1.4 2006/04/21 10:45:48 broeker Exp $";

/* keyword text for fast testing of keywords in the scanner */
char	enumtext[] = "enum";
char	externtext[] = "extern";
char	structtext[] = "struct";
char	typedeftext[] = "typedef";
char	uniontext[] = "union";

/* This keyword table is also used for keyword text compression.  Keywords
 * with an index less than the numeric value of a space are replaced with the
 * control character corresponding to the index, so they cannot be moved
 * without changing the database file version and adding compatibility code
 * for old databases.
 */
struct	keystruct keyword[] = {
	{"",		'\0',	NULL},	/* dummy entry */
	{"#define",	' ',	NULL},	/* must be table entry 1 */
	{"#include",	' ',	NULL},	/* must be table entry 2 */
	{"break",	'\0',	NULL},	/* rarely in cross-reference */
	{"case",	' ',	NULL},
	{"char",	' ',	NULL},
	{"continue",	'\0',	NULL},	/* rarely in cross-reference */
	{"default",	'\0',	NULL},	/* rarely in cross-reference */
	{"double",	' ',	NULL},
	{"\t",		'\0',	NULL},	/* must be the table entry 9 */
	{"\n",		'\0',	NULL},	/* must be the table entry 10 */
	{"else",	' ',	NULL},
	{enumtext,	' ',	NULL},
	{externtext,	' ',	NULL},
	{"float",	' ',	NULL},
	{"for",		'(',	NULL},
	{"goto",	' ',	NULL},
	{"if",		'(',	NULL},
	{"int",		' ',	NULL},
	{"long",	' ',	NULL},
	{"register",	' ',	NULL},
	{"return",	'\0',	NULL},
	{"short",	' ',	NULL},
	{"sizeof",	'\0',	NULL},
	{"static",	' ',	NULL},
	{structtext,	' ',	NULL},
	{"switch",	'(',	NULL},
	{typedeftext,	' ',	NULL},
	{uniontext,	' ',	NULL},
	{"unsigned",	' ',	NULL},
	{"void",	' ',	NULL},
	{"while",	'(',	NULL},
	
	/* these keywords are not compressed */
	{"do",		'\0',	NULL},
	{"auto",	' ',	NULL},
	{"fortran",	' ',	NULL},
	{"const",	' ',	NULL},
	{"signed",	' ',	NULL},
	{"volatile",	' ',	NULL},
};
#define KEYWORDS	(sizeof(keyword) / sizeof(struct keystruct))

#define HASHMOD	(KEYWORDS * 2 + 1)

static	struct	keystruct *hashtab[HASHMOD]; /* pointer table */

/* put the keywords into the symbol table */

void
initsymtab(void)
{
    unsigned int i, j;
    struct keystruct *p;
	
    for (i = 1; i < KEYWORDS; ++i) {
	p = keyword + i;
	j = hash(p->text) % HASHMOD;
	p->next = hashtab[j];
	hashtab[j] = p;
    }
}

/* see if this identifier is a keyword */

char *
lookup(char *ident)
{
	struct	keystruct *p;
	int	c;
	
	/* look up the identifier in the keyword table */
	for (p = hashtab[hash(ident) % HASHMOD]; p != NULL; p = p->next) {
		if (strequal(ident, p->text)) {
			if (compress == YES && (c = p - keyword) < ' ') {
				ident[0] = c;	/* compress the keyword */
			}
			return(p->text);
		}
	}
	/* this is an identifier */
	return(NULL);
}

/* form hash value for string */
int
hash(char *ss)
{
	int	i;
	unsigned char 	*s = (unsigned char *)ss;
	
	for (i = 0; *s != '\0'; )
		i += *s++;	/* += is faster than <<= for cscope */
	return(i);
}
