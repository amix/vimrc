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

/* $Id: build.h,v 1.1 2001/07/09 14:01:18 broeker Exp $ */


#ifndef CSCOPE_BUILD_H
#define CSCOPE_BUILD_H

#include "global.h"		/* FIXME: temp. only */
#include "invlib.h"

/* types and macros of build.c to be used by other modules */

/* database output macros that update its offset */
#define	dbputc(c)	(++dboffset, (void) putc(c, newrefs))
#define	dbfputs(s)	(dboffset += strlen(s), fputs(s, newrefs))

/* declarations for globals defined in build.c */

extern	BOOL	buildonly;	/* only build the database */
extern	BOOL	unconditional;	/* unconditionally build database */
extern	BOOL	fileschanged;	/* assume some files changed */

extern	char	*reffile;	/* cross-reference file path name */
extern	char	*invname; 	/* inverted index to the database */
extern	char	*invpost;	/* inverted index postings */
extern	char	*newreffile;	/* new cross-reference file name */
extern	FILE	*newrefs;	/* new cross-reference */
extern	FILE	*postings;	/* new inverted index postings */
extern	int	symrefs;	/* cross-reference file */

extern	INVCONTROL invcontrol;	/* inverted file control structure */

/* Prototypes of external functions defined by build.c */

void	build(void);
void	free_newbuildfiles(void);
void	opendatabase(void);
void	rebuild(void);
void	setup_build_filenames(char *reffile);
void 	seek_to_trailer(FILE *f);

#endif /* CSCOPE_BUILD_H */
