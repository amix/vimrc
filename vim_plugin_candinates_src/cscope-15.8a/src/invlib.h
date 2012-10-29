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

/* $Id: invlib.h,v 1.7 2004/02/12 18:14:05 broeker Exp $ */


#ifndef CSCOPE_INVLIB_H
#define CSCOPE_INVLIB_H

#include <stdio.h>		/* need definition of FILE* */
#include <limits.h>

/* inverted index definitions */

/* postings temporary file long number coding into characters */
/* FIXME HBB: where would these definitions come from ? */
#if CHAR_MAX==255
# define	BASE		223	/* 255 - ' ' */
# define	PRECISION	4	/* maximum digits after converting a long */
#else
# if CHAR_MAX==127	/* assume sign-extension of a char when converted to an int */
#  define	BASE		95	/* 127 - ' ' */
#  define	PRECISION	5	/* maximum digits after converting a long */
# else
  #error Need a platform with 8 bits in a char value
# endif
#endif

/* inverted index access parameters */
#define INVAVAIL	0
#define INVBUSY		1
#define INVALONE	2

/* boolean set operations */
#define	BOOL_OR		3
#define	AND		4
#define	NOT		5
#define	REVERSENOT	6

/* note that the entire first block is for parameters */
typedef	struct	{
	long	version;	/* inverted index format version */
	long	filestat;	/* file status word  */
	long	sizeblk;	/* size of logical block in bytes */
	long	startbyte;	/* first byte of superfinger */
	long	supsize;	/* size of superfinger in bytes */
	long	cntlsize;	/* size of max cntl space (should be a multiple of BUFSIZ) */
	long	share;		/* flag whether to use shared memory */
} PARAM;

typedef	struct {
	FILE	*invfile;	/* the inverted file ptr */
	FILE	*postfile;	/* posting file ptr */
	PARAM	param;		/* control parameters for the file */
	char	*iindex;	/* ptr to space for superindex */
	union logicalblk *logblk;	/* ptr to space for a logical block */
	long	numblk;		/* number of block presently at *logblk */
	long	keypnt;		/* number item in present block found */
} INVCONTROL;

typedef        struct  {
       short   offset;         /* offset in this logical block */
       unsigned char size;     /* size of term */
       unsigned char space;    /* number of longs of growth space */
       long    post;           /* number of postings for this entry */
} ENTRY;

typedef	struct {
	long	lineoffset;	/* source line database offset */
	long	fcnoffset;	/* function name database offset */
	long	fileindex : 24;	/* source file name index */
	long	type : 8;	/* reference type (mark character) */
} POSTING;

extern	long	*srcoffset;	/* source file name database offsets */
extern	int	nsrcoffset;	/* number of file name database offsets */


void	boolclear(void);
POSTING	*boolfile(INVCONTROL *invcntl, long *num, int boolarg);
void	invclose(INVCONTROL *invcntl);
void	invdump(INVCONTROL *invcntl, char *term);
long	invfind(INVCONTROL *invcntl, char *searchterm);
int	invforward(INVCONTROL *invcntl);
int	invopen(INVCONTROL *invcntl, char *invname, char *invpost, int status);
long	invmake(char *invname, char *invpost, FILE *infile);
long	invterm(INVCONTROL *invcntl, char *term);

#endif /* CSCOPE_INVLIB_H */
