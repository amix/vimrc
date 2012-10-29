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

/* memory allocation functions */

#include <stdio.h>
#include <string.h>
#include "alloc.h"

#include "global.h" /* for postfatal() */

static char const rcsid[] = "$Id: alloc.c,v 1.8 2006/07/23 20:59:20 broeker Exp $";

static	void	*alloctest(void *p);

/* let autoconf find out if <stdlib.h> is available. This test will
 * succeed more reliably than the defined(__STDC__) one I replaced */
#if STDC_HEADERS
# include <stdlib.h>
#else
char	*calloc(), *malloc(), *realloc(), *strcpy();
#endif

/* allocate a string */

char *
my_strdup(char *s)
{
	return(strcpy(mymalloc(strlen(s) + 1), s));
}


/* version of malloc that only returns if successful */
void *
mymalloc(size_t size)
{
    return(alloctest(malloc((unsigned) size)));
}


/* version of calloc that only returns if successful */
void *
mycalloc(size_t nelem, size_t size)
{
    return(alloctest(calloc((unsigned) nelem, (unsigned) size)));
}


/* version of realloc that only returns if successful */
void *
myrealloc(void *p, size_t size)
{
    return(alloctest(realloc(p, (unsigned) size)));
}


/* check for memory allocation failure */
static	void *
alloctest(void *p)
{
    if (p == NULL) {
	postfatal("\n%s: out of storage\n", argv0);
	/* NOTREACHED */
    }
    return(p);
}
