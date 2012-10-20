/*
*   $Id: qdos.c 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 1999, Thierry Godefroy <godefroy@imaginet.fr>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions to handle wildcard expansion and file name
*   conversion under QDOS.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <qdos.h>
#include <string.h>
#include <errno.h>
#include "ctags.h"

/* Translate the filenames from UNIX to QDOS conventions on open calls */
int (*_Open) (const char *, int, ...) = qopen;

long _stack          = 24576;  /* Plenty of stack space */
long _memincr        = 10240;  /* Big increments to cut fragmentation */
char _prog_name []   = "ctags";
char _version []     = PROGRAM_VERSION;
char _copyright [32] = __DATE__;
char *_endmsg        = "\nPress a key to exit.";
int  custom_expand (char * param, char ***argvptr, int *argcptr);
int  (*_cmdwildcard) ()  = custom_expand;


struct WINDOWDEF _condetails = { 208, 1, 0, 7, 512, 256, 0, 0};
void (*_consetup) ()         = consetup_title;

/* custom cmdexpand: also expands directory names */

#define FILEBUF_INIT    1024  /* Initial allocation size for buffer */
#define FILEBUF_INCR    1024  /* Increment size for buffer */

int custom_expand (char * param, char ***argvptr, int *argcptr)
{
	int     count,sl;
	size_t  bufsize;
	char    *filenamebuf;
	char    *ptr,*safeptr;

	/*
	 *  Check to see if we should do wild card expansion.
	 *  We only perform wildcard expansion if the parameter
	 *  was not a string and if it contains one of the
	 *  wild card characters.
	 *
	 *  We also do not expand any option that starts with '-'
	 *  as we then assume that it is a unix stylew option.
	 */
	if ((*param == '-') ||  (strpbrk (param,"*?") == NULL) ) {
	    return 0;
	}

	if ((filenamebuf = malloc (bufsize = FILEBUF_INIT)) == NULL) {
	    return -1;
	}
TRYAGAIN:
	count = getfnl (param, filenamebuf, bufsize, QDR_ALL);
	if (count == -1  && errno == ENOMEM) {
	    /*
	     *  We have overflowed the buffer, so we try
	     *  to get a bigger buffer and try again.
	     */
	    bufsize += FILEBUF_INCR;
	    if ((filenamebuf = realloc (filenamebuf, bufsize)) == NULL) {
	        return -1;
	    } else {
	        goto TRYAGAIN;
	    }
	}
	/*
	 *  If no files were found, then return unexpanded.
	 */
	if (count == 0) {
	    free (filenamebuf);
	    return 0;
	}
	/*
	 *  Files were found, so add these to the list instead
	 *  of the original parameter typed by the user.
	 */
	for ( ptr=filenamebuf ; count > 0 ; count -- ) {
		*argvptr = (char **) realloc (*argvptr, (size_t) (((*argcptr) + 2) * sizeof (char *)));
		safeptr= (char *) malloc ((size_t) (sl=strlen (ptr) + 1));
		if (safeptr == NULL || *argvptr == NULL) {
			return -1;
		}
		(void) memcpy (safeptr,ptr, (size_t) sl);
		(*argvptr) [*argcptr] = safeptr;
		*argcptr += 1;
		ptr += sl;
	}
	free (filenamebuf);
	return *argcptr;
}

/* vi:set tabstop=4 shiftwidth=4: */
