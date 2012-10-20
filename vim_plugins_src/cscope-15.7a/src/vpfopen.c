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

/* vpfopen - view path version of the fopen library function */

#include <stdio.h>
#include <string.h>
#include "vp.h"
#include "global.h"

static char const rcsid[] = "$Id: vpfopen.c,v 1.4 2009/04/10 13:39:23 broeker Exp $";

FILE *
vpfopen(char *filename, char *type)
{
	char	buf[MAXPATH + 1];
	FILE	*returncode;
	int	i;

	if ((returncode = myfopen(filename, type)) == NULL 
		&& filename[0] != '/' 
		/* && strcmp(type, "r") == 0 */ /* HBB: this breaks if type=="rb" */
		&& type[0] == 'r'
		) {
		vpinit(NULL);
		for (i = 1; i < vpndirs; i++) {
			(void) snprintf(buf, sizeof(buf), "%s/%s", vpdirs[i], filename);
			if ((returncode = myfopen(buf, type)) != NULL) {
				break;
			}

		}
	}
	return(returncode);
}
