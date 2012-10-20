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


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#if SHARE
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#define ERR  -1
#endif
#include "invlib.h"
#include "global.h"

#include <assert.h>

#define	DEBUG		0	/* debugging code and realloc messages */
#define BLOCKSIZE	2 * BUFSIZ	/* logical block size */
#define	POSTINC		10000	/* posting buffer size increment */
#define SEP		' '	/* sorted posting field separator */
#define	SETINC		100	/* posting set size increment */
#define	STATS		0	/* print statistics */
#define	SUPERINC	10000	/* super index size increment */
#define	TERMMAX		512	/* term max size */
#define	FMTVERSION	1	/* inverted index format version */
#define	ZIPFSIZE	200	/* zipf curve size */

static char const rcsid[] = "$Id: invlib.c,v 1.18 2006/09/30 15:38:16 broeker Exp $";

#if DEBUG
/* FIXME HBB 20010705: nowhere in the source is `invbreak' ever set to
 * a value other than the (silent) initialization to zero. Pretty
 * useless, that looks */
int	invbreak;
#endif

static	int	boolready(void);
static	int	invnewterm(void);
static	void	invstep(INVCONTROL *invcntl);
static	void	invcannotalloc(unsigned n);
static	void	invcannotopen(char *file);
static	void	invcannotwrite(char *file);

#if STATS
int	showzipf;	/* show postings per term distribution */
#endif

static	POSTING	*item, *enditem, *item1 = NULL, *item2 = NULL;
static	unsigned setsize1, setsize2;
static	long	numitems, totterm, zerolong;
static	char	*indexfile, *postingfile;
static	FILE	*outfile, *fpost;
static	unsigned supersize = SUPERINC, supintsize;
static  unsigned int numpost, numlogblk, amtused, nextpost;
static  unsigned int lastinblk, numinvitems;
static	POSTING	*POST, *postptr;
static	unsigned long	*SUPINT, *supint, nextsupfing;
static	char	*SUPFING, *supfing;
static	char	thisterm[TERMMAX];
typedef union logicalblk {
	long	invblk[BLOCKSIZE / sizeof(long)];
	char	chrblk[BLOCKSIZE];
} t_logicalblk;
static t_logicalblk logicalblk;

#if DEBUG || STATS
static	long	totpost;
#endif

#if STATS
static	int	zipf[ZIPFSIZE + 1];
#endif

long
invmake(char *invname, char *invpost, FILE *infile)
{
	unsigned char	*s;
	long	num;
	int	i;
	long	fileindex = 0;	/* initialze, to avoid warning */
	unsigned postsize = POSTINC * sizeof(POSTING);
	unsigned long	*intptr;
	char	line[TERMMAX];
	long	tlong;
	PARAM	param;
	POSTING	posting;
	char 	temp[BLOCKSIZE];
#if STATS
	int	j;
	unsigned maxtermlen = 0;
#endif
	/* output file */
	if ((outfile = vpfopen(invname, "w+b")) == NULL) {
		invcannotopen(invname);
		return(0);
	}
	indexfile = invname;
	fseek(outfile, BUFSIZ, SEEK_SET);

	/* posting file  */
	if ((fpost = vpfopen(invpost, "wb")) == NULL) {
		invcannotopen(invpost);
		return(0);
	}
	postingfile = invpost;
	nextpost = 0;
	/* get space for the postings list */
	if ((POST = malloc(postsize)) == NULL) {
		invcannotalloc(postsize);
		return(0);
	}
	postptr = POST;
	/* get space for the superfinger (superindex) */
	if ((SUPFING = malloc(supersize)) == NULL) {
		invcannotalloc(supersize);
		return(0);
	}
	supfing = SUPFING;
	/* FIXME HBB: magic number alert (40) */
	supintsize = supersize / 40;
	/* also for the superfinger index */
	if ((SUPINT = malloc(supintsize * sizeof(long))) == NULL) {
		invcannotalloc(supintsize * sizeof(long));
		return(0);
	}
	supint = SUPINT;
	supint++; /* leave first term open for a count */
	/* initialize using an empty term */
	strcpy(thisterm, "");
	*supint++ = 0;
	*supfing++ = ' ';
	*supfing++ = '\0';
	nextsupfing = 2;
#if DEBUG || STATS
	totpost = 0L;
#endif
	totterm = 0L;
	numpost = 1;

	/* set up as though a block had come and gone, i.e., set up for new block  */
	/* FIXME HBB: magic number alert (16) */
	amtused = 16; /* leave no space - init 3 words + one for luck */
	numinvitems = 0;
	numlogblk = 0;
	lastinblk = sizeof(t_logicalblk);

	/* now loop as long as more to read (till eof)  */
	while (fgets(line, TERMMAX, infile) != NULL) {
#if DEBUG || STATS
		++totpost;
#endif
		s = strchr(line, SEP);
		if (s != NULL) {
			*s = '\0';
		}
		else {
			continue;
		}
#if STATS
		if ((i = strlen(line)) > maxtermlen) {
			maxtermlen = i;
		}
#endif
#if DEBUG
		printf("%ld: %s ", totpost, line);
		fflush(stdout);
#endif
		if (strcmp(thisterm, line) == 0) {
			if (postptr + 10 > POST + postsize / sizeof(POSTING)) {
				i = postptr - POST;
				postsize += POSTINC * sizeof(POSTING);
				if ((POST = realloc(POST, postsize)) == NULL) {
					invcannotalloc(postsize);
					return(0);
				}
				postptr = i + POST;
#if DEBUG
				printf("reallocated post space to %u, totpost=%ld\n",
				       postsize, totpost);
#endif
			}
			numpost++;
		} else {
			/* have a new term */
			if (!invnewterm()) {
				return(0);
			}
			strcpy(thisterm, line);
			numpost = 1;
			postptr = POST;
			fileindex = 0;
		}
		/* get the new posting */
		num = *++s - '!';
		i = 1;
		do {
			num = BASE * num + *++s - '!';
		} while (++i < PRECISION);
		posting.lineoffset = num;
		while (++fileindex < nsrcoffset && num > srcoffset[fileindex]) {
			;
		}
		posting.fileindex = --fileindex;
		posting.type = *++s;
		num = *++s - '!';
		if (*s != '\n') {
			num = *++s - '!';
			while (*++s != '\n') {
				num = BASE * num + *s - '!';
			}
			posting.fcnoffset = num;
		}
		else {
			posting.fcnoffset = 0;
		}
		*postptr++ = posting;
#if DEBUG
		printf("%ld %ld %ld %ld\n", posting.fileindex,
		       posting.fcnoffset, posting.lineoffset, posting.type);
		fflush(stdout);
#endif
	}
	if (!invnewterm()) {
		return(0);
	}
	/* now clean up final block  */
	logicalblk.invblk[0] = numinvitems;
	/* loops pointer around to start */
	logicalblk.invblk[1] = 0;
	logicalblk.invblk[2] = numlogblk - 1;
	if (fwrite(&logicalblk, sizeof(t_logicalblk), 1, outfile) == 0) {
		goto cannotwrite;
	}
	numlogblk++;
	/* write out block to save space. what in it doesn't matter */
	if (fwrite(&logicalblk, sizeof(t_logicalblk), 1, outfile) == 0) {
		goto cannotwrite;
	}
	/* finish up the super finger */
	*SUPINT = numlogblk;
	/* add to the offsets the size of the offset pointers */
	intptr = (SUPINT + 1);
	i = (char *)supint - (char *)SUPINT;
	while (intptr < supint)
		*intptr++ += i;
	/* write out the offsets (1 for the N at start) and the super finger */
	if (fwrite(SUPINT, sizeof(*SUPINT), numlogblk + 1, outfile) == 0 ||
	    fwrite(SUPFING, 1, supfing - SUPFING, outfile) == 0) {
		goto cannotwrite;
	}
	/* save the size for reference later */
	nextsupfing = sizeof(long) + sizeof(long) * numlogblk + (supfing - SUPFING);
	/* make sure the file ends at a logical block boundary.  This is 
	necessary for invinsert to correctly create extended blocks 
	 */
	i = nextsupfing % sizeof(t_logicalblk);
	/* write out junk to fill log blk */
	if (fwrite(temp, sizeof(t_logicalblk) - i, 1, outfile) == 0 ||
	    fflush(outfile) == EOF) {	/* rewind doesn't check for write failure */
		goto cannotwrite;
	}
	/* write the control area */
	rewind(outfile);
	param.version = FMTVERSION;
	param.filestat = 0;
	param.sizeblk = sizeof(t_logicalblk);
	param.startbyte = (numlogblk + 1) * sizeof(t_logicalblk) + BUFSIZ;;
	param.supsize = nextsupfing;
	param.cntlsize = BUFSIZ;
	param.share = 0;
	if (fwrite(&param, sizeof(param), 1, outfile) == 0) {
		goto cannotwrite;
	}
	for (i = 0; i < 10; i++)	/* for future use */
		if (fwrite(&zerolong, sizeof(zerolong), 1, outfile) == 0) {
			goto cannotwrite;
		}

	/* make first block loop backwards to last block */
	if (fflush(outfile) == EOF) {	/* fseek doesn't check for write failure */
		goto cannotwrite;
	}
	/* get to second word first block */
	fseek(outfile, BUFSIZ + 2 * sizeof(long), SEEK_SET);
	tlong = numlogblk - 1;
	if (fwrite(&tlong, sizeof(tlong), 1, outfile) == 0 ||
	    fclose(outfile) == EOF) {
	cannotwrite:
		invcannotwrite(invname);
		return(0);
	}
	if (fclose(fpost) == EOF) {
		invcannotwrite(postingfile);
		return(0);
	}
	--totterm;	/* don't count null term */
#if STATS
	printf("logical blocks = %d, postings = %ld, terms = %ld, max term length = %d\n",
	    numlogblk, totpost, totterm, maxtermlen);
	if (showzipf) {
		printf("\n*************   ZIPF curve  ****************\n");
		for (j = ZIPFSIZE; j > 1; j--)
			if (zipf[j]) 
				break;
		for (i = 1; i < j; ++i) {
			printf("%3d -%6d ", i, zipf[i]);
			if (i % 6 == 0) putchar('\n');
		}
		printf(">%d-%6d\n", ZIPFSIZE, zipf[0]);
	}
#endif
	/* free all malloc'd memory */
	free(POST);
	free(SUPFING);
	free(SUPINT);
	return(totterm);
}

/* add a term to the data base */

static int
invnewterm(void)
{
    int	backupflag, i, j, holditems, gooditems, howfar;
    unsigned int maxback, len, numwilluse, wdlen;
    char	*tptr, *tptr2, *tptr3;

    union {
	unsigned long	packword[2];
	ENTRY		e;
    } iteminfo;

    gooditems = 0;		/* initialize, to avoid warning */
    totterm++;
#if STATS
    /* keep zipfian info on the distribution */
    if (numpost <= ZIPFSIZE)
	zipf[numpost]++;
    else
	zipf[0]++;
#endif
    len = strlen(thisterm);
    wdlen = (len + (sizeof(long) - 1)) / sizeof(long);
    /* HBB FIXME 20060419: magic number: 3 */
    numwilluse = (wdlen + 3) * sizeof(long);
    /* new block if at least 1 item in block */
    if (numinvitems && numwilluse + amtused > sizeof(t_logicalblk)) {
	/* set up new block */
	if (supfing + 500 > SUPFING + supersize) {
	    i = supfing - SUPFING;
	    supersize += 20000;
	    if ((SUPFING = (char *)realloc(SUPFING, supersize)) == NULL) {
		invcannotalloc(supersize);
		return(0);
	    }
	    supfing = i + SUPFING;
#if DEBUG
	    printf("reallocated superfinger space to %d, totpost=%ld\n", 
		   supersize, totpost);
#endif
	}
	/* check that room for the offset as well */
	/* FIXME HBB: magic number alert (10) */
	if ((numlogblk + 10) > supintsize) {
	    i = supint - SUPINT;
	    supintsize += SUPERINC;
	    if ((SUPINT = realloc(SUPINT, supintsize * sizeof(long))) == NULL) {
		invcannotalloc(supintsize * sizeof(long));
		return(0);
	    }
	    supint = i + SUPINT;
#if DEBUG
	    printf("reallocated superfinger offset to %d, totpost = %ld\n",
		   supintsize * sizeof(long), totpost);
#endif
	}
	/* See if backup is efficatious  */
	backupflag = 0;
	maxback = (int) strlen(thisterm) / 10;
	holditems = numinvitems;
	if (maxback > numinvitems)
	    maxback = numinvitems - 2;
	howfar = 0;
	while (maxback-- > 1) {
	    howfar++;
	    iteminfo.packword[0] =
		logicalblk.invblk[--holditems * 2 + (sizeof(long) - 1)];
	    if ((i = iteminfo.e.size / 10) < maxback) {
		maxback = i;
		backupflag = howfar;
		gooditems = holditems;
		tptr2 = logicalblk.chrblk + iteminfo.e.offset;
	    }
	}
	/* see if backup will occur  */
	if (backupflag) {
	    numinvitems = gooditems;
	}
	logicalblk.invblk[0] = numinvitems;
	/* set forward pointer pointing to next */
	logicalblk.invblk[1] = numlogblk + 1; 
	/* set back pointer to last block */
	logicalblk.invblk[2] = numlogblk - 1;
	if (fwrite(logicalblk.chrblk, 1, sizeof(t_logicalblk), outfile) == 0) {
	    invcannotwrite(indexfile);
	    return(0);
	}
	amtused = 16;
	numlogblk++;
	/* check if had to back up, if so do it */
	if (backupflag) {
	    /* find out where the end of the new block is */
	    iteminfo.packword[0] = logicalblk.invblk[numinvitems*2+1];
	    tptr3 = logicalblk.chrblk + iteminfo.e.offset;
	    /* move the index for this block */
	    for (i = 3; i <= (backupflag * 2 + 2); i++)
		logicalblk.invblk[i] = logicalblk.invblk[numinvitems*2+i];
	    /* move the word into the super index */
	    iteminfo.packword[0] = logicalblk.invblk[3];
	    iteminfo.packword[1] = logicalblk.invblk[4];
	    tptr2 = logicalblk.chrblk + iteminfo.e.offset;
	    strncpy(supfing, tptr2, (int) iteminfo.e.size);
	    *(supfing + iteminfo.e.size) = '\0';
#if DEBUG
	    printf("backup %d at term=%s to term=%s\n",
		   backupflag, thisterm, supfing);
#endif
	    *supint++ = nextsupfing;
	    nextsupfing += strlen(supfing) + 1;
	    supfing += strlen(supfing) + 1;
	    /* now fix up the logical block */
	    tptr = logicalblk.chrblk + lastinblk;
	    lastinblk = sizeof(t_logicalblk);
	    tptr2 = logicalblk.chrblk + lastinblk;
	    j = tptr3 - tptr;
	    while (tptr3 > tptr)
		*--tptr2 = *--tptr3;
	    lastinblk -= j;
	    amtused += (8 * backupflag + j);
	    for (i = 3; i < (backupflag * 2 + 2); i += 2) {
		iteminfo.packword[0] = logicalblk.invblk[i];
		iteminfo.e.offset += (tptr2 - tptr3);
		logicalblk.invblk[i] = iteminfo.packword[0];
	    }
	    numinvitems = backupflag;
	} else { /* no backup needed */
	    numinvitems = 0;
	    lastinblk = sizeof(t_logicalblk);
	    /* add new term to superindex */
	    strcpy(supfing, thisterm);
	    supfing += strlen(thisterm) + 1;
	    *supint++ = nextsupfing;
	    nextsupfing += strlen(thisterm) + 1;
	}
    }
    /* HBB 20010501: Fixed bug by replacing magic number '8' by
     * what it actually represents. */
    lastinblk -= (numwilluse - 2 * sizeof(long));
    iteminfo.e.offset = lastinblk;
    iteminfo.e.size = len;
    iteminfo.e.space = 0;
    iteminfo.e.post = numpost;
    strncpy(logicalblk.chrblk + lastinblk, thisterm, len);
    amtused += numwilluse;
    logicalblk.invblk[(lastinblk/sizeof(long))+wdlen] = nextpost;
    if ((i = postptr - POST) > 0) {
	if (fwrite(POST, sizeof(POSTING), i, fpost) == 0) {
	    invcannotwrite(postingfile);
	    return(0);
	}
	nextpost += i * sizeof(POSTING);
    }
    logicalblk.invblk[3+2*numinvitems++] = iteminfo.packword[0];
    logicalblk.invblk[2+2*numinvitems] = iteminfo.packword[1];
    return(1);
}

/* 
 * If 'invname' ends with the 'from' substring, it is replaced inline with the
 * 'to' substring (which must be of the exact same length), and the function
 * returns 0. Otherwise, returns -1.  
 */

static int 
invflipname(char * invname, const char *from, const char *to)
{
	char *temp, *i = NULL;

	assert(strlen(from) == strlen(to));

	temp = invname - 1;
	while( (temp = strstr(temp + 1, from)))
		i = temp;
	if (!i || i[strlen(from)] != '\0')
		return -1;
	while(*to)
		*i++ = *to++;
	return 0;
}

int
invopen(INVCONTROL *invcntl, char *invname, char *invpost, int stat)
{
	int	read_index;

	if ((invcntl->invfile = vpfopen(invname, ((stat == 0) ? "rb" : "r+b"))) == NULL) {
		/* If db created without '-f', but now invoked with '-f cscope.out',
		 * we need to check for 'cscope.in.out', rather than 'cscope.out.in': 
		 * I.e, hack around our own violation of the inverse db naming convention */
		if (!invflipname(invname, INVNAME2, INVNAME)) {
			if ((invcntl->invfile = vpfopen(invname, ((stat == 0) ? "rb" : "r+b")))) 
				goto openedinvname;
			invflipname(invname, INVNAME, INVNAME2); /* change back for err msg */
		} 
		/* more silliness: if you create the db with '-f cscope', then try to open 
		 * it without '-f cscope', you'll fail unless we check for 'cscope.out.in'
		 * here. */
		else if (!invflipname(invname, INVNAME, INVNAME2)) {
			if ((invcntl->invfile = vpfopen(invname, ((stat == 0) ? "rb" : "r+b")))) 
				goto openedinvname;
			invflipname(invname, INVNAME2, INVNAME); /* change back for err msg */
		}	
		invcannotopen(invname);
		return(-1);
	}
openedinvname:
	if (fread(&invcntl->param, sizeof(invcntl->param), 1, invcntl->invfile) == 0) {
		fprintf(stderr, "%s: empty inverted file\n", argv0);
		goto closeinv;
	}
	if (invcntl->param.version != FMTVERSION) {
		fprintf(stderr, "%s: cannot read old index format; use -U option to force database to rebuild\n", argv0);
		goto closeinv;
	}
	assert(invcntl->param.sizeblk == sizeof(t_logicalblk));

	if (stat == 0 && invcntl->param.filestat == INVALONE) {
		fprintf(stderr, "%s: inverted file is locked\n", argv0);
		goto closeinv;
	}
	if ((invcntl->postfile = vpfopen(invpost, ((stat == 0) ? "rb" : "r+b"))) == NULL) {
		/* exact same naming convention hacks as above for invname */
		if (!invflipname(invpost, INVPOST2, INVPOST)) {
			if ((invcntl->postfile = vpfopen(invpost, ((stat == 0) ? "rb" : "r+b")))) 
				goto openedinvpost;
			invflipname(invpost, INVPOST, INVPOST2); /* change back for err msg */
		} else if (!invflipname(invpost, INVPOST, INVPOST2)) {
			if ((invcntl->postfile = vpfopen(invpost,((stat == 0)?"rb":"r+b")))) 
				goto openedinvpost;
			invflipname(invpost, INVPOST2, INVPOST); /* change back for err msg */
		}
		invcannotopen(invpost);
		goto closeinv;
	}
openedinvpost:
	/* allocate core for a logical block  */
	if ((invcntl->logblk = malloc((unsigned) invcntl->param.sizeblk)) == NULL) {
		invcannotalloc((unsigned) invcntl->param.sizeblk);
		goto closeboth;
	}
	/* allocate for and read in superfinger  */
	read_index = 1;
	invcntl->iindex = NULL;
#if SHARE
	if (invcntl->param.share == 1) {
		key_t shm_key;
		struct shmid_ds shm_buf;
		int	shm_id;

		/* see if the shared segment exists */
		shm_key = ftok(invname, 2);
		shm_id = shmget(shm_key, 0, 0);
		/* Failure simply means (hopefully) that segment doesn't exists */
		if (shm_id == -1) {
			/* Have to give general write permission due to AMdahl not having protected segments */
			shm_id = shmget(shm_key, invcntl->param.supsize + sizeof(long), IPC_CREAT | 0666);
			if (shm_id == -1)
				perror("Could not create shared memory segment");
		} else
			read_index = 0;

		if (shm_id != -1) {
			invcntl->iindex = shmat(shm_id, 0, ((read_index) ? 0 : SHM_RDONLY));
			if (invcntl->iindex == (char *)ERR) {
				fprintf(stderr, "%s: shared memory link failed\n", argv0);
				invcntl->iindex = NULL;
				read_index = 1;
			}
		}
	}
#endif
	if (invcntl->iindex == NULL)
	        /* FIXME HBB: magic number alert (4) */
		invcntl->iindex = malloc((unsigned) invcntl->param.supsize
					 + 4 *sizeof(long));
	if (invcntl->iindex == NULL) {
		invcannotalloc((unsigned) invcntl->param.supsize);
		free(invcntl->logblk);
		goto closeboth;
	}
	if (read_index) {
		fseek(invcntl->invfile, invcntl->param.startbyte, SEEK_SET);
		fread(invcntl->iindex, (int) invcntl->param.supsize, 1,
		      invcntl->invfile);
	}
	invcntl->numblk = -1;
	if (boolready() == -1) {
	closeboth:
		fclose(invcntl->postfile);
	closeinv:
		fclose(invcntl->invfile);
		return(-1);
	}
	/* write back out the control block if anything changed */
	invcntl->param.filestat = stat;
	if (stat > invcntl->param.filestat ) {
		rewind(invcntl->invfile);
		fwrite(&invcntl->param, sizeof(invcntl->param), 1, invcntl->invfile);
	}
	return(1);
}

/** invclose must be called to wrap things up and deallocate core  **/
void
invclose(INVCONTROL *invcntl)
{
	/* write out the control block in case anything changed */
	if (invcntl->param.filestat > 0) {
		invcntl->param.filestat = 0;
		rewind(invcntl->invfile);
		fwrite(&invcntl->param, 1,
		    sizeof(invcntl->param), invcntl->invfile);
	}
	if (invcntl->param.filestat == INVALONE) {
		/* write out the super finger */
		fseek(invcntl->invfile, invcntl->param.startbyte, SEEK_SET);
		fwrite(invcntl->iindex, 1,
		       (int) invcntl->param.supsize, invcntl->invfile);
	}
	fclose(invcntl->invfile);
	fclose(invcntl->postfile);
#if SHARE
	if (invcntl->param.share > 0) {
		shmdt(invcntl->iindex);
		invcntl->iindex = NULL;
	}
#endif
	if (invcntl->iindex != NULL)
		free(invcntl->iindex);
	free(invcntl->logblk);
}

/** invstep steps the inverted file forward one item **/
static void
invstep(INVCONTROL *invcntl)
{
	if (invcntl->keypnt < (invcntl->logblk->invblk[0] - 1)) {
		invcntl->keypnt++; 
		return;
	}

	/* move forward a block else wrap */
	invcntl->numblk = invcntl->logblk->invblk[1]; /* was: *(int *)(invcntl->logblk + sizeof(long))*/                           

	/* now read in the block  */
	fseek(invcntl->invfile,
	      invcntl->numblk*invcntl->param.sizeblk + invcntl->param.cntlsize,
	      SEEK_SET);
	fread(invcntl->logblk, (int) invcntl->param.sizeblk, 1,
	      invcntl->invfile); 
	invcntl->keypnt = 0; 
}

/** invforward moves forward one term in the inverted file  **/
int
invforward(INVCONTROL *invcntl)
{
	invstep(invcntl); 
	/* skip things with 0 postings */
	/* FIXME HBB: magic number alert! (3) */
	while (((ENTRY * )(invcntl->logblk->invblk + 3) + invcntl->keypnt)->post == 0) {
		invstep(invcntl); 
	}
	/* Check for having wrapped - reached start of inverted file! */
	if ((invcntl->numblk == 0) && (invcntl->keypnt == 0))
		return(0);
	return(1);
}

/**  invterm gets the present term from the present logical block  **/
long
invterm(INVCONTROL *invcntl, char *term)
{
	ENTRY * entryptr;

	/* FIXME HBB: magic number alert! (3) */
	entryptr = (ENTRY *)(invcntl->logblk->invblk + 3) + invcntl->keypnt;
	strncpy(term, invcntl->logblk->chrblk + entryptr->offset,
		       (int) entryptr->size);
	*(term + entryptr->size) = '\0';
	return(entryptr->post);
}

/** invfind searches for an individual item in the inverted file  **/
long
invfind(INVCONTROL *invcntl, char *searchterm) /* term being searched for  */
{
	int	imid, ilow, ihigh;
	long	num;
	int	i;
	unsigned long	*intptr, *intptr2;
	ENTRY *entryptr;

	/* make sure it is initialized via invready  */
	if (invcntl->invfile == 0)
		return(-1L);

	/* now search for the appropriate finger block */
	intptr = (unsigned long *)invcntl->iindex;

	ilow = 0;
	ihigh = *intptr++ - 1;
	while (ilow <= ihigh) {
		imid = (ilow + ihigh) / 2;
		intptr2 = intptr + imid;
		i = strcmp(searchterm, (invcntl->iindex + *intptr2));
		if (i < 0)
			ihigh = imid - 1;
		else if (i > 0)
			ilow = ++imid;
		else {
			ilow = imid + 1;
			break;
		}
	}
	/* be careful about case where searchterm is after last in this block  */
	imid = (ilow) ? ilow - 1 : 0;

	/* fetch the appropriate logical block if not in core  */
	/* note always fetch it if the file is busy */
	if ((imid != invcntl->numblk) || (invcntl->param.filestat >= INVBUSY)) {
		fseek(invcntl->invfile,
		      (imid*invcntl->param.sizeblk) + invcntl->param.cntlsize,
		      SEEK_SET);
		invcntl->numblk = imid;
		fread(invcntl->logblk, (int)invcntl->param.sizeblk, 1,
		      invcntl->invfile);
	}

srch_ext:
	/* now find the term in this block. tricky this  */
	intptr = (unsigned long *) invcntl->logblk->invblk;

	ilow = 0;
	ihigh = *intptr - 1;
	intptr += 3;
	num = 0;
	while (ilow <= ihigh) {
		imid = (ilow + ihigh) / 2;
		entryptr = (ENTRY *)intptr + imid;
		i = strncmp(searchterm, invcntl->logblk->chrblk + entryptr->offset,
		    (int) entryptr->size );
		if (i == 0)
			i = strlen(searchterm) - entryptr->size;
		if (i < 0)
			ihigh = imid - 1;
		else if (i > 0)
			ilow = ++imid;
		else {
			num = entryptr->post;
			break;
		}
	}
	/* be careful about case where searchterm is after last in this block  */
	if (imid >= invcntl->logblk->invblk[0]) {
		invcntl->keypnt = invcntl->logblk->invblk[0];
		invstep(invcntl);
		/* note if this happens the term could be in extended block */
		if (invcntl->param.startbyte < invcntl->numblk * invcntl->param.sizeblk)
			goto srch_ext;
	} else
		invcntl->keypnt = imid;
	return(num);
}

#if DEBUG

/** invdump dumps the block the term parameter is in **/
void
invdump(INVCONTROL *invcntl, char *term)
{
	long	i, j, n, *longptr;
	ENTRY * entryptr;
	char	temp[512], *ptr;

	/* dump superindex if term is "-"  */
	if (*term == '-') {
		j = atoi(term + 1);
		longptr = (long *)invcntl->iindex;
		n = *longptr++;
		printf("Superindex dump, num blocks=%ld\n", n);
		longptr += j;
		while ((longptr <= ((long *)invcntl->iindex) + n) && invbreak == 0) {
			printf("%2ld  %6ld %s\n", j++, *longptr, invcntl->iindex + *longptr);
			longptr++;
		}
		return;
	} else if (*term == '#') {
		j = atoi(term + 1);
		/* fetch the appropriate logical block */
		invcntl->numblk = j;
		fseek(invcntl->invfile,
		      (j * invcntl->param.sizeblk) + invcntl->param.cntlsize,
		      SEEK_SET);
		fread(invcntl->logblk, (int) invcntl->param.sizeblk, 1,
		      invcntl->invfile);
	} else
		i = abs((int) invfind(invcntl, term));
	longptr = invcntl->logblk->invblk;
	n = *longptr++;
	printf("Entry term to invdump=%s, postings=%ld, forwrd ptr=%ld, back ptr=%ld\n"
	    , term, i, *(longptr), *(longptr + 1));
	/* FIXME HBB: magic number alert! (3) */
	entryptr = (ENTRY *) (invcntl->logblk->invblk + 3);
	printf("%ld terms in this block, block=%ld\n", n, invcntl->numblk);
	printf("\tterm\t\t\tposts\tsize\toffset\tspace\t1st word\n");
	for (j = 0; j < n && invbreak == 0; j++) {
		ptr = invcntl->logblk->chrblk + entryptr->offset;
		strncpy(temp, ptr, (int) entryptr->size);
		temp[entryptr->size] = '\0';
		ptr += (sizeof(long) * (long)((entryptr->size + (sizeof(long) - 1)) / sizeof(long)));
		printf("%2ld  %-24s\t%5ld\t%3d\t%d\t%d\t%ld\n", j, temp, entryptr->post,
		    entryptr->size, entryptr->offset, entryptr->space,
		    *(long *)ptr);
		entryptr++;
	}
}
#endif

static int
boolready(void)
{
	numitems = 0;
	if (item1 != NULL) 
		free(item1);
	setsize1 = SETINC;
	if ((item1 = malloc(SETINC * sizeof(POSTING))) == NULL) {
		invcannotalloc(SETINC);
		return(-1);
	}
	if (item2 != NULL) 
		free(item2);
	setsize2 = SETINC;
	if ((item2 = malloc(SETINC * sizeof(POSTING))) == NULL) {
		invcannotalloc(SETINC);
		return(-1);
	}
	item = item1;
	enditem = item;
	return(0);
}

void
boolclear(void)
{
	numitems = 0;
	item = item1;
	enditem = item;
}

POSTING *
boolfile(INVCONTROL *invcntl, long *num, int boolarg)
{
	ENTRY	*entryptr;
	FILE	*file;
	void	*ptr;
	unsigned long	*ptr2;
	POSTING	*newitem = NULL; /* initialize, to avoid warning */
	POSTING	posting;
	unsigned u;
	POSTING *newsetp = NULL, *set1p;
	long	newsetc, set1c, set2c;

	/* FIXME HBB: magic number alert! (3) */
	entryptr = (ENTRY *) (invcntl->logblk->invblk + 3) + invcntl->keypnt;
	ptr = invcntl->logblk->chrblk + entryptr->offset;
	ptr2 = ((unsigned long *) ptr) + (entryptr->size + (sizeof(long) - 1)) / sizeof(long);
	*num = entryptr->post;
	switch (boolarg) {
	case BOOL_OR:
	case NOT:
		if (*num == 0) {
			*num = numitems;
			return(item);
		}
	}
	/* make room for the new set */
	u = 0;
	switch (boolarg) {
	case AND:
	case NOT:
		newsetp = set1p = item;
		break;

	case BOOL_OR:
		u = enditem - item;
		/* FALLTHROUGH */
	case REVERSENOT:
		u += *num;
		if (item == item2) {
			if (u > setsize1) {
				u += SETINC;
				if ((item1 = realloc(
				    item1, u * sizeof(POSTING))) == NULL) {
					goto cannotalloc;
				}
				setsize1 = u;
			}
			newitem = item1;
		}
		else {
			if (u > setsize2) {
				u += SETINC;
				if ((item2 = realloc( 
				    item2, u * sizeof(POSTING))) == NULL) {
				cannotalloc:
					invcannotalloc(u * sizeof(POSTING));
					boolready();
					*num = -1;
					return(NULL);
				}
				setsize2 = u;
			}
			newitem = item2;
		}
		set1p = item;
		newsetp = newitem;
	}
	file = invcntl->postfile;
	fseek(file, *ptr2, SEEK_SET);
	fread(&posting, sizeof(posting), 1, file);
	newsetc = 0;
	switch (boolarg) {
	case BOOL_OR:
		/* while something in both sets */
		set1p = item;
		newsetp = newitem;
		for (set1c = 0, set2c = 0;
		    set1c < numitems && set2c < *num; newsetc++) {
			if (set1p->lineoffset < posting.lineoffset) {
				*newsetp++ = *set1p++;
				set1c++;
			}
			else if (set1p->lineoffset > posting.lineoffset) {
				*newsetp++ = posting;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else if (set1p->type < posting.type) {
				*newsetp++ = *set1p++;
				set1c++;
			}
			else if (set1p->type > posting.type) {
				*newsetp++ = posting;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else {	/* identical postings */
				*newsetp++ = *set1p++;
				set1c++;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
		}
		/* find out what ran out and move the rest in */
		if (set1c < numitems) {
			newsetc += numitems - set1c;
			while (set1c++ < numitems) {
				*newsetp++ = *set1p++;
			}
		} else {
			while (set2c++ < *num) {
				*newsetp++ = posting;
				newsetc++;
				fread(&posting, (int) sizeof(posting), 1, file);
			}
		}
		item = newitem;
		break; /* end of BOOL_OR */
#if 0
	case AND:
		for (set1c = 0, set2c = 0; set1c < numitems && set2c < *num; ) {
			if (set1p->lineoffset < posting.lineoffset) {
				set1p++;
				set1c++;
			}
			else if (set1p->lineoffset > posting.lineoffset) {
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else if (set1p->type < posting.type)  {
				*set1p++;
				set1c++;
			}
			else if (set1p->type > posting.type) {
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else {	/* identical postings */
				*newsetp++ = *set1p++;
				newsetc++;
				set1c++;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
		}
		break; /* end of AND */

	case NOT:
		for (set1c = 0, set2c = 0; set1c < numitems && set2c < *num; ) {
			if (set1p->lineoffset < posting.lineoffset) {
				*newsetp++ = *set1p++;
				newsetc++;
				set1c++;
			}
			else if (set1p->lineoffset > posting.lineoffset) {
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else if (set1p->type < posting.type) {
				*newsetp++ = *set1p++;
				newsetc++;
				set1c++;
			}
			else if (set1p->type > posting.type) {
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else {	/* identical postings */
				set1c++;
				set1p++;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
		}
		newsetc += numitems - set1c;
		while (set1c++ < numitems) {
			*newsetp++ = *set1p++;
		}
		break; /* end of NOT */

	case REVERSENOT:  /* core NOT incoming set */
		for (set1c = 0, set2c = 0; set1c < numitems && set2c < *num; ) {
			if (set1p->lineoffset < posting.lineoffset) {
				set1p++;
				set1c++;
			}
			else if (set1p->lineoffset > posting.lineoffset) {
				*newsetp++ = posting;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else if (set1p->type < posting.type) {
				set1p++;
				set1c++;
			}
			else if (set1p->type > posting.type) {
				*newsetp++ = posting;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
			else {	/* identical postings */
				set1c++;
				set1p++;
				fread(&posting, (int) sizeof(posting), 1, file);
				set2c++;
			}
		}
		while (set2c++ < *num) {
			*newsetp++ = posting;
			newsetc++;
			fread(&posting, (int) sizeof(posting), 1, file);
		}
		item = newitem;
		break; /* end of REVERSENOT  */
#endif
	}
	numitems = newsetc;
	*num = newsetc;
	enditem = (POSTING *) newsetp;
	return((POSTING *) item);
}

#if 0
POSTING *
boolsave(int clear)		/* flag about whether to clear core  */
{
	int	i;
	POSTING	*ptr;
	POSTING	*oldstuff, *newstuff;

	if (numitems == 0) {
		if (clear) 
			boolclear();
		return(NULL);
	}
	/* if clear then give them what we have and use boolready to realloc  */
	if (clear) {
		ptr = item;
		/* free up the space we didn't give them */
		if (item == item1)
			item1 = NULL;
		else
			item2 = NULL;
		boolready();
		return(ptr);
	}
	i = (enditem - item) * sizeof(POSTING) + 100;
	if ((ptr = malloc(i))r == NULL) {
		invcannotalloc(i);
		return(ptr);
	}
	/* move present set into place  */
	oldstuff = item;
	newstuff = ptr;
	while (oldstuff < enditem)
		*newstuff++ = *oldstuff++;
	return(ptr);
}
#endif

static void
invcannotalloc(unsigned n)
{
	fprintf(stderr, "%s: cannot allocate %u bytes\n", argv0, n);
}

static void
invcannotopen(char *file)
{
	fprintf(stderr, "%s: cannot open file %s\n", argv0, file);
}

static void
invcannotwrite(char *file)
{
	perror(argv0);	/* must be first to preserve errno */
	fprintf(stderr, "%s: write to file %s failed\n", argv0, file);
}
