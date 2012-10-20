/*
*   $Id: e_mac.h 443 2006-05-30 04:37:13Z darren $
*
*   Copyright (c) 2001, Maarten L. Hekkelman
*
*   Author: Maarten L. Hekkelman <maarten@hekkelman.com>
*           http://www.hekkelman.com
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License. It is provided on an as-is basis and no
*   responsibility is accepted for its failure to perform as expected.
*
*   Configures ctags for Macintosh environment.
*/
#ifndef E_MAC_H
#define E_MAC_H

#define BUILD_MPW_TOOL 1

#define MACROS_USE_PATTERNS	1
#define DEFAULT_FILE_FORMAT 2
#define INTERNAL_SORT 1
#define TMPDIR "/tmp"
#define NEED_PROTO_TRUNCATE 1
#define STDC_HEADERS 1
#define HAVE_CLOCK 1
#define HAVE_FGETPOS 1
#define HAVE_OPENDIR 1
#define HAVE_REMOVE 1
#define HAVE_SETENV 1
#define HAVE_STRERROR 1
#define HAVE_STRSTR 1
#define HAVE_FCNTL_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_DIR_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_TIME_H 1
#define HAVE_TYPES_H 1
#define HAVE_STDLIB_H 1

#include <time.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>

#include <Files.h>

#if BUILD_MPW_TOOL

/*
	The following defines are collected from various header files from some
	Linux distribution
*/

typedef unsigned long  mode_t;
typedef unsigned long  ino_t;
typedef unsigned long  dev_t;
typedef short          nlink_t;
typedef unsigned long  uid_t;
typedef unsigned long  gid_t;

/* Encoding of the file mode.  */
#define	S_IFMT  0170000  /* These bits determine file type.  */

/* File types.  */
#define	S_IFDIR  0040000  /* Directory.  */
#define	S_IFCHR  0020000  /* Character device.  */
#define	S_IFBLK  0060000  /* Block device.  */
#define	S_IFREG  0100000  /* Regular file.  */

#define	S_ISTYPE(mode, mask)  (((mode) & S_IFMT) == (mask))

#define	S_ISDIR(mode)  S_ISTYPE((mode), S_IFDIR)
#define	S_ISCHR(mode)  S_ISTYPE((mode), S_IFCHR)
#define	S_ISBLK(mode)  S_ISTYPE((mode), S_IFBLK)
#define	S_ISREG(mode)  S_ISTYPE((mode), S_IFREG)

struct stat {
	dev_t              st_dev;      /* Device.  */
	unsigned short int __pad1;
	ino_t              st_ino;      /* File serial number.	*/
	mode_t             st_mode;     /* File mode.  */
	nlink_t            st_nlink;    /* Link count.  */
	uid_t              st_uid;      /* User ID of the file's owner.	*/
	gid_t              st_gid;      /* Group ID of the file's group.*/
	off_t              st_size;     /* Size of file, in bytes.  */
	unsigned long int  st_blksize;  /* Optimal block size for I/O.  */
	long               st_blocks;   /* Number 512-byte blocks allocated. */
	time_t             st_atime;    /* Time of last access.  */
	time_t             st_mtime;    /* Time of last modification.  */
	time_t             st_ctime;    /* Time of last status change.  */
};

int fstat(int fildes, struct stat *buf);

#else
#include <console.h>
#include <stat.mac.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

/*
	Our own stat, accepts unix like paths.
*/
int mstat(const char *path, struct stat *buf);

struct dirent {
	char d_name[64];
};

typedef struct {
	FSSpec file;
	int index;
	struct dirent ent;
} DIR;

extern DIR* opendir(const char *dirname);
extern struct dirent* readdir(DIR* dirp);
extern int closedir(DIR* dirp);
extern void rewinddir(DIR* dirp);
extern char* getcwd(char*, int);

/*
	Our own fopen, accepts unix like paths.
*/
extern FILE* mfopen(const char* file, const char* mode);

/*
	Dirty, define the standard functions fopen, stat and lstat to map to our
	own routines.
*/
#define fopen       mfopen
#define stat(a,b)   mstat(a,b)
#define lstat(a,b)  mstat(a,b)

#endif

/* vi:set tabstop=4 shiftwidth=4: */
