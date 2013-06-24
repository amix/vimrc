/*
*   $Id: mac.c 443 2006-05-30 04:37:13Z darren $
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
*   This module contains support functions for Exuberant Ctags on Macintosh.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"

#include <Files.h>
#include <TextUtils.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*
*   FUNCTION DEFINITIONS
*/

static int get_path(const char* in_unix_path, unsigned char* out_mac_path)
{
	int l = strlen(in_unix_path);
	int result = 0;
	
	if (l > 254)
		result = -1;
	else
	{
		const char* s = in_unix_path;
		char *d = (char*)out_mac_path + 1;
		
		if (*s != '/')
			*d++ = ':';
		else
			++s;
		
		while (*s)
		{
			if (s[0] == '.' && s[1] == '.' && s[2] == '/')
			{
				s += 3;
				*d++ = ':';
			}
			else if (s[0] == '.' && s[1] == '/')
				s += 2;
			else if (s[0] == '/')
			{
				*d++ = ':';
				
				++s;
				while (*s == '/')
					++s;
			}
			else
				*d++ = *s++;
		}

		out_mac_path[0] = (d - (char*)out_mac_path) - 1;
	}
	
	return result;
}

DIR *opendir(const char *dirname)
{
	DIR* dirp = (DIR*)calloc(1, sizeof(DIR));

	if (dirp != NULL)
	{
		OSErr err;
		Str255 s;
		CInfoPBRec pb = { 0 };
		
		if (strcmp(dirname, "."))
		{
			get_path(dirname, s);
			pb.hFileInfo.ioNamePtr = s;
		}
		else
			pb.hFileInfo.ioNamePtr = NULL;
		
		err = PBGetCatInfoSync(&pb);
		if (err != noErr || (pb.hFileInfo.ioFlAttrib & ioDirMask) == 0)
		{
			free(dirp);
			dirp = NULL;
		}
		else
		{
			dirp->file.vRefNum = pb.hFileInfo.ioVRefNum;
			dirp->file.parID = pb.hFileInfo.ioDirID;
			dirp->file.name[0] = '\0';
			dirp->index = 1;
		}
	}
	
	return dirp;
}

struct dirent *readdir(DIR *dirp)
{
	if (dirp)
	{
		CInfoPBRec pb = { 0 };
		
		pb.hFileInfo.ioVRefNum = dirp->file.vRefNum;
		pb.hFileInfo.ioDirID = dirp->file.parID;
		pb.hFileInfo.ioFDirIndex = dirp->index++;
		pb.hFileInfo.ioNamePtr = dirp->file.name;
	
		if (PBGetCatInfoSync(&pb) != noErr)
			return NULL;
		
		memcpy(dirp->ent.d_name, dirp->file.name + 1, dirp->file.name[0]);
		dirp->ent.d_name[dirp->file.name[0]] = 0;
		return &dirp->ent;
	}
	return NULL;
}

int closedir(DIR *dirp)
{
	if (dirp)
		free(dirp);
	return 0;
}

void rewinddir(DIR *dirp)
{
	if (dirp)
		dirp->index = 1;
}

int mstat(const char* file, struct stat* st)
{
	CInfoPBRec		pb;
	unsigned char	path[256];
	int				result = 0;

	memset(&pb, 0, sizeof(CInfoPBRec));

	if (strcmp(file, ".") == 0)
	{
		memset(st, 0, sizeof(struct stat));
		st->st_mode = S_IFDIR;
		st->st_ino = -1;
	}
	else
	{
		result = get_path(file, path);
		
		if (result == 0)
		{
			pb.hFileInfo.ioNamePtr = path;
			
			if (PBGetCatInfoSync(&pb) != noErr)
				result = -1;
			else
			{
				memset(st, 0, sizeof(struct stat));
	
				if (pb.hFileInfo.ioFlAttrib & ioDirMask)
					st->st_mode = S_IFDIR;
				else
					st->st_mode = S_IFREG;

				st->st_ino = pb.hFileInfo.ioFlStBlk;
				st->st_dev = pb.hFileInfo.ioVRefNum;
				st->st_nlink = 1;
				st->st_size = pb.hFileInfo.ioFlLgLen;
				st->st_atime = pb.hFileInfo.ioFlMdDat;
				st->st_mtime = pb.hFileInfo.ioFlMdDat;
				st->st_ctime = pb.hFileInfo.ioFlCrDat;
			}
		}
	}

	return result;
}

#undef fopen

FILE* mfopen(const char* file, const char* mode)
{
	unsigned char path[256];
		
	if (get_path(file, path) == 0)
	{
		int l = path[0];
		memmove(path, path + 1, l);
		path[l] = 0;
		return fopen((char*)path, mode);
	}
	else
		return NULL;
}

char* getcwd(char* out_path, int out_path_len)
{
	OSErr		err = noErr;
	CInfoPBRec	pb;
	FSSpec		cwd;

	if (out_path == NULL)
	{
		if (out_path_len < PATH_MAX)
			out_path_len = PATH_MAX;
		out_path = (char*)malloc(out_path_len);
	}
	
	err = FSMakeFSSpec(0, 0, "\p:", &cwd);
	
	if (cwd.parID == fsRtParID)
	{
		*out_path = '/';
		memcpy(out_path + 1, cwd.name + 1, cwd.name[0]);
		out_path[1 + cwd.name[0]] = 0;
	}
	else
	{
		/* The object isn't a volume */
		
		/* Is the object a file or a directory? */
		
		char t[PATH_MAX];
		char* s;

		s = t + PATH_MAX - cwd.name[0] - 1;
		memcpy(s, cwd.name + 1, cwd.name[0]);
		s[cwd.name[0]] = 0;
		
		/* Get the ancestor directory names */
		pb.dirInfo.ioNamePtr = cwd.name;
		pb.dirInfo.ioVRefNum = cwd.vRefNum;
		pb.dirInfo.ioDrParID = cwd.parID;
		do  /* loop until we have an error or find the root directory */
		{
			pb.dirInfo.ioFDirIndex = -1;
			pb.dirInfo.ioDrDirID = pb.dirInfo.ioDrParID;
			err = PBGetCatInfoSync(&pb);
			if ( err == noErr )
			{
				*--s = '/';
				s -= cwd.name[0];
				memcpy(s, cwd.name + 1, cwd.name[0]);
			}
		}
		while (err == noErr && pb.dirInfo.ioDrDirID != fsRtDirID && s > t + 1);

		if (s > t + 1)
		{
			*--s = '/';
			strcpy(out_path, s);
		}
		else
			strcpy(out_path, ".");
	}

	return out_path;
}

/* vi:set tabstop=4 shiftwidth=4: */
