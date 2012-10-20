#! /bin/sh
###############################################################################
#
# File:         cscope-indexer
# RCS:          $Header: /cvsroot/cscope/cscope/contrib/xcscope/cscope-indexer,v 1.2 2001/06/28 04:39:47 darrylo Exp $
# Description:  Script to index files for cscope
#
#		This script generates a list of files to index
#		(cscope.out), which is then (optionally) used to
#		generate a cscope database.  You can use this script
#		to just build a list of files, or it can be used to
#		build a list and database.  This script is not used to
#		just build a database (skipping the list of files
#		step), as this can be simply done by just calling
#		"cscope -b".
#
#		Normally, cscope will do its own indexing, but this
#		script can be used to force indexing.  This is useful
#		if you need to recurse into subdirectories, or have
#		many files to index (you can run this script from a
#		cron job, during the night).  It is especially useful
#		for large projects, which can contstantly have source
#		files added and deleted; by using this script, the
#		changing sources files are automatically handled.
#
#		Currently, any paths containing "/CVS/" or "/RCS/" are
#		stripped out (ignored).
#
#		This script is written to use only basic shell features, as
#		not all shells have advanced features.
#
# Author:       Darryl Okahata
# Created:      Thu Apr 27 17:12:14 2000
# Modified:     Tue Jun 19 09:47:45 2001 (Darryl Okahata) darrylo@soco.agilent.com
# Language:     Shell-script
# Package:      N/A
# Status:       Experimental
#
# (C) Copyright 2000, Darryl Okahata, all rights reserved.
#
###############################################################################
#
# Usage:
#
#     cscope-indexer [ -v ] [-f database_file ] [-i list_file ] [ -l ] [ -r ]
#
# where:
#
#     -f database_file
#          Specifies the cscope database file (default: cscope.out).
#
#     -i list_file
#          Specifies the name of the file into which the list of files
#          to index is placed (default: cscope.files).
#
#     -l
#          Suppress the generation/updating of the cscope database
#          file.  Only a list of files is generated.
#
#     -r
#          Recurse into subdirectories to locate files to index.
#          Without this option, only the current directory is
#          searched.
#
#     -v
#          Be verbose.  Output simple progress messages.
#
#
###############################################################################
set -e

# May have to edit this:
PATH="/usr/local/bin:/sbin:/usr/sbin:/bin:/usr/bin:$PATH"
export PATH

LIST_ONLY=
DIR='.'
LIST_FILE='cscope.files'
DATABASE_FILE='cscope.out'
RECURSE=
VERBOSE=
export DIR RECURSE			# Need to pass these to subprocesses

while [ -n "$1" ]
do
    case "$1" in
    -f)
	if [ "X$2" = "X" ]
	then
	    echo "$0: No database file specified" >&2
	    exit 1
	fi
	DATABASE_FILE="$2"
	shift
	;;
    -i)
	if [ "X$2" = "X" ]
	then
	    echo "$0: No list file specified" >&2
	    exit 1
	fi
	LIST_FILE="$2"
	shift
	;;
    -l)
	LIST_ONLY=1
	;;
    -r)
	RECURSE=1
	;;
    -v)
	VERBOSE=1
	;;
    *)
	DIR="$1"
	;;
    esac
    shift
done

cd $DIR

if [ "X$VERBOSE" != "X" ]
then
    echo "Creating list of files to index ..."
fi

(
    if [ "X$RECURSE" = "X" ]
    then
	# Ugly, inefficient, but it works.
	for f in *
	do
	    echo "$DIR/$f"
	done
    else
	find $DIR \( -type f -o -type l \)
    fi
) | \
    egrep -i '\.([chly](xx|pp)*|cc|hh)$' | \
    sed -e '/\/CVS\//d' -e '/\/RCS\//d' -e 's/^\.\///' | \
    sort > $LIST_FILE

if [ "X$VERBOSE" != "X" ]
then
    echo "Creating list of files to index ... done"
fi

if [ "X$LIST_ONLY" != "X" ]
then
    exit 0
fi

if [ "X$VERBOSE" != "X" ]
then
    echo "Indexing files ..."
fi

cscope -b -i $LIST_FILE -f $DATABASE_FILE

if [ "X$VERBOSE" != "X" ]
then
    echo "Indexing files ... done"
fi

exit 0
