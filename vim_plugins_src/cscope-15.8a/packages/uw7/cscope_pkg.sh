#!/bin/sh

#===========================================================================
# Copyright (c) 1998-2000, The Santa Cruz Operation 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# *Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# *Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# *Neither name of The Santa Cruz Operation nor the names of its contributors
# may be used to endorse or promote products derived from this software
# without specific prior written permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
# IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE. 
# =========================================================================*/

# This simple script creates an UW7 pkg for cscope
# Execute these commands as root

BINDIR=/usr/local/bin/
LIBDIR=/usr/local/lib/
MANDIR=/usr/local/man/man1/
SPOOLDIR=/var/spool/pkg/

# Check that the user is root

if [ `id -un` != "root" ]
then
	echo "ERROR: Need to be root to build package" >&2
	exit 1;
fi

# Check that cscope has been installed
if [ ! -f $BINDIR/cscope ]
then
	echo "ERROR: cscope not installed" >&2
	exit 1
fi

if [ ! -f $BINDIR/ocs ]
then
	echo "ERROR: ocs not installed" >&2
	exit 1
fi

if [ ! -f $MANDIR/cscope.1 ]
then
	echo "ERROR: cscope man page not installed" >&2
	exit 1
fi

# Check that there is no current cscope pkg spooled

if [ -d $SPOOLDIR/cscope ]
then
	echo "ERROR: cscope is currently spooled in $SPOOLDIR/cscope." >&2
	echo "       Please remove this directory and its contents to" >&2
	echo "       continue." >&2
	exit 1
fi

# Make the spool dir entry
# Note that cscope must be already installed
echo "INFO: Spooling package" >&2
pkgmk	
if [ $? -ne 0 ]
then
	echo "ERROR: An error occurred during spooling the package" >&2
	exit 1
fi

# Install the spool dir cscope entry into the package
echo "INFO: creating package" >&2
pkgtrans -s /var/spool/pkg cscope.16.0a.pkg cscope
if [ $? -ne 0 ]
then
	echo "ERROR: An error occurred while creating the package" >&2
	exit 1
fi

