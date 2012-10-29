#!/bin/bash
#===============================================================================
#          FILE:  wrapper.sh
#         USAGE:  ./wrapper.sh executable [cmd-line-args] 
#   DESCRIPTION:  Wraps the execution of a programm or script.
#                 Use with xterm: xterm -e wrapper.sh executable cmd-line-args
#                 This script is used by the plugins c.vim 
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:  Dr.-Ing. Fritz Mehner (Mn), mehner@fh-swf.de
#       COMPANY:  Fachhochschule Südwestfalen, Iserlohn
#       CREATED:  23.11.2004 18:04:01 CET
#      REVISION:  $Id: wrapper.sh,v 1.5 2009/06/03 17:47:06 mehner Exp $
#===============================================================================

executable="${1}"                               # name of the executable

if [ ${#} -ge 1 ] && [ -x "$executable" ]
then
  "${@}"
  returncode=$?
  [ $returncode -ne 0 ] && printf "'${@}' returned ${returncode}\n"
else
  printf "\n  !! file \"${executable}\" does not exist or is not executable !!\n"
  returncode=126                                # command invoked cannot execute
fi
read -p "  ... press return key ... " dummy
exit $returncode
