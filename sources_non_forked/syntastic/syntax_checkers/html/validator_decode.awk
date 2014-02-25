#!/usr/bin/awk -f
#============================================================================
#File:        validator_decode.awk
#Description: Helper script for validator.vim
#Maintainer:  LCD 47 <lcd047 at gmail dot com>
#License:     This program is free software. It comes without any warranty,
#             to the extent permitted by applicable law. You can redistribute
#             it and/or modify it under the terms of the Do What The Fuck You
#             Want To Public License, Version 2, as published by Sam Hocevar.
#             See http://sam.zoy.org/wtfpl/COPYING for more details.
#
#============================================================================

BEGIN {
    FS = OFS = "\""
    hextab ["0"] = 0; hextab ["8"] = 8;
    hextab ["1"] = 1; hextab ["9"] = 9;
    hextab ["2"] = 2; hextab ["A"] = hextab ["a"] = 10
    hextab ["3"] = 3; hextab ["B"] = hextab ["b"] = 11;
    hextab ["4"] = 4; hextab ["C"] = hextab ["c"] = 12;
    hextab ["5"] = 5; hextab ["D"] = hextab ["d"] = 13;
    hextab ["6"] = 6; hextab ["E"] = hextab ["e"] = 14;
    hextab ["7"] = 7; hextab ["F"] = hextab ["f"] = 15;
}

function urldecode (url) {
    decoded = ""
    i = 1
    len = length (url)
    while ( i <= len ) {
        c = substr (url, i, 1)
        if ( c == "%" ) {
            if ( i + 2 <= len ) {
                c1 = substr (url, i + 1, 1)
                c2 = substr (url, i + 2, 1)
                if ( hextab [c1] != "" && hextab [c2] != "" ) {
                    code = 0 + hextab [c1] * 16 + hextab [c2] + 0
                    c = sprintf ("%c", code)
                }
                else
                    c = c c1 c2
                i += 2
            }
            else if ( i + 1 <= len ) {
                c = substr (url, i, 2)
                i++
            }
        }
        else if ( c == "+" )
            c = " "
        decoded = decoded c
        i++
    }
    return decoded
}

{ 
    $2 =  urldecode($2)
    gsub ("\\\\\"", "\"", $2)
    print
}
