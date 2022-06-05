#! /usr/bin/osascript
-- joinList from Geert Vanderkelen @ bit.ly/1gRPYbH
-- toDo push new terminal to background after creation
to joinList(aList, delimiter)
    set retVal to ""
    set prevDelimiter to AppleScript's text item delimiters
    set AppleScript's text item delimiters to delimiter
    set retVal to aList as string
    set AppleScript's text item delimiters to prevDelimiter
    return retVal
end joinList

-- theSplit from iTerm version check example @ https://goo.gl/dSbQYU
on theSplit(theString, theDelimiter)
    set oldDelimiters to AppleScript's text item delimiters
    set AppleScript's text item delimiters to theDelimiter
    set theArray to every text item of theString
    set AppleScript's text item delimiters to oldDelimiters
    return theArray
end theSplit

-- IsModernVersion from iTerm version check example @ https://goo.gl/dSbQYU
on IsModernVersion(version)
    set myArray to my theSplit(version, ".")
    set major to item 1 of myArray
    set minor to item 2 of myArray
    set veryMinor to item 3 of myArray

    if major < 2 then
        return false
    end if
    if major > 2 then
        return true
    end if
    if minor < 9 then
        return false
    end if
    if minor > 9 then
        return true
    end if
    if veryMinor < 20140903 then
        return false
    end if
    return true
end IsModernVersion

on run arg
    set thecommand to joinList(arg, " ")
    tell application "iTerm"
        activate
        if my IsModernVersion(version) then
            set myterm to (create window with default profile)
            set mysession to current session of myterm
        else
            set myterm to (make new teminal)
            tell myterm
                set mysession to (launch session "Default")
            end tell
        end if
        tell myterm
            tell mysession
                write text thecommand
            end tell
        end tell
    end tell
end run
