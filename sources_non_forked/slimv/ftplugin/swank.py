#!/usr/bin/env python

###############################################################################
#
# SWANK client for Slimv
# swank.py:     SWANK client code for slimv.vim plugin
# Version:      0.9.14
# Last Change:  22 Aug 2021
# Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
# License:      This file is placed in the public domain.
#               No warranty, express or implied.
#               *** ***   Use At-Your-Own-Risk!   *** ***
#
###############################################################################

from __future__ import print_function

import os
import socket
import time
import select
import string
import sys
import re

input_port      = 4005
output_port     = 4006
lenbytes        = 6             # Message length is encoded in this number of bytes
maxmessages     = 50            # Maximum number of messages to receive in one listening session
recv_timeout    = 0.001         # socket recv timeout in seconds
listen_retries  = 10            # number of retries if no response in swank_listen()
sock            = None          # Swank socket object
id              = 0             # Message id
debug           = False
log             = False         # Set this to True in order to enable logging
logfile         = 'swank.log'   # Logfile name in case logging is on
pid             = '0'           # Process id
current_thread  = '0'
use_unicode     = True          # Use unicode message length counting
debug_active    = False         # Swank debugger is active
debug_activated = False         # Swank debugger was activated
read_string     = None          # Thread and tag in Swank read string mode
empty_last_line = True          # Swank output ended with a new line
prompt          = 'SLIMV'       # Command prompt
package         = 'COMMON-LISP-USER' # Current package
actions         = dict()        # Swank actions (like ':write-string'), by message id
indent_info     = dict()        # Data of :indentation-update
frame_locals    = dict()        # Map frame variable names to their index
inspect_lines   = 0             # Number of lines in the Inspector (excluding help text)
inspect_newline = True          # Start a new line in the Inspector (for multi-part objects)
inspect_package = ''            # Package used for the current Inspector
swank_version   = ''            # Swank version string in format YYYY-MM-DD
swank_param     = ''            # Additional parameter for the swank listener


###############################################################################
# Basic utility functions
###############################################################################

def logprint(text):
    if log:
        f = open(logfile, "a")
        f.write(text + '\n')
        f.close()

def logtime(text):
    if sys.version_info >= (3, 3):
        logprint(text + ' ' + str(time.perf_counter()))
    else:
        logprint(text + ' ' + str(time.clock()))

###############################################################################
# Simple Lisp s-expression parser
###############################################################################

# Possible error codes
PARSERR_NOSTARTBRACE        = -1    # s-expression does not start with a '('
PARSERR_NOCLOSEBRACE        = -2    # s-expression does not end with a '('
PARSERR_NOCLOSESTRING       = -3    # string is not closed with double quote
PARSERR_MISSINGLITERAL      = -4    # literal is missing after the escape character
PARSERR_EMPTY               = -5    # s-expression is empty


def parse_comment( sexpr ):
    """Parses a ';' Lisp comment till the end of line, returns comment length
    """
    pos = sexpr.find( '\n' )
    if pos >= 0:
        return pos + 1
    return len( sexpr )

def parse_keyword( sexpr ):
    """Parses a Lisp keyword, returns keyword length
    """
    for pos in range( len( sexpr ) ):
        if sexpr[pos] in string.whitespace + ')]':
            return pos
    return pos

def parse_sub_sexpr( sexpr, opening, closing ):
    """Parses a Lisp sub -expression, returns parsed string length
       and a Python list built from the s-expression,
       expression can be a Clojure style list surrounded by braces
    """
    result = []
    l = len( sexpr )
    for pos in range( l ):
        # Find first opening '(' or '['
        if sexpr[pos] == opening:
            break
        if not sexpr[pos] in string.whitespace:
            # S-expression does not start with '(' or '['
            return [PARSERR_NOSTARTBRACE, result]
    else:
        # Empty s-expression
        return [PARSERR_EMPTY, result]

    pos = pos + 1
    quote_cnt = 0
    while pos < l:
        literal = 0
        if sexpr[pos] == '\\':
            literal = 1
            pos = pos + 1
            if pos == l:
                return [PARSERR_MISSINGLITERAL, result]
        if not literal and sexpr[pos] == '"':
            # We toggle a string
            quote_cnt = 1 - quote_cnt
            if quote_cnt == 1:
                quote_pos = pos
            else:
                result = result + [sexpr[quote_pos:pos+1]]
        elif quote_cnt == 0:
            # We are not in a string
            if not literal and sexpr[pos] == '(':
                # Parse sub expression
                [slen, subresult] = parse_sub_sexpr( sexpr[pos:], '(', ')' )
                if slen < 0:
                    # Sub expression parsing error
                    return [slen, result]
                result = result + [subresult]
                pos = pos + slen - 1
            elif not literal and sexpr[pos] == '[':
                # Parse sub expression
                [slen, subresult] = parse_sub_sexpr( sexpr[pos:], '[', ']' )
                if slen < 0:
                    # Sub expression parsing error
                    return [slen, result]
                result = result + [subresult]
                pos = pos + slen - 1
            elif not literal and sexpr[pos] == closing:
                # End of this sub expression
                return [pos + 1, result]
            elif not literal and sexpr[pos] != closing and sexpr[pos] in ')]':
                # Wrong closing brace/bracket
                return [PARSERR_NOCLOSEBRACE, result]
            elif not literal and sexpr[pos] == ';':
                # Skip coment
                pos = pos + parse_comment( sexpr[pos:] ) - 1
            elif not literal and sexpr[pos] in "#'`@~,^":
                # Skip prefix characters
                while pos+1 < l and sexpr[pos+1] not in string.whitespace + '([':
                    pos = pos + 1
            elif not sexpr[pos] in string.whitespace + '\\':
                # Parse keyword but ignore dot in dotted notation (a . b)
                klen = parse_keyword( sexpr[pos:] )
                if klen > 1 or sexpr[pos] != '.':
                    result = result + [sexpr[pos:pos+klen]]
                    pos = pos + klen - 1
        pos = pos + 1

    if quote_cnt != 0:
        # Last string is not closed
        return [PARSERR_NOCLOSESTRING, result]
    # Closing ')' or ']' not found
    return [PARSERR_NOCLOSEBRACE, result]

def parse_sexpr( sexpr ):
    """Parses a Lisp s-expression, returns parsed string length
       and a Python list built from the s-expression
    """
    return parse_sub_sexpr( sexpr, '(', ')' )


###############################################################################
# Swank server interface
###############################################################################

class swank_action:
    def __init__ (self, id, name, data):
        self.id = id
        self.name = name
        self.data = data
        self.result = ''
        self.pending = True

def get_prompt():
    global prompt
    if prompt.rstrip()[-1] == '>':
        return prompt + ' '
    else:
        return prompt + '> '

def unquote(s):
    if len(s) < 2:
        return s
    if s[0] == '"' and s[-1] == '"':
        slist = []
        esc = False
        for c in s[1:-1]:
            if not esc and c == '\\':
                esc = True
            elif esc and c == 'n':
                esc = False
                slist.append('\n')
            else:
                esc = False
                slist.append(c)
        return "".join(slist)
    else:
        return s

def requote(s):
    t = s.replace('\\', '\\\\')
    t = t.replace('"', '\\"')
    return '"' + t + '"'

def new_line(new_text):
    global empty_last_line

    if new_text != '':
        if new_text[-1] != '\n':
            return '\n'
    elif not empty_last_line:
        return '\n'
    return ''

def make_keys(lst):
    keys = {}
    for i in range(len(lst)):
        if i < len(lst)-1 and lst[i][0] == ':':
            keys[lst[i]] = unquote( lst[i+1] )
    return keys

def parse_plist(lst, keyword):
    for i in range(0, len(lst), 2):
        if keyword == lst[i]:
            return unquote(lst[i+1])
    return ''

def parse_filepos(fname, loc):
    lnum = 1
    cnum = 1
    pos = loc
    try:
        f = open(fname, "r")
    except:
        return [0, 0]
    for line in f:
        if pos < len(line):
            cnum = pos
            break
        pos = pos - len(line)
        lnum = lnum + 1
    f.close()
    return [lnum, cnum]

def format_filename(fname):
    fname = vim.eval('fnamemodify(' + fname + ', ":~:.")')
    if fname.find(' ') >= 0:
        fname = '"' + fname + '"'
    return fname

def parse_location(lst):
    fname = ''
    line  = ''
    pos   = ''
    if lst[0] == ':location':
        if type(lst[1]) == str:
            return unquote(lst[1])
        for l in lst[1:]:
            if l[0] == ':file':
                fname = l[1]
            if l[0] == ':line':
                line = l[1]
            if l[0] == ':position':
                pos = l[1]
        if fname == '':
            fname = 'Unknown file'
        if line != '':
            return 'in ' + format_filename(fname) + ' line ' + line
        if pos != '':
            [lnum, cnum] = parse_filepos(unquote(fname), int(pos))
            if lnum > 0:
                return 'in ' + format_filename(fname) + ' line ' + str(lnum)
            else:
                return 'in ' + format_filename(fname) + ' byte ' + pos
    return 'no source line information'

def unicode_len(text):
    if use_unicode:
        if sys.version_info[0] > 2:
            return len(str(text))
        else:
            return len(unicode(text, "utf-8"))
    else:
        if sys.version_info[0] > 2:
            return len(text.encode('utf-8'))
        else:
            return len(text)

def swank_send(text):
    global sock

    logtime('[---Sent---]')
    logprint(text)
    l = "%06x" % unicode_len(text)
    t = l + text
    if debug:
        print( 'Sending:', t)
    try:
        if sys.version_info[0] > 2:
            sock.send(t.encode('utf-8'))
        else:
            sock.send(t)
    except socket.error:
        vim.command("let s:swank_result='Socket error when sending to SWANK server.\n'")
        swank_disconnect()

def swank_recv_len(timeout):
    global sock

    rec = ''
    sock.setblocking(0)
    ready = select.select([sock], [], [], timeout)
    if ready[0]:
        l = lenbytes
        sock.setblocking(1)
        try:
            data = sock.recv(l)
        except socket.error:
            vim.command("let s:swank_result='Socket error when receiving from SWANK server.\n'")
            swank_disconnect()
            return rec
        while data and len(rec) < lenbytes:
            if sys.version_info[0] > 2:
                rec = rec + data.decode('utf-8')
            else:
                rec = rec + data
            l = l - len(data)
            if l > 0:
                try:
                    data = sock.recv(l)
                except socket.error:
                    vim.command("let s:swank_result='Socket error when receiving from SWANK server.\n'")
                    swank_disconnect()
                    return rec
    return rec

def swank_recv(msglen, timeout):
    global sock

    if msglen > 0:
        sock.setblocking(0)
        ready = select.select([sock], [], [], timeout)
        if ready[0]:
            sock.setblocking(1)
            rec = ''
            while True:
                # Each codepoint has at least 1 byte; so we start with the
                # number of bytes, and read more if needed.
                try:
                    needed = msglen - unicode_len(rec)
                except UnicodeDecodeError:
                    # Add single bytes until we've got valid UTF-8 again
                    needed = max(msglen - len(rec), 1)
                if needed == 0:
                    return rec
                try:
                    data = sock.recv(needed)
                except socket.error:
                    vim.command("let s:swank_result='Socket error when receiving from SWANK server.\n'")
                    swank_disconnect()
                    return rec
                if len(data) == 0:
                    vim.command("let s:swank_result='Socket error when receiving from SWANK server.\n'")
                    swank_disconnect()
                    return rec
                if sys.version_info[0] > 2:
                    rec = rec + data.decode('utf-8')
                else:
                    rec = rec + data
    rec = ''

def swank_parse_inspect_content(pcont):
    """
    Parse the swank inspector content
    """
    global inspect_lines
    global inspect_newline

    if type(pcont[0]) != list:
        return
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    help_lines = int( vim.eval('exists("b:help_shown") ? len(b:help) : 1') )
    pos = help_lines + inspect_lines
    buf[pos:] = []
    istate = pcont[1]
    start  = pcont[2]
    end    = pcont[3]
    lst = []
    for el in pcont[0]:
        logprint(str(el))
        newline = False
        if type(el) == list:
            if el[0] == ':action':
                text = '{<' + unquote(el[2]) + '> ' + unquote(el[1]) + ' <>}'
            else:
                text = '{[' + unquote(el[2]) + '] ' + unquote(el[1]) + ' []}'
            lst.append(text)
        else:
            text = unquote(el)
            lst.append(text)
            if text == "\n":
                newline = True
    lines = "".join(lst).split("\n")
    if inspect_newline or pos > len(buf):
        buf.append(lines)
    else:
        buf[pos-1] = buf[pos-1] + lines[0]
        buf.append(lines[1:])
    inspect_lines = len(buf) - help_lines
    inspect_newline = newline
    if int(istate) > int(end):
        # Swank returns end+1000 if there are more entries to request
        buf.append(['', "[--more--]", "[--all---]"])
    inspect_path = vim.eval('s:inspect_path')
    if len(inspect_path) > 1:
        buf.append(['', '[<<] Return to ' + ' -> '.join(inspect_path[:-1])])
    else:
        buf.append(['', '[<<] Exit Inspector'])
    if int(istate) > int(end):
        # There are more entries to request
        # Save current range for the next request
        vim.command("let b:range_start=" + start)
        vim.command("let b:range_end=" + end)
        vim.command("let b:inspect_more=" + end)
    else:
        # No ore entries left
        vim.command("let b:inspect_more=0")
    vim.command('call SlimvEndUpdate()')

def swank_parse_inspect(struct):
    """
    Parse the swank inspector output
    """
    global inspect_lines
    global inspect_newline

    vim.command('call SlimvBeginUpdate()')
    vim.command('call SlimvOpenInspectBuffer()')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    title = parse_plist(struct, ':title')
    vim.command('let b:inspect_title="' + title + '"')
    buf[:] = ['Inspecting ' + title, '--------------------', '']
    vim.command('normal! 3G0')
    vim.command('call SlimvHelp(2)')
    pcont = parse_plist(struct, ':content')
    inspect_lines = 3
    inspect_newline = True
    swank_parse_inspect_content(pcont)
    vim.command('call SlimvSetInspectPos("' + title + '")')

def swank_parse_debug(struct):
    """
    Parse the SLDB output
    """
    vim.command('call SlimvBeginUpdate()')
    vim.command('call SlimvOpenSldbBuffer()')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    [thread, level, condition, restarts, frames, conts] = struct[1:7]
    buf[:] = [l for l in (unquote(condition[0]) + "\n" + unquote(condition[1])).splitlines()]
    buf.append(['', 'Restarts:'])
    for i in range( len(restarts) ):
        r0 = unquote( restarts[i][0] )
        r1 = unquote( restarts[i][1] )
        r1 = r1.replace("\n", " ")
        buf.append([str(i).rjust(3) + ': [' + r0 + '] ' + r1])
    buf.append(['', 'Backtrace:'])
    for f in frames:
        frame = str(f[0])
        ftext = unquote( f[1] )
        ftext = ftext.replace('\n', '')
        ftext = ftext.replace('\\\\n', '')
        buf.append([frame.rjust(3) + ': ' + ftext])
    vim.command('call SlimvEndUpdate()')
    vim.command("call search('^Restarts:', 'w')")
    vim.command('stopinsert')
    # This text will be printed into the REPL buffer
    return unquote(condition[0]) + "\n" + unquote(condition[1]) + "\n"

def swank_parse_xref(struct):
    """
    Parse the swank xref output
    """
    buf = ''
    for e in struct:
        buf = buf + unquote(e[0]) + ' - ' + parse_location(e[1]) + '\n'
    return buf

def swank_parse_compile(struct):
    """
    Parse compiler output
    """
    buf = ''
    warnings = struct[1]
    time = struct[3]
    filename = ''
    if len(struct) > 5:
        filename = struct[5]
    if filename == '' or filename[0] != '"':
        filename = '"' + filename + '"'
    vim.command('let s:compiled_file=' + filename + '')
    vim.command("let qflist = []")
    if type(warnings) == list:
        buf = '\n' + str(len(warnings)) + ' compiler notes:\n\n'
        for w in warnings:
            msg      = parse_plist(w, ':message')
            severity = parse_plist(w, ':severity')
            if severity[0] == ':':
                severity = severity[1:]
            location = parse_plist(w, ':location')
            if location[0] == ':error':
                # "no error location available"
                buf = buf + '  ' + unquote(location[1]) + '\n'
                buf = buf + '  ' + severity + ': ' + msg + '\n\n'
            else:
                fname   = unquote(location[1][1])
                pos     = location[2][1]
                if location[3] != 'nil':
                    snippet = unquote(location[3][1]).replace('\r', '')
                    buf = buf + snippet + '\n'
                buf = buf + fname + ':' + pos + '\n'
                buf = buf + '  ' + severity + ': ' + msg + '\n\n'
                if location[2][0] == ':line':
                    lnum = pos
                    cnum = 1
                else:
                    [lnum, cnum] = parse_filepos(fname, int(pos))
                msg = msg.replace("'", "' . \"'\" . '")
                qfentry = "{'filename':'"+fname+"','lnum':'"+str(lnum)+"','col':'"+str(cnum)+"','text':'"+msg+"'}"
                logprint(qfentry)
                vim.command("call add(qflist, " + qfentry + ")")
    else:
        buf = '\nCompilation finished. (No warnings)  [' + time + ' secs]\n\n'
    vim.command("call setqflist(qflist)")
    return buf

def swank_parse_list_threads(tl):
    vim.command('call SlimvBeginUpdate()')
    vim.command('call SlimvOpenThreadsBuffer()')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    buf[:] = ['Threads in pid '+pid, '--------------------']
    vim.command('call SlimvHelp(2)')
    buf.append(['', 'Idx  ID    Status                 Name                   Priority', \
                    '---- ----  --------------------   --------------------   ---------'])
    vim.command('normal! G0')
    lst = tl[1]
    headers = lst.pop(0)
    logprint(str(lst))
    idx = 0
    for t in lst:
        priority = ''
        if len(t) > 3:
            priority = unquote(t[3])
        buf.append(["%3d:  %3d  %-22s %-22s %s" % (idx, int(t[0]), unquote(t[2]), unquote(t[1]), priority)])
        idx = idx + 1
    vim.command('normal! j')
    vim.command('call SlimvEndUpdate()')

def swank_parse_frame_call(struct, action):
    """
    Parse frame call output
    """
    vim.command('call SlimvGotoFrame(' + action.data + ')')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    win = vim.current.window
    line = win.cursor[0]
    if type(struct) == list:
        buf[line:line] = [struct[1][1]]
    else:
        buf[line:line] = ['No frame call information']
    vim.command('call SlimvEndUpdate()')

def swank_parse_frame_source(struct, action):
    """
    Parse frame source output
    http://comments.gmane.org/gmane.lisp.slime.devel/9961 ;-(
    'Well, let's say a missing feature: source locations are currently not available for code loaded as source.'
    """
    vim.command('call SlimvGotoFrame(' + action.data + ')')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    win = vim.current.window
    line = win.cursor[0]
    if type(struct) == list and len(struct) == 4:
        if struct[1] == 'nil':
            [lnum, cnum] = [int(struct[2][1]), 1]
            fname = 'Unknown file'
        else:
            [lnum, cnum] = parse_filepos(unquote(struct[1][1]), int(struct[2][1]))
            fname = format_filename(struct[1][1])
        if lnum > 0:
            s = '      in ' + fname + ' line ' + str(lnum)
        else:
            s = '      in ' + fname + ' byte ' + struct[2][1]
        slines = s.splitlines()
        if len(slines) > 2:
            # Make a fold (closed) if there are too many lines
            slines[ 0] = slines[ 0] + '{{{'
            slines[-1] = slines[-1] + '}}}'
            buf[line:line] = slines
            vim.command(str(line+1) + 'foldclose')
        else:
            buf[line:line] = slines
    else:
        buf[line:line] = ['      No source line information']
    vim.command('call SlimvEndUpdate()')

def swank_parse_locals(struct, action):
    """
    Parse frame locals output
    """
    frame_num = action.data
    vim.command('call SlimvGotoFrame(' + frame_num + ')')
    vim.command('setlocal modifiable')
    buf = vim.current.buffer
    win = vim.current.window
    line = win.cursor[0]
    if type(struct) == list:
        lines = '    Locals:'
        num = 0
        for f in struct:
            name  = parse_plist(f, ':name')
            id    = parse_plist(f, ':id')
            value = parse_plist(f, ':value')
            lines = lines + '\n      ' + name + ' = ' + value
            # Remember variable index in frame
            frame_locals[str(frame_num) + " " + name] = num
            num = num + 1
    else:
        lines = '    No locals'
    buf[line:line] = lines.split("\n")
    vim.command('call SlimvEndUpdate()')

def swank_listen():
    global output_port
    global use_unicode
    global debug_active
    global debug_activated
    global read_string
    global empty_last_line
    global current_thread
    global prompt
    global package
    global pid
    global swank_version
    global swank_param

    retval = ''
    msgcount = 0
    #logtime('[- Listen--]')
    timeout = recv_timeout
    while msgcount < maxmessages:
        rec = swank_recv_len(timeout)
        if rec == '':
            break
        timeout = 0.0
        msgcount = msgcount + 1
        if debug:
            print('swank_recv_len received', rec)
        msglen = int(rec, 16)
        if debug:
            print('Received length:', msglen)
        if msglen > 0:
            # length already received so it must be followed by data
            # use a higher timeout
            rec = swank_recv(msglen, 1.0)
            logtime('[-Received-]')
            logprint(rec)
            if vim.eval('exists("g:slimv_strip_ansi") && g:slimv_strip_ansi') != '0':
                # strip ANSI escape sequences from output string
                pattern = re.compile( r'(\x9B|\x1B\[)[0-?]*[ -\/]*[@-~]' )
                rec = pattern.sub( '', rec )
            [s, r] = parse_sexpr( rec )
            if debug:
                print('Parsed:', r)
            if len(r) > 0:
                r_id = r[-1]
                message = r[0].lower()
                if debug:
                    print('Message:', message)

                if message == ':open-dedicated-output-stream':
                    output_port = int( r[1].lower(), 10 )
                    if debug:
                        print(':open-dedicated-output-stream result:', output_port)
                    break

                elif message == ':presentation-start':
                    retval = retval + new_line(retval)

                elif message == ':write-string':
                    # REPL has new output to display
                    if len(r) > 2 and r[2] == ':repl-result':
                        retval = retval + new_line(retval)
                    retval = retval + unquote(r[1])
                    add_prompt = True
                    for k,a in actions.items():
                        if a.pending and a.name.find('eval') >= 0:
                            add_prompt = False
                            break
                    if add_prompt:
                        retval = retval + new_line(retval) + get_prompt()

                elif message == ':read-string':
                    # REPL requests entering a string
                    read_string = r[1:3]
                    vim.command('let s:read_string_mode=1')

                elif message == ':read-from-minibuffer':
                    # REPL requests entering a string in the command line
                    read_string = r[1:3]
                    vim.command('let s:read_string_mode=1')
                    vim.command("let s:input_prompt='%s'" % unquote(r[3]).replace("'", "''"))

                elif message == ':indentation-update':
                    for el in r[1]:
                        indent_info[ unquote(el[0]) ] = el[1]

                elif message == ':new-package':
                    package = unquote( r[1] )
                    prompt  = unquote( r[2] )

                elif message == ':return':
                    read_string = None
                    vim.command('let s:read_string_mode=0')
                    if len(r) > 1:
                        result = r[1][0].lower()
                    else:
                        result = ""
                    if type(r_id) == str and r_id in actions:
                        action = actions[r_id]
                        action.pending = False
                    else:
                        action = None
                    if log:
                        logtime('[Actionlist]')
                        for k,a in sorted(actions.items()):
                            if a.pending:
                                pending = 'pending '
                            else:
                                pending = 'finished'
                            logprint("%s: %s %s %s" % (k, str(pending), a.name, a.result))

                    if result == ':ok':
                        params = r[1][1]
                        logprint('params: ' + str(params))
                        if params == []:
                            params = 'nil'
                        if type(params) == str:
                            element = params.lower()
                            to_ignore = [':frame-call', ':quit-inspector', ':kill-thread', ':debug-thread']
                            to_nodisp = [':describe-symbol']
                            to_prompt = [':undefine-function', ':swank-macroexpand-1', ':swank-macroexpand-all', ':disassemble-form', \
                                         ':load-file', ':toggle-profile-fdefinition', ':profile-by-substring', ':swank-toggle-trace', 'sldb-break']
                            if action and action.name in to_ignore:
                                # Just ignore the output for this message
                                pass
                            elif element == 'nil' and action and action.name == ':inspector-pop':
                                # Quit inspector
                                vim.command('call SlimvQuitInspect(0)')
                            elif element != 'nil' and action and action.name in to_nodisp:
                                # Do not display output, just store it in actions
                                action.result = unquote(params)
                            else:
                                retval = retval + new_line(retval)
                                if element != 'nil':
                                    retval = retval + unquote(params)
                                    if action:
                                        action.result = retval
                                vim.command("let s:swank_ok_result='%s'" % retval.replace("'", "''").replace("\0", "^@"))
                                if element == 'nil' or (action and action.name in to_prompt):
                                    # No more output from REPL, write new prompt
                                    retval = retval + new_line(retval) + get_prompt()

                        elif type(params) == list and params:
                            element = ''
                            if type(params[0]) == str:
                                element = params[0].lower()
                            if element == ':present':
                                # No more output from REPL, write new prompt
                                retval = retval + new_line(retval) + unquote(params[1][0][0]) + '\n' + get_prompt()
                            elif element == ':values':
                                retval = retval + new_line(retval)
                                if type(params[1]) == list:
                                    retval = retval + unquote(params[1][0]) + '\n'
                                else:
                                    retval = retval + unquote(params[1]) + '\n' + get_prompt()
                            elif element == ':suppress-output':
                                pass
                            elif element == ':pid':
                                conn_info = make_keys(params)
                                pid = conn_info[':pid']
                                swank_version = conn_info.get(':version', 'nil')
                                if len(swank_version) == 8:
                                    # Convert version to YYYY-MM-DD format
                                    swank_version = swank_version[0:4] + '-' + swank_version[4:6] + '-' + swank_version[6:8]
                                imp = make_keys( conn_info[':lisp-implementation'] )
                                pkg = make_keys( conn_info[':package'] )
                                package = pkg[':name']
                                prompt = pkg[':prompt']
                                vim.command('let s:swank_version="' + swank_version + '"')
                                if len(swank_version) < 8 or swank_version >= '2011-11-08':
                                    # Recent swank servers count bytes instead of unicode characters
                                    use_unicode = False
                                vim.command('let s:lisp_version="' + imp[':version'] + '"')
                                retval = retval + new_line(retval)
                                retval = retval + imp[':type'] + ' ' + imp[':version'] + '  Port: ' + str(input_port) + '  Pid: ' + pid + '\n; SWANK ' + swank_version
                                retval = retval + '\n' + get_prompt()
                                logprint(' Package:' + package + ' Prompt:' + prompt)
                            elif element == ':name':
                                keys = make_keys(params)
                                retval = retval + new_line(retval)
                                retval = retval + '  ' + keys[':name'] + ' = ' + keys[':value'] + '\n'
                            elif element == ':title':
                                swank_parse_inspect(params)
                            elif element == ':compilation-result':
                                retval = retval + new_line(retval) + swank_parse_compile(params) + get_prompt()
                            else:
                                if action.name == ':simple-completions':
                                    if type(params[0]) == list and len(params[0]) > 0 and type(params[0][0]) == str and params[0][0] != 'nil':
                                        compl = "\n".join(params[0])
                                        retval = retval + compl.replace('"', '')
                                elif action.name == ':fuzzy-completions':
                                    if type(params[0]) == list and type(params[0][0]) == list:
                                        compl = "\n".join(map(lambda x: x[0], params[0]))
                                        retval = retval + compl.replace('"', '')
                                elif action.name == ':find-definitions-for-emacs':
                                    tags_file = vim.eval("g:slimv_tags_file")
                                    maxwidth = int(vim.eval("&columns")) - 16
                                    temp = open(tags_file, 'w')
                                    myitems = [[elem[1][1][1], elem[1][2][1], elem[1][3][1], elem[0]]
                                               for elem in params
                                                   if type(elem) == list and type(elem[1]) == list and elem[1][0] == ':location']
                                    for i in myitems:
                                        temp.write(swank_param)
                                        temp.write('\t')
                                        temp.write(i[0].replace('"', ''))
                                        cmd = ":go %s" % i[1]
                                        if i[2][0] == '"':
                                            # swank provided a code snippet too
                                            cmd = cmd + ' " ' + ' '.join(unquote(i[2]).split())
                                        elif i[3][0] == '"':
                                            # no code snippet, print location name
                                            cmd = cmd + ' " ' + ' '.join(unquote(i[3]).split())
                                        temp.write("\t%s\n" % cmd[:maxwidth])
                                    temp.close()
                                    retval = swank_param
                                elif action.name == ':list-threads':
                                    swank_parse_list_threads(r[1])
                                elif action.name == ':xref':
                                    retval = retval + '\n' + swank_parse_xref(r[1][1])
                                    retval = retval + new_line(retval) + get_prompt()
                                elif action.name == ':set-package':
                                    package = unquote(params[0])
                                    prompt = unquote(params[1])
                                    retval = retval + '\n' + get_prompt()
                                elif action.name == ':untrace-all':
                                    retval = retval + '\nUntracing:'
                                    for f in params:
                                        retval = retval + '\n' + '  ' + f
                                    retval = retval + '\n' + get_prompt()
                                elif action.name == ':frame-call':
                                    swank_parse_frame_call(params, action)
                                elif action.name == ':frame-source-location':
                                    swank_parse_frame_source(params, action)
                                elif action.name == ':frame-locals-and-catch-tags':
                                    swank_parse_locals(params[0], action)
                                elif action.name == ':profiled-functions':
                                    retval = retval + '\n' + 'Profiled functions:\n'
                                    for f in params:
                                        retval = retval + '  ' + f + '\n'
                                    retval = retval + get_prompt()
                                elif action.name == ':inspector-range':
                                    swank_parse_inspect_content(params)
                                if action:
                                    action.result = retval

                    elif result == ':abort':
                        debug_active = False
                        vim.command('let s:sldb_level=-1')
                        if len(r[1]) > 1:
                            retval = retval + '; Evaluation aborted on ' + unquote(r[1][1]).replace('\n', '\n;') + '\n' + get_prompt()
                        else:
                            retval = retval + '; Evaluation aborted\n' + get_prompt()

                elif message == ':inspect':
                    swank_parse_inspect(r[1])

                elif message == ':debug':
                    retval = retval + swank_parse_debug(r)

                elif message == ':debug-activate':
                    debug_active = True
                    debug_activated = True
                    current_thread = r[1]
                    sldb_level = r[2]
                    vim.command('let s:sldb_level=' + sldb_level)
                    frame_locals.clear()

                elif message == ':debug-return':
                    debug_active = False
                    vim.command('let s:sldb_level=-1')
                    retval = retval + '; Quit to level ' + r[2] + '\n' + get_prompt()

                elif message == ':ping':
                    [thread, tag] = r[1:3]
                    swank_send('(:emacs-pong ' + thread + ' ' + tag + ')')
    if retval != '':
        empty_last_line = (retval[-1] == '\n')
    return retval

def swank_rex(action, cmd, package, thread, data=''):
    """
    Send an :emacs-rex command to SWANK
    """
    global id
    id = id + 1
    key = str(id)
    actions[key] = swank_action(key, action, data)
    form = '(:emacs-rex ' + cmd + ' ' + package + ' ' + thread + ' ' + str(id) + ')\n'
    swank_send(form)

def get_package():
    """
    Package set by slimv.vim or nil
    """
    pkg = vim.eval("s:swank_package")
    if pkg == '':
        return 'nil'
    else:
        return requote(pkg)

def get_swank_package():
    """
    Package set by slimv.vim or current swank package
    """
    pkg = vim.eval("s:swank_package")
    if pkg == '':
        return requote(package)
    else:
        return requote(pkg)

def get_indent_info(name):
    indent = ''
    if name in indent_info:
        indent = indent_info[name]
    vc = ":let s:indent='" + indent + "'"
    vim.command(vc)

###############################################################################
# Various SWANK messages
###############################################################################

def swank_connection_info():
    global log
    actions.clear()
    indent_info.clear()
    frame_locals.clear()
    debug_activated = False
    secret_file = os.path.expanduser('~') + '/.slime-secret'
    if os.path.exists(secret_file):
        # If the user has a .slime-secret file then the connection must start by sending the password
        with open(secret_file) as f:
            secret = f.readline().rstrip('\n')
            log = False
            swank_send(secret)
    if vim.eval('exists("g:swank_log") && g:swank_log') != '0':
        log = True
    swank_rex(':connection-info', '(swank:connection-info)', 'nil', 't')

def swank_create_repl():
    global swank_version
    if len(swank_version) < 8 or swank_version >= '2014-10-01':
        swank_rex(':create-repl', '(swank-repl:create-repl nil)', get_swank_package(), 't')
    else:
        swank_rex(':create-repl', '(swank:create-repl nil)', get_swank_package(), 't')

def swank_eval(exp):
    if len(swank_version) < 8 or swank_version >= '2014-10-01':
        cmd = '(swank-repl:listener-eval ' + requote(exp) + ')'
    else:
        cmd = '(swank:listener-eval ' + requote(exp) + ')'
    swank_rex(':listener-eval', cmd, get_swank_package(), ':repl-thread')

def swank_eval_in_frame(exp, n):
    pkg = get_swank_package()
    if len(swank_version) < 8 or swank_version >= '2011-11-21':
        cmd = '(swank:eval-string-in-frame ' + requote(exp) + ' ' + str(n) + ' ' + pkg + ')'
    else:
        cmd = '(swank:eval-string-in-frame ' + requote(exp) + ' ' + str(n) + ')'
    swank_rex(':eval-string-in-frame', cmd, pkg, current_thread, str(n))

def swank_pprint_eval(exp):
    cmd = '(swank:pprint-eval ' + requote(exp) + ')'
    swank_rex(':pprint-eval', cmd, get_swank_package(), ':repl-thread')

def swank_interrupt():
    swank_send('(:emacs-interrupt :repl-thread)')

def swank_invoke_restart(level, restart):
    cmd = '(swank:invoke-nth-restart-for-emacs ' + level + ' ' + restart + ')'
    swank_rex(':invoke-nth-restart-for-emacs', cmd, 'nil', current_thread, restart)

def swank_throw_toplevel():
    swank_rex(':throw-to-toplevel', '(swank:throw-to-toplevel)', 'nil', current_thread)

def swank_invoke_abort():
    swank_rex(':sldb-abort', '(swank:sldb-abort)', 'nil', current_thread)

def swank_invoke_continue():
    swank_rex(':sldb-continue', '(swank:sldb-continue)', 'nil', current_thread)

def swank_step_into(frame):
    cmd = '(swank:sldb-step ' + frame + ')'
    swank_rex(':sldb-step', cmd, 'nil', current_thread)

def swank_step_next(frame):
    cmd = '(swank:sldb-next ' + frame + ')'
    swank_rex(':sldb-next', cmd, 'nil', current_thread)

def swank_step_out(frame):
    cmd = '(swank:sldb-out ' + frame + ')'
    swank_rex(':sldb-out', cmd, 'nil', current_thread)

def swank_require(contrib):
    cmd = "(swank:swank-require '" + contrib + ')'
    swank_rex(':swank-require', cmd, 'nil', 't')

def swank_frame_call(frame):
    cmd = '(swank-backend:frame-call ' + frame + ')'
    swank_rex(':frame-call', cmd, 'nil', current_thread, frame)

def swank_frame_source_loc(frame):
    cmd = '(swank:frame-source-location ' + frame + ')'
    swank_rex(':frame-source-location', cmd, 'nil', current_thread, frame)

def swank_frame_locals(frame):
    cmd = '(swank:frame-locals-and-catch-tags ' + frame + ')'
    swank_rex(':frame-locals-and-catch-tags', cmd, 'nil', current_thread, frame)

def swank_restart_frame(frame):
    cmd = '(swank-backend:restart-frame ' + frame + ')'
    swank_rex(':restart-frame', cmd, 'nil', current_thread, frame)

def swank_set_package(pkg):
    cmd = '(swank:set-package "' + pkg + '")'
    swank_rex(':set-package', cmd, get_package(), ':repl-thread')

def swank_describe_symbol(fn):
    cmd = '(swank:describe-symbol "' + fn + '")'
    swank_rex(':describe-symbol', cmd, get_package(), 't')

def swank_describe_function(fn):
    cmd = '(swank:describe-function "' + fn + '")'
    swank_rex(':describe-function', cmd, get_package(), 't')

def swank_op_arglist(op):
    pkg = get_swank_package()
    cmd = '(swank:operator-arglist "' + op + '" ' + pkg + ')'
    swank_rex(':operator-arglist', cmd, pkg, 't')

def swank_completions(symbol):
    cmd = '(swank:simple-completions "' + symbol + '" ' + get_swank_package() + ')'
    swank_rex(':simple-completions', cmd, 'nil', 't')

def swank_fuzzy_completions(symbol):
    cmd = '(swank:fuzzy-completions "' + symbol + '" ' + get_swank_package() + ' :limit 2000 :time-limit-in-msec 2000)'
    swank_rex(':fuzzy-completions', cmd, 'nil', 't')

def swank_undefine_function(fn):
    cmd = '(swank:undefine-function "' + fn + '")'
    swank_rex(':undefine-function', cmd, get_package(), 't')

def swank_return_string(s):
    global read_string
    swank_send('(:emacs-return-string ' + read_string[0] + ' ' + read_string[1] + ' ' + requote(s) + ')')
    read_string = None
    vim.command('let s:read_string_mode=0')

def swank_return(s):
    global read_string
    if s != '':
        swank_send('(:emacs-return ' + read_string[0] + ' ' + read_string[1] + ' "' + s + '")')
    read_string = None
    vim.command('let s:read_string_mode=0')

def swank_inspect(symbol):
    global inspect_package
    cmd = '(swank:init-inspector "' + symbol + '")'
    inspect_package = get_swank_package()
    swank_rex(':init-inspector', cmd, inspect_package, 't')

def swank_inspect_nth_part(n):
    cmd = '(swank:inspect-nth-part ' + str(n) + ')'
    swank_rex(':inspect-nth-part', cmd, get_swank_package(), 't', str(n))

def swank_inspector_nth_action(n):
    cmd = '(swank:inspector-call-nth-action ' + str(n) + ')'
    swank_rex(':inspector-call-nth-action', cmd, 'nil', 't', str(n))

def swank_inspector_pop():
    # Remove the last entry from the inspect path
    vim.command('let s:inspect_path = s:inspect_path[:-2]')
    swank_rex(':inspector-pop', '(swank:inspector-pop)', 'nil', 't')

def swank_inspect_in_frame(symbol, n):
    key = str(n) + " " + symbol
    if key in frame_locals:
        cmd = '(swank:inspect-frame-var ' + str(n) + " " + str(frame_locals[key]) + ')'
    else:
        cmd = '(swank:inspect-in-frame "' + symbol + '" ' + str(n) + ')'
    swank_rex(':inspect-in-frame', cmd, get_swank_package(), current_thread, str(n))

def swank_inspector_range():
    start = int(vim.eval("b:range_start"))
    end   = int(vim.eval("b:range_end"))
    cmd = '(swank:inspector-range ' + str(end) + " " + str(end+(end-start)) + ')'
    swank_rex(':inspector-range', cmd, inspect_package, 't')

def swank_quit_inspector():
    global inspect_package
    swank_rex(':quit-inspector', '(swank:quit-inspector)', 'nil', 't')
    inspect_package = ''

def swank_break_on_exception(flag):
    if flag:
        swank_rex(':break-on-exception', '(swank:break-on-exception "true")', 'nil', current_thread)
    else:
        swank_rex(':break-on-exception', '(swank:break-on-exception "false")', 'nil', current_thread)

def swank_set_break(symbol):
    cmd = '(swank:sldb-break "' + symbol + '")'
    swank_rex(':sldb-break', cmd, get_package(), 't')

def swank_toggle_trace(symbol):
    cmd = '(swank:swank-toggle-trace "' + symbol + '")'
    swank_rex(':swank-toggle-trace', cmd, get_package(), 't')

def swank_untrace_all():
    swank_rex(':untrace-all', '(swank:untrace-all)', 'nil', 't')

def swank_macroexpand(formvar):
    form = vim.eval(formvar)
    cmd = '(swank:swank-macroexpand-1 ' + requote(form) + ')'
    swank_rex(':swank-macroexpand-1', cmd, get_package(), 't')

def swank_macroexpand_all(formvar):
    form = vim.eval(formvar)
    cmd = '(swank:swank-macroexpand-all ' + requote(form) + ')'
    swank_rex(':swank-macroexpand-all', cmd, get_package(), 't')

def swank_disassemble(symbol):
    cmd = '(swank:disassemble-form "' + "'" + symbol + '")'
    swank_rex(':disassemble-form', cmd, get_package(), 't')

def swank_xref(fn, type):
    cmd = "(swank:xref '" + type + " '" + '"' + fn + '")'
    swank_rex(':xref', cmd, get_package(), 't')

def swank_compile_string(formvar):
    form = vim.eval(formvar)
    filename = vim.eval("substitute( expand('%:p'), '\\', '/', 'g' )")
    line = vim.eval("line('.')")
    pos = vim.eval("line2byte(line('.'))")
    if vim.eval("&fileformat") == 'dos':
        # Remove 0x0D, keep 0x0A characters
        pos = str(int(pos) - int(line) + 1)
    cmd = '(swank:compile-string-for-emacs ' + requote(form) + ' nil ' + "'((:position " + str(pos) + ") (:line " + str(line) + " 1)) " + requote(filename) + ' nil)'
    swank_rex(':compile-string-for-emacs', cmd, get_package(), 't')

def swank_compile_file(name):
    if vim.eval("exists('g:slimv_fasl_directory')") != '0':
        fasl_directory = vim.eval('g:slimv_fasl_directory')
        if not fasl_directory.endswith('/'):
            fasl_directory += '/'
        fasl_directory_arg = ' :fasl-directory ' + requote(fasl_directory)
    else:
        fasl_directory_arg = ''
    cmd = '(swank:compile-file-for-emacs ' + requote(name) + ' t' + fasl_directory_arg + ')'
    swank_rex(':compile-file-for-emacs', cmd, get_package(), 't')

def swank_load_file(name):
    cmd = '(swank:load-file ' + requote(name) + ')'
    swank_rex(':load-file', cmd, get_package(), 't')

def swank_toggle_profile(symbol):
    cmd = '(swank:toggle-profile-fdefinition "' + symbol + '")'
    swank_rex(':toggle-profile-fdefinition', cmd, get_package(), 't')

def swank_profile_substring(s, package):
    if package == '':
        p = 'nil'
    else:
        p = requote(package)
    cmd = '(swank:profile-by-substring ' + requote(s) + ' ' + p + ')'
    swank_rex(':profile-by-substring', cmd, get_package(), 't')

def swank_unprofile_all():
    swank_rex(':unprofile-all', '(swank:unprofile-all)', 'nil', 't')

def swank_profiled_functions():
    swank_rex(':profiled-functions', '(swank:profiled-functions)', 'nil', 't')

def swank_profile_report():
    swank_rex(':profile-report', '(swank:profile-report)', 'nil', 't')

def swank_profile_reset():
    swank_rex(':profile-reset', '(swank:profile-reset)', 'nil', 't')

def swank_list_threads():
    cmd = '(swank:list-threads)'
    swank_rex(':list-threads', cmd, get_swank_package(), 't')

def swank_kill_thread(index):
    cmd = '(swank:kill-nth-thread ' + str(index) + ')'
    swank_rex(':kill-thread', cmd, get_swank_package(), 't', str(index))

def swank_find_definitions_for_emacs(str):
    global swank_param
    swank_param = str
    cmd = '(swank:find-definitions-for-emacs "' + str + '")'
    swank_rex(':find-definitions-for-emacs', cmd, get_package(), ':repl-thread')

def swank_debug_thread(index):
    cmd = '(swank:debug-nth-thread ' + str(index) + ')'
    swank_rex(':debug-thread', cmd, get_swank_package(), 't', str(index))

def swank_quit_lisp():
    swank_rex(':quit-lisp', '(swank:quit-lisp)', 'nil', 't')
    swank_disconnect()

###############################################################################
# Generic SWANK connection handling
###############################################################################

def swank_connect(host, port, resultvar):
    """
    Create socket to swank server and request connection info
    """
    global sock
    global input_port

    if not sock:
        try:
            input_port = port
            swank_server = (host, input_port)
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(swank_server)
            swank_connection_info()
            vim.command('let ' + resultvar + '=""')
            return sock
        except socket.error:
            vim.command('let ' + resultvar + '="SWANK server is not running."')
            sock = None
            return sock
    vim.command('let ' + resultvar + '=""')

def swank_disconnect():
    """
    Disconnect from swank server
    """
    global sock
    try:
        # Try to close socket but don't care if doesn't succeed
        sock.close()
    finally:
        sock = None
        vim.command('let s:swank_connected = 0')
        vim.command("let s:swank_result='Connection to SWANK server is closed.\n'")

def swank_input(formvar):
    global empty_last_line

    empty_last_line = True
    form = vim.eval(formvar)
    if read_string:
        # We are in :read-string mode, pass string entered to REPL
        swank_return_string(form)
    elif form[0] == '[':
        if form[1] == '-':
            swank_inspector_pop()
        else:
            swank_inspect_nth_part(form[1:-2])
    elif form[0] == '<':
        swank_inspector_nth_action(form[1:-2])
    else:
        # Normal s-expression evaluation
        swank_eval(form)

def actions_pending():
    count = 0
    for k,a in sorted(actions.items()):
        if a.pending:
            count = count + 1
    vc = ":let s:swank_actions_pending=" + str(count)
    vim.command(vc)
    return count

def append_repl(text, varname_given):
    """
    Append text at the end of the REPL buffer
    Does not bring REPL buffer into focus if loaded but not displayed in any window
    """
    repl_buf = int(vim.eval("s:repl_buf"))
    if repl_buf < 0 or int(vim.eval("buflisted(%d) && bufloaded(%d)" % (repl_buf, repl_buf))) == 0:
        # No REPL buffer exists
        vim.command('call SlimvBeginUpdate()')
        vim.command('call SlimvOpenReplBuffer()')
        vim.command('call SlimvRestoreFocus(0)')
        repl_buf = int(vim.eval("s:repl_buf"))
    for buf in vim.buffers:
        if buf.number == repl_buf:
            break
    if repl_buf > 0 and buf.number == repl_buf:
        if varname_given:
            lines = vim.eval(text).split("\n")
        else:
            lines = text.split("\n")
        if lines[0] != '':
            # Concatenate first line to the last line of the buffer
            nlines = len(buf)
            buf[nlines-1] = buf[nlines-1] + lines[0]
        if len(lines) > 1:
            # Append all subsequent lines
            buf.append(lines[1:])

        # Keep only the last g:slimv_repl_max_len lines
        repl_max_len = int(vim.eval("g:slimv_repl_max_len"))
        repl_prompt_line = int(vim.eval("getbufvar(%d, 'repl_prompt_line')" % repl_buf))
        lastline = len(buf)
        prompt_offset = lastline - repl_prompt_line
        if repl_max_len > 0 and lastline > repl_max_len:
            form = "\n".join(buf[0:(lastline-repl_max_len)])
            ending = vim.eval("substitute(s:CloseForm('%s'), '\\n', '', 'g')" % form.replace("'", "''"))
            # Delete extra lines
            buf[0:(lastline - repl_max_len)] = []
            if ending.find(')') >= 0 or ending.find(']') >= 0 or ending.find(']') >= 0:
                # Reverse the ending and replace matched characters with their pairs
                start = ending[::-1]
                start = start.replace(')', '(').replace(']', '[').replace('}', '{').replace("\n", '')
                # Re-balance the beginning of the buffer
                buf[0:0] = [start + " .... ; output shortened"]
            vim.command("call setbufvar(%d, 'repl_prompt_line', %d)" % (repl_buf, len(buf) - prompt_offset))

        # Move cursor at the end of REPL buffer in case it was originally after the prompt
        vim.command('call SlimvReplSetCursorPos(0)')

def swank_output(echo):
    global sock
    global debug_active
    global debug_activated

    if not sock:
        return "SWANK server is not connected."
    count = 0
    #logtime('[- Output--]')
    debug_activated = False
    result = swank_listen()
    pending = actions_pending()
    while sock and result == '' and pending > 0 and count < listen_retries:
        result = swank_listen()
        pending = actions_pending()
        count = count + 1
    if echo and result != '':
        # Append SWANK output to REPL buffer
        append_repl(result, 0)
    if debug_activated and debug_active:
        # Debugger was activated in this run
        vim.command('call SlimvOpenSldbBuffer()')
        vim.command('call SlimvEndUpdate()')
        vim.command("call search('^Restarts:', 'w')")

def swank_response(name):
    #logtime('[-Response-]')
    for k,a in sorted(actions.items()):
        if not a.pending and (name == '' or name == a.name):
            vc = ":let s:swank_action='" + a.name + "'"
            vim.command(vc)
            vim.command("let s:swank_result='%s'" % a.result.replace("'", "''"))
            actions.pop(a.id)
            actions_pending()
            return
    vc = ":let s:swank_action=''"
    vc = ":let s:swank_result=''"
    vim.command(vc)
    actions_pending()

