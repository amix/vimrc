#!/usr/bin/python
'''Builds Vim syntax file for Cocoa keywords.'''
import sys, datetime
import cocoa_definitions

def usage():
    print 'usage: build_syntaxfile.py [outputfile]'
    return -1

def generate_syntax_file():
    '''Returns a list of lines for a Vim syntax file of Cocoa keywords.'''
    dir = './cocoa_indexes/'
    cocoa_definitions.extract_files_to(dir)

    # Normal classes & protocols need to be differentiated in syntax, so
    # we need to generate them again.
    headers = ' '.join(cocoa_definitions.default_headers())

    output = \
    ['" Description:    Syntax highlighting for the cocoa.vim plugin.',
    '"                 Adds highlighting for Cocoa keywords (classes, types, etc.).',
    '" Last Generated: ' + datetime.date.today().strftime('%B %d, %Y'),
    '']

    output += ['" Cocoa Functions',
               'syn keyword cocoaFunction containedin=objcMessage '
               + join_lines(read_file(dir + 'functions.txt')),
               '',
               '" Cocoa Classes',
               'syn keyword cocoaClass containedin=objcMessage '
               + join_lines(get_classes(headers)),
               '',
               '" Cocoa Protocol Classes',
               'syn keyword cocoaProtocol containedin=objcProtocol '
               + join_lines(get_protocol_classes(headers)),
               '',
               '" Cocoa Types',
               'syn keyword cocoaType containedin=objcMessage CGFloat '
               + join_lines(read_file(dir + 'types.txt')),
               '',
               '" Cocoa Constants',
               'syn keyword cocoaConstant containedin=objcMessage '
               + join_lines(read_file(dir + 'constants.txt')),
               '',
               '" Cocoa Notifications',
               'syn keyword cocoaNotification containedin=objcMessage '
               + join_lines(read_file(dir + 'notifications.txt')),
               '']

    output += ['hi link cocoaFunction Keyword',
               'hi link cocoaClass Special',
               'hi link cocoaProtocol cocoaClass',
               'hi link cocoaType Type',
               'hi link cocoaConstant Constant',
               'hi link cocoaNotification Constant']
    return output

def read_file(fname):
    '''Returns the lines as a string for the given filename.'''
    f = open(fname, 'r')
    lines = f.read()
    f.close()
    return lines

def join_lines(lines):
    '''Returns string of lines with newlines converted to spaces.'''
    if type(lines).__name__ == 'str':
        return lines.replace('\n', ' ')
    else:
        line = ([line[:-1] if line[-1] == '\n' else line for line in lines])
        return ' '.join(line)

def get_classes(header_files):
    '''Returns @interface classes.'''
    return cocoa_definitions.match_output("grep -ho '@interface \(NS\|UI\)[A-Za-z]*' "
                                          + header_files, '(NS|UI)\w+', 0)

def get_protocol_classes(header_files):
    '''Returns @protocol classes.'''
    return cocoa_definitions.match_output("grep -ho '@protocol \(NS\|UI\)[A-Za-z]*' "
                                          + header_files, '(NS|UI)\w+', 0)

def output_file(fname=None):
    '''Writes syntax entries to file or prints them if no file is given.'''
    if fname:
        cocoa_definitions.write_file(fname, generate_syntax_file())
    else:
        print "\n".join(generate_syntax_file())

if __name__ == '__main__':
    if '-h' in sys.argv or '--help' in sys.argv:
        sys.exit(usage())
    else:
        output_file(sys.argv[1] if len(sys.argv) > 1 else None)
