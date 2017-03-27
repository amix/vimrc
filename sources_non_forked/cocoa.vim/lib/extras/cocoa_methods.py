#!/usr/bin/python
'''
Lists Cocoa methods in given file or ./cocoa_indexes/methods.txt by default.
'''
import re, os, gzip
from cocoa_definitions import default_headers, format_function_line

def get_methods(headers):
    '''Returns list of Cocoa methods.'''
    matches = []
    for header in headers:
        f = open(header, 'r')
        current_class = ''
        for line in f:
            if current_class == '':
                if line[:10] == '@interface' or line[:9] == '@protocol':
                    current_class = re.match('@(interface|protocol)\s+(\w+)',
                                             line).group(2)
            else:
                if line[:3] == '@end':
                    current_class = ''
                elif re.match('[-+]\s*\(', line):
                    method_name = get_method_name(line)
                    if method_name:
                        match = current_class + ' ' + method_name
                        if match not in matches:
                            matches.append(match)
        f.close()
    matches = [format_line(line) for line in matches]
    matches.sort()
    return matches

def get_method_name(line):
    '''Returns the method name & argument types for the given line.'''
    if re.search('\w+\s*:', line):
        return ' '.join(re.findall('\w+\s*:\s*\(.*?\)', line))
    else:
        return re.match(r'[-+]\s*\(.*?\)\s*(\w+)', line).group(1)

def format_line(line):
    '''Removes parentheses/comments/unnecessary spacing for the given line.'''
    line = re.sub(r'\s*:\s*', ':', line)
    line = re.sub(r'/\*.*?\*/\s*|[()]', '', line)
    line = re.sub(r'(NS\S+)Pointer', r'\1 *', line)
    return format_function_line(line)

def extract_file_to(fname=None):
    '''
    Extracts methods to given file or ./cocoa_indexes/methods.txt by default.
    '''
    if fname is None:
        fname = './cocoa_indexes/methods.txt.gz'
    if not os.path.isdir(os.path.dirname(fname)):
        os.mkdir(os.path.dirname(fname))

    # This file is quite large, so I've compressed it.
    f = gzip.open(fname, 'w')
    f.write("\n".join(get_methods(default_headers())))
    f.close()

if __name__ == '__main__':
    from sys import argv
    extract_file_to(argv[1] if len(argv) > 1 else None)
