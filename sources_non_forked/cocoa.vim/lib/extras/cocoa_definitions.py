#!/usr/bin/python
'''Creates a folder containing text files of Cocoa keywords.'''
import os, commands, re
from sys import argv

def find(searchpath, ext):
    '''Mimics the "find searchpath -name *.ext" unix command.'''
    results = []
    for path, dirs, files in os.walk(searchpath):
        for filename in files:
            if filename.endswith(ext):
                results.append(os.path.join(path, filename))
    return results

def find_headers(root_folder, frameworks):
    '''Returns list of the header files for the given frameworks.'''
    headers = []
    folder = root_folder + '/System/Library/Frameworks/'
    for framework in frameworks:
        headers.extend(find(folder + framework + '.framework', '.h'))
    return headers

def default_headers():
    '''Headers for common Cocoa frameworks.'''
    cocoa_frameworks = ('Foundation', 'CoreFoundation', 'AppKit',
                        'AddressBook', 'CoreData', 'PreferencePanes', 'QTKit',
                        'ScreenSaver', 'SyncServices', 'WebKit')
    iphone_frameworks = ('UIKit', 'GameKit')
    iphone_sdk_path = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.1.sdk'
    return find_headers('', cocoa_frameworks) + \
           find_headers(iphone_sdk_path, iphone_frameworks)

def match_output(command, regex, group_num):
    '''
    Returns an ordered list of all matches of the supplied regex for the
    output of the given command.
    '''
    results = []
    for line in commands.getoutput(command).split("\n"):
        match = re.search(regex, line)
        if match and not match.group(group_num) in results:
            results.append(match.group(group_num))
    results.sort()
    return results

def get_functions(header_files):
    '''Returns list of Cocoa Functions.'''
    lines = match_output(r"grep -h '^[A-Z][A-Z_]* [^;]* \**\(NS\|UI\)\w\+ *(' "
                         + header_files, r'((NS|UI)\w+)\s*\(.*?\)', 1)
    lines = [format_function_line(line) for line in lines]
    lines += match_output(r"grep -h '^#define \(NS\|UI\)\w\+ *(' "
                          + header_files, r'((NS|UI)\w+)\s*\(.*?\)', 1)
    return lines

def format_function_line(line):
    # line = line.replace('NSInteger', 'int')
    # line = line.replace('NSUInteger', 'unsigned int')
    # line = line.replace('CGFloat', 'float')
    return re.sub(r'void(\s*[^*])', r'\1', line)

def get_types(header_files):
    '''Returns a list of Cocoa Types.'''
    return match_output(r"grep -h 'typedef .* _*\(NS\|UI\)[A-Za-z]*' "
                          + header_files, r'((NS|UI)[A-Za-z]+)\)?\s*(;|{)', 1)

def get_constants(header_files):
    '''Returns a list of Cocoa Constants.'''
    return match_output(r"awk '/^(typedef )?(enum|NS_(ENUM|OPTIONS)\(.*\)) .*\{/ {pr = 1;} /\}/ {pr = 0;}"
                        r"{ if(pr) print $0; }' " + header_files,
                        r'^\s*((NS|UI)[A-Z][A-Za-z0-9_]*)', 1)

def get_notifications(header_files):
    '''Returns a list of Cocoa Notifications.'''
    return match_output(r"egrep -h '\*(\s*const\s+)?\s*(NS|UI).*Notification' "
                        + header_files, r'(NS|UI)\w*Notification', 0)

def write_file(filename, lines):
    '''Attempts to write list to file or exits with error if it can't.'''
    try:
        f = open(filename, 'w')
    except IOError, error:
        raise SystemExit(argv[0] + ': %s' % error)
    f.write("\n".join(lines))
    f.close()

def extract_files_to(dirname=None):
    '''Extracts .txt files to given directory or ./cocoa_indexes by default.'''
    if dirname is None:
        dirname = './cocoa_indexes'
    if not os.path.isdir(dirname):
        os.mkdir(dirname)
    headers = ' '.join(default_headers())

    write_file(dirname + '/functions.txt',     get_functions    (headers))
    write_file(dirname + '/types.txt',         get_types        (headers))
    write_file(dirname + '/constants.txt',     get_constants    (headers))
    write_file(dirname + '/notifications.txt', get_notifications(headers))

if __name__ == '__main__':
    extract_files_to(argv[1] if len(argv) > 1 else None)
