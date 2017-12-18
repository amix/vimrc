#!/usr/bin/python
'''
Creates text file of Cocoa superclasses in given filename or in
./cocoa_indexes/classes.txt by default.
'''
import os, re
from cocoa_definitions import write_file, find
from commands import getoutput

# We need find_headers() to return a dictionary instead of a list
def find_headers(root_folder, frameworks):
    '''Returns a dictionary of the headers for each given framework.'''
    headers_and_frameworks = {}
    folder = root_folder + '/System/Library/Frameworks/'
    for framework in frameworks:
        bundle = folder + framework + '.framework'
        if os.path.isdir(bundle):
            headers_and_frameworks[framework] = ' '.join(find(bundle, '.h'))
    return headers_and_frameworks

def get_classes(header_files_and_frameworks):
    '''Returns list of Cocoa Protocols classes & their framework.'''
    classes = {}
    for framework, files in header_files_and_frameworks:
        for line in getoutput(r"grep -ho '@\(interface\|protocol\) [A-Z]\w\+' "
                                       + files).split("\n"):
            cocoa_class = re.search(r'[A-Z]\w+', line)
            if cocoa_class and not classes.has_key(cocoa_class.group(0)):
                classes[cocoa_class.group(0)] = framework
    classes = classes.items()
    classes.sort()
    return classes

def get_superclasses(classes_and_frameworks):
    '''
    Given a list of Cocoa classes & their frameworks, returns a list of their
    superclasses in the form: "class\|superclass\|superclass\|...".
    '''
    args = ''
    for classname, framework in classes_and_frameworks:
        args += classname + ' ' + framework + ' '
    return getoutput('./superclasses ' + args).split("\n")

def output_file(fname=None):
    '''Output text file of Cocoa classes to given filename.'''
    if fname is None:
        fname = './cocoa_indexes/classes.txt'
    if not os.path.isdir(os.path.dirname(fname)):
        os.mkdir(os.path.dirname(fname))

    cocoa_frameworks = ('Foundation', 'AppKit', 'AddressBook', 'CoreData',
                        'PreferencePanes', 'QTKit', 'ScreenSaver',
                        'SyncServices', 'WebKit')
    iphone_frameworks = ('UIKit', 'GameKit')
    iphone_sdk_path = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.1.sdk'
    headers_and_frameworks = find_headers('', cocoa_frameworks).items() + \
                             find_headers(iphone_sdk_path, iphone_frameworks).items()

    superclasses = get_superclasses(get_classes(headers_and_frameworks))
    write_file(fname, superclasses)

if __name__ == '__main__':
    from sys import argv
    output_file(argv[1] if len(argv) > 1 else None)
