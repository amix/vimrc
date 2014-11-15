#!/usr/bin/env python

# Python script for fast text file searching using keyword index on disk.
#
# Author: Peter Odding <peter@peterodding.com>
# Last Change: September 2, 2013
# URL: http://peterodding.com/code/vim/notes/
# License: MIT
#
# This Python script can be used by the notes.vim plug-in to perform fast
# keyword searches in the user's notes. It has two advantages over just
# using Vim's internal :vimgrep command to search all of the user's notes:
# 
#  - Very large notes don't slow searching down so much;
#  - Hundreds of notes can be searched in less than a second.
#
# The keyword index is a Python dictionary that's persisted using the pickle
# module. The structure of the dictionary may seem very naive but it's quite
# fast. Also the pickle protocol makes sure repeating strings are stored only
# once, so it's not as bad as it may appear at first sight :-).
# 
# For more information about the Vim plug-in see http://peterodding.com/code/vim/notes/.

"""
Usage: search-notes.py [OPTIONS] KEYWORD...

Search one or more directories of plain text files using a full text index,
updated automatically during each invocation of the program.

Valid options include:

  -i, --ignore-case    ignore case of keyword(s)
  -l, --list=SUBSTR    list keywords matching substring
  -d, --database=FILE  set path to keywords index file
  -n, --notes=DIR      set directory with user notes (can be repeated)
  -e, --encoding=NAME  set character encoding of notes
  -v, --verbose        make more noise
  -h, --help           show this message and exit

For more information see http://peterodding.com/code/vim/notes/
"""

# Standard library modules.
import fnmatch
import getopt
import logging
import os
import re
import sys
import time

# Load the faster C variant of the pickle module where possible, but
# fall back to the Python implementation that's always available.
try:
  import cPickle as pickle
except ImportError:
  import pickle

# Try to import the Levenshtein module, don't error out if it's not installed.
try:
  import Levenshtein
  levenshtein_supported = True
except ImportError:
  levenshtein_supported = False

# The version of the index format that's supported by this revision of the
# `search-notes.py' script; if an existing index file is found with an
# unsupported version, the script knows that it should rebuild the index.
INDEX_VERSION = 2

class NotesIndex:

  def __init__(self):
    ''' Entry point to the notes search. '''
    global_timer = Timer()
    self.init_logging()
    keywords = self.parse_args()
    self.load_index()
    self.update_index()
    if self.dirty:
      self.save_index()
    print "Python works fine!"
    if self.keyword_filter is not None:
      self.list_keywords(self.keyword_filter)
      self.logger.debug("Finished listing keywords in %s", global_timer)
    else:
      matches = self.search_index(keywords)
      if matches:
        print '\n'.join(sorted(matches))
      self.logger.debug("Finished searching index in %s", global_timer)

  def init_logging(self):
    ''' Initialize the logging subsystem. '''
    self.logger = logging.getLogger('search-notes')
    self.logger.addHandler(logging.StreamHandler(sys.stderr))
    if os.isatty(0):
      self.logger.setLevel(logging.INFO)

  def parse_args(self):
    ''' Parse the command line arguments. '''
    try:
      opts, keywords = getopt.getopt(sys.argv[1:], 'il:d:n:e:vh',
          ['ignore-case', 'list=', 'database=', 'notes=', 'encoding=', 'verbose', 'help'])
    except getopt.GetoptError, error:
      print str(error)
      self.usage()
      sys.exit(2)
    # Define the command line option defaults.
    self.database_file = '~/.vim/misc/notes/index.pickle'
    self.user_directories = ['~/.vim/misc/notes/user/']
    self.character_encoding = 'UTF-8'
    self.case_sensitive = True
    self.keyword_filter = None
    # Map command line options to variables.
    for opt, arg in opts:
      if opt in ('-i', '--ignore-case'):
        self.case_sensitive = False
        self.logger.debug("Disabling case sensitivity")
      elif opt in ('-l', '--list'):
        self.keyword_filter = arg.strip().lower()
      elif opt in ('-d', '--database'):
        self.database_file = arg
      elif opt in ('-n', '--notes'):
        self.user_directories.append(arg)
      elif opt in ('-e', '--encoding'):
        self.character_encoding = arg
      elif opt in ('-v', '--verbose'):
        self.logger.setLevel(logging.DEBUG)
      elif opt in ('-h', '--help'):
        self.usage()
        sys.exit(0)
      else:
        assert False, "Unhandled option"
    self.logger.debug("Index file: %s", self.database_file)
    self.logger.debug("Notes directories: %r", self.user_directories)
    self.logger.debug("Character encoding: %s", self.character_encoding)
    if self.keyword_filter is not None:
      self.keyword_filter = self.decode(self.keyword_filter)
    # Canonicalize pathnames, check validity.
    self.database_file = self.munge_path(self.database_file)
    self.user_directories = map(self.munge_path, self.user_directories)
    self.user_directories = filter(os.path.isdir, self.user_directories)
    if not any(os.path.isdir(p) for p in self.user_directories):
      sys.stderr.write("None of the notes directories exist!\n")
      sys.exit(1)
    # Return tokenized keyword arguments.
    return [self.normalize(k) for k in self.tokenize(' '.join(keywords))]

  def load_index(self):
    ''' Load the keyword index or start with an empty one. '''
    try:
      load_timer = Timer()
      self.logger.debug("Loading index from %s ..", self.database_file)
      with open(self.database_file) as handle:
        self.index = pickle.load(handle)
        self.logger.debug("Format version of index loaded from disk: %i", self.index['version'])
        assert self.index['version'] == INDEX_VERSION, "Incompatible index format detected!"
        self.first_use = False
        self.dirty = False
        self.logger.debug("Loaded %i notes from index in %s", len(self.index['files']), load_timer)
    except Exception, e:
      self.logger.warn("Failed to load index from file: %s", e)
      self.first_use = True
      self.dirty = True
      self.index = {'keywords': {}, 'files': {}, 'version': INDEX_VERSION}

  def save_index(self):
    ''' Save the keyword index to disk. '''
    save_timer = Timer()
    with open(self.database_file, 'w') as handle:
      pickle.dump(self.index, handle)
    self.logger.debug("Saved index to disk in %s", save_timer)

  def update_index(self):
    ''' Update the keyword index by scanning the notes directory. '''
    update_timer = Timer()
    # First we find the filenames and last modified times of the notes on disk.
    notes_on_disk = {}
    last_count = 0
    for directory in self.user_directories:
      for filename in os.listdir(directory):
        # Vim swap files are ignored.
        if (filename != '.swp' and not fnmatch.fnmatch(filename, '.s??')
            and not fnmatch.fnmatch(filename, '.*.s??')):
          abspath = os.path.join(directory, filename)
          if os.path.isfile(abspath):
            notes_on_disk[abspath] = os.path.getmtime(abspath)
      self.logger.info("Found %i notes in %s ..", len(notes_on_disk) - last_count, directory)
      last_count = len(notes_on_disk)
    # Check for updated and/or deleted notes since the last run?
    if not self.first_use:
      for filename in self.index['files'].keys():
        if filename not in notes_on_disk:
          # Forget a deleted note.
          self.delete_note(filename)
        else:
          # Check whether previously seen note has changed?
          last_modified_on_disk = notes_on_disk[filename]
          last_modified_in_db = self.index['files'][filename]
          if last_modified_on_disk > last_modified_in_db:
            self.delete_note(filename)
            self.add_note(filename, last_modified_on_disk)
          # Already checked this note, we can forget about it.
          del notes_on_disk[filename]
    # Add new notes to index.
    for filename, last_modified in notes_on_disk.iteritems():
      self.add_note(filename, last_modified)
    self.logger.debug("Updated index in %s", update_timer)

  def add_note(self, filename, last_modified):
    ''' Add a note to the index (assumes the note is not already indexed). '''
    self.logger.info("Adding file to index: %s", filename)
    self.index['files'][filename] = last_modified
    with open(filename) as handle:
      for kw in self.tokenize(handle.read()):
        if kw not in self.index['keywords']:
          self.index['keywords'][kw] = [filename]
        else:
          self.index['keywords'][kw].append(filename)
    self.dirty = True

  def delete_note(self, filename):
    ''' Remove a note from the index. '''
    self.logger.info("Removing file from index: %s", filename)
    del self.index['files'][filename]
    for kw in self.index['keywords']:
      self.index['keywords'][kw] = [x for x in self.index['keywords'][kw] if x != filename]
    self.dirty = True

  def search_index(self, keywords):
    ''' Return names of files containing all of the given keywords. '''
    matches = None
    normalized_db_keywords = [(k, self.normalize(k)) for k in self.index['keywords']]
    for usr_kw in keywords:
      submatches = set()
      for original_db_kw, normalized_db_kw in normalized_db_keywords:
        # Yes I'm using a nested for loop over all keywords in the index. If
        # I really have to I'll probably come up with something more
        # efficient, but really it doesn't seem to be needed -- I have over
        # 850 notes (about 8 MB) and 25000 keywords and it's plenty fast.
        if usr_kw in normalized_db_kw:
          submatches.update(self.index['keywords'][original_db_kw])
      if matches is None:
        matches = submatches
      else:
        matches &= submatches
    return list(matches) if matches else []

  def list_keywords(self, substring, limit=25):
    ''' Print all (matching) keywords to standard output. '''
    decorated = []
    substring = self.normalize(substring)
    for kw, filenames in self.index['keywords'].iteritems():
      normalized_kw = self.normalize(kw)
      if substring in normalized_kw:
        if levenshtein_supported:
          decorated.append((Levenshtein.distance(normalized_kw, substring), -len(filenames), kw))
        else:
          decorated.append((-len(filenames), kw))
    decorated.sort()
    selection = [d[-1] for d in decorated[:limit]]
    print self.encode(u'\n'.join(selection))

  def tokenize(self, text):
    ''' Tokenize a string into a list of normalized, unique keywords. '''
    words = set()
    text = self.decode(text)
    for word in re.findall(r'\w+', text, re.UNICODE):
      word = word.strip()
      if word != '' and not word.isspace() and len(word) >= 2:
        words.add(word)
    return words

  def normalize(self, keyword):
    ''' Normalize the case of a keyword if configured to do so. '''
    return keyword if self.case_sensitive else keyword.lower()

  def encode(self, text):
    ''' Encode a string in the user's preferred character encoding. '''
    return text.encode(self.character_encoding, 'ignore')

  def decode(self, text):
    ''' Decode a string in the user's preferred character encoding. '''
    return text.decode(self.character_encoding, 'ignore')

  def munge_path(self, path):
    ''' Canonicalize user-defined path, making it absolute. '''
    return os.path.abspath(os.path.expanduser(path))

  def usage(self):
    print __doc__.strip()

class Timer:

    """
    Easy to use timer to keep track of long during operations.
    """

    def __init__(self):
        self.start_time = time.time()

    def __str__(self):
        return "%.2f seconds" % self.elapsed_time

    @property
    def elapsed_time(self):
        return time.time() - self.start_time

if __name__ == '__main__':
  NotesIndex()

# vim: ts=2 sw=2 et
