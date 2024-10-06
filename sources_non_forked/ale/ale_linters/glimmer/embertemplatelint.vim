" Author: Sam Saffron <sam.saffron@gmail.com>
" Description: Ember-template-lint for checking GJS (Glimmer JS) files

scriptencoding utf-8

call ale#handlers#embertemplatelint#DefineLinter('glimmer')
