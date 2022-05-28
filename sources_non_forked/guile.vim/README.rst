.. default-role:: code

###########################
 GNU Guile support for Vim
###########################

This plugin extends Vim's Scheme support to include the additions to the
language provided by the `GNU Guile`_ implementation. The plugin automatically
detects whether a Scheme file is a Guile file and adds syntax highlighting for
Guile's special forms.


Installation
############

Install this like any other Vim plugin.


Using the plugin
################

When a Guile buffer has been detected its `filetype` option will be set to the
value `scheme.guile`. This uses Vim's dotted file type (see `:h 'filetype'`) in
order to allow users to keep using their setting any plugins for Scheme in
addition to this.

Guile is detected by either looking for a shebang in the first line (see
`4.3.1 The Top of a Script File`_ in the Guile manual), or by scanning the file
for an occurrence of `define-module` or `use-modules`. This is not absolutely
reliable, but it should work for the vast majority of cases.


License
#######

Released under the MIT (Expat) license, see the COPYING_ file for details.


.. ----------------------------------------------------------------------------
.. _GNU Guile: http://www.gnu.org/software/guile/
.. _COPYING: COPYING.txt
.. _4.3.1 The Top of a Script File: info:guile.info#The%20Top%20of%20a%20Script%20File
