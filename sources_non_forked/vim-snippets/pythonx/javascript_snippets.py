#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Helper utilities to format javascript snippets.
"""

ALWAYS = 'always'
NEVER = 'never'


def get_option(snip, option, default=None):
    return snip.opt('g:ultisnips_javascript["{}"]'.format(option), default)


def semi(snip):
    option = get_option(snip, 'semi', ALWAYS)

    if option == NEVER:
        ret = ''
    elif option == ALWAYS:
        ret = ';'
    else:
        ret = ';'
    return ret


def space_before_function_paren(snip):
    option = get_option(snip, 'space-before-function-paren', NEVER)

    if option == NEVER:
        ret = ''
    elif option == ALWAYS:
        ret = ' '
    else:
        ret = ''
    return ret


def keyword_spacing(snip):
    option = get_option(snip, 'keyword-spacing', ALWAYS)

    if option == NEVER:
        ret = ''
    elif option == ALWAYS:
        ret = ' '
    else:
        ret = ''
    return ret
