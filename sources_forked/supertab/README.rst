.. Copyright (c) 2012 - 2014, Eric Van Dewoestine
   All rights reserved.

   Redistribution and use of this software in source and binary forms, with
   or without modification, are permitted provided that the following
   conditions are met:

   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.

   * Neither the name of Eric Van Dewoestine nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission of
     Eric Van Dewoestine.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

.. _overview:

==================
Overview
==================

Supertab is a vim plugin which allows you to use <Tab> for all your insert
completion needs (:help ins-completion).

Features
--------

- Configurable to suit you needs:

  - Default completion type to use.
  - Prevent <Tab> from completing after/before defined patterns.
  - Close vim's completion preview window when code completion is finished.
  - When using other completion types, you can configure how long to 'remember'
    the current completion type before returning to the default.
  - Don't like using <Tab>? You can also configure a different pair of keys to
    scroll forwards and backwards through completion results.

- Optional improved 'longest' completion support (after typing some characters,
  hitting <Tab> will highlight the next longest match).
- Built in 'context' completion option which chooses the appropriate completion
  type based on the text preceding the cursor.

  - You can also plug in your own functions to determine which completion type
    to use.

- Support for simple completion chaining (falling back to a different
  completion type, keyword completion for example, if omni or user completion
  returns no results).

Installation
------------

You have a few options when it comes to installing supertab:

1. Use your linux package manager:

   Some linux distributions include a supertab package so you don't have to
   manage the install/upgrade of supertab separately from other software on your
   system.

2. Use a vim plugin manager:

   There are several plugin managers for vim, which will either allow you to
   manually clone vim plugin repositories, or will do so for you. Probably the
   two most popular ones currently are `pathogen
   <https://github.com/tpope/vim-pathogen>`_ and `vundle
   <https://github.com/gmarik/Vundle.vim>`_. Please refer to their docs for
   instructions on how to install plugins.

3. And lastly you can use the vimball (.vmb) file found on
   `vim.org <http://www.vim.org/scripts/script.php?script_id=1643>`_:

   Vimball files are installed by simply opening them in vim and then sourcing
   the file:

   ::

     $ vim supertab.vmb
     :source %

Frequently Asked Questions
--------------------------

- **Why isn't supertab honoring my configured settings (attempts to complete at the
  start of a line, always performs keyword completion instead of my configured
  default, etc.)?**

  Chances are that you have a very old version of `snipmate
  <https://github.com/msanders/snipmate.vim>`_ installed, or something similar,
  which will issue a `<c-n>` when no snippet is found. Supertab use to map to
  `<c-n>`, so this behavior would act as a fallback to supertab, but current
  versions of supertab no longer do so, resulting in snipmate bypassing supertab
  entirely.

  You can check if this is the case by running the following in vim to see what
  is mapped to `<tab>`:

  ::

    :verbose imap <tab>

  To resolve the issue you can either:

  #. Install my `fork <https://github.com/ervandew/snipmate.vim>`_ or
  #. Upgrade to a more recent snipmate fork, like `garbas/vim-snipmate
     <https://github.com/garbas/vim-snipmate>`_

  See `#74 <https://github.com/ervandew/supertab/issues/74>`_ for additional
  details.

- **Why does <tab> navigate the completion menu from bottom to top?**

  First, if after reading the explanation below (or if you don't want to bother
  reading it), you still want the default to scroll down the list then you can
  use:

  ::

    let g:SuperTabDefaultCompletionType = "<c-n>"

  or if your default completion type is currently `context` then you can use
  this instead:

  ::

    let g:SuperTabContextDefaultCompletionType = "<c-n>"

  Now on to the reasoning behind this. When using `<c-p>` or `<c-n>` to start
  insert completion, both populate the completion popup with the same list of
  words in the same order, the only difference is that `<c-p>` highlights the
  nearest matching word located above the current cursor position, which is the
  result at the bottom of the completion popup. Without supertab installed,
  continuing to hit `<c-p>` will walk up the list to next nearest word above the
  cursor.

  I think Bram chose to display the results like this so that

  #. the completion logic is the same for `<c-n>` and `<c-p>`, only the first
     entry to highlight differs
  #. so that the behavior of `<c-p>` mode is consistent, always moving up the
     list and
  #. when starting `<c-p>` mode you don't have to switch over to
     using `<c-n>` to get the next nearest entry, just continue to hit `<c-p>`.

  So, with supertab I wanted to preserve the same behavior. If `<c-p>` is your
  default completion method (supertab defaults to this being the case), then
  `<tab>` will start it and additional uses of `<tab>` will move up the list
  instead of down so that you don't have to suddenly switch to using `<s-tab>`
  to get the next nearest result.

  Why is `<c-p>` the supertab default? The original supertab author found (and I
  agree with his finding) that while coding, the keyword match you want is
  typically the closer of the matches above the cursor, which `<c-p>` naturally
  provides.
