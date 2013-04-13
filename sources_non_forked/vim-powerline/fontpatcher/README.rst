======================
Powerline font patcher
======================

:Author: Kim Silkebækken (kim.silkebaekken+vim@gmail.com)

Description
-----------

This font patcher creates dividers and symbols for use with Powerline. The 
script requires Python 2 and FontForge compiled with Python bindings.

Patched fonts are renamed by default (" for Powerline" is added to the font 
name) so they don't conflict with existing fonts. Use the ``--no-rename`` 
option to disable font renaming.

Glyph table
-----------

All the glyphs are stored in the ``U+2B60``-``U+2BFF`` range ("Misc symbols 
and arrows").

+------------+-------------------+
| Code point | Description       |
+============+===================+
| ``U+2B60`` | Branch symbol     |
+------------+-------------------+
| ``U+2B61`` | LN (line) symbol  |
+------------+-------------------+
| ``U+2B62`` | FT symbol, part 1 |
+------------+-------------------+
| ``U+2B63`` | FT symbol, part 2 |
+------------+-------------------+
| ``U+2B64`` | Padlock (closed)  |
+------------+-------------------+
| ``U+2B80`` | Hard right arrow  |
+------------+-------------------+
| ``U+2B81`` | Soft right arrow  |
+------------+-------------------+
| ``U+2B82`` | Hard left arrow   |
+------------+-------------------+
| ``U+2B83`` | Soft left arrow   |
+------------+-------------------+

===================
Font patching guide
===================

There's a `GitHub wiki page`_ dedicated to community-contributed patched 
fonts. You may download one of the fonts on that page if you don't want to 
patch the fonts yourself.

If you do patch a font that's not included in the wiki (and you have 
permission to distribute it), please include it on the wiki page.

**Note:** The fonts in the wiki may be outdated, and may have different 
glyphs than the ones provided in the latest version of Powerline. It's 
recommended that you always patch your fonts yourself if you have the 
required software.

.. _`GitHub wiki page`: https://github.com/Lokaltog/vim-powerline/wiki/Patched-fonts

Linux
-----

1. Install fontforge with Python bindings. For Ubuntu users the required 
   package is ``python-fontforge``, for Arch Linux users the required 
   package is ``fontforge``. It should be something similar for other 
   distros.

2. Run the font patcher::

       $ /path/to/fontpatcher MyFontFile.ttf

3. Copy the font file into ``~/.fonts`` (or another X font directory)::

       $ cp MyFontFile-Powerline.otf ~/.fonts

   **Note:** If the font is a pure bitmap font (e.g. a PCF font) it will be 
   stored in the BDF format. This is usually not a problem, and you may 
   convert the font back to the PCF format using ``bdftopcf`` if you want 
   to. All other fonts will be stored in the OTF format regardless of the 
   original format.

4. Update your font cache::

       $ sudo fc-cache -vf

   **Note:** If you use vim in rxvt-unicode in the client/daemon mode, you 
   may need to close all running terminals as well for the font to be 
   updated.

5. **For gvim users:** Update the GUI font in your ``vimrc`` file::

       set guifont=MyFont\ for\ Powerline

   **For terminal users:** Update your terminal configuration to use the 
   patched font.

6. Update your ``vimrc`` configuration to use the new symbols::

       let g:Powerline_symbols = 'fancy'

7. Make sure that the cache file is deleted::

       $ rm /tmp/Powerline.cache

8. Start vim and enjoy your new statusline!

OS X
----

1. Check if you have a FontForge version with Python support by running 
   ``fontforge -version``. You should see something like this::

       $ fontforge -version
       Copyright (c) 2000-2011 by George Williams.
       Executable based on sources from 13:48 GMT 22-Feb-2011-D.
       Library based on sources from 13:48 GMT 22-Feb-2011.
       fontforge 20110222
       libfontforge 20110222

   Make sure that the executable version number doesn't have ``NoPython`` in 
   it. If everything looks OK, skip ahead to step 4.

2. If you have FontForge but with ``NoPython`` in the version number, please 
   try to update to a later version::

       $ brew uninstall fontforge
       $ brew update
       $ brew install --use-gcc fontforge

   **Note:** You may have to use ``--use-clang`` instead of ``--use-gcc`` 
   when compiling FontForge.

3. If you don't have FontForge, install it with Homebrew::

       $ brew update
       $ brew install --use-gcc fontforge

4. Patch your fonts by passing the ``fontpatcher`` script as a parameter to 
   FontForge::

       $ fontforge -script /path/to/fontpatcher MyFontFile.ttf

5. Install the font by double-clicking the font file in Finder and click 
   "Install this font" from the preview window.

6. **For gvim users:** Update the GUI font in your ``vimrc`` file::

       set guifont=MyFont\ for\ Powerline

   **For terminal users:** Update your terminal configuration to use the 
   patched font.

7. Update your ``vimrc`` configuration to use the new symbols::

       let g:Powerline_symbols = 'fancy'

8. Make sure that the cache file is deleted::

       $ rm /tmp/Powerline.cache

9. Start vim and enjoy your new statusline!
