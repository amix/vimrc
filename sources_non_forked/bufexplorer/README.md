created by
---------
[jeff lanzarotta](http://www.vim.org/account/profile.php?user_id=97)
 
script type
----------
utility
 
description
-----------
With bufexplorer, you can quickly and easily switch between buffers by using the one of the default public interfaces: 

    '\be' (normal open)  or 
    '\bs' (force horizontal split open)  or 
    '\bv' (force vertical split open) 

Once the bufexplorer window is open you can use the normal movement keys (hjkl) to move around and then use <Enter> or <Left-Mouse-Click> to select the buffer you would like to open. If you would like to have the selected buffer opened in a new tab, simply press either <Shift-Enter> or 't'. Please note that when opening a buffer in a tab, that if the buffer is already in another tab, bufexplorer can switch to that tab automatically for you if you would like. More about that in the supplied VIM help. 

Bufexplorer also offers various options including: 

* Display the list of buffers in various sort orders including: 
  * Most Recently Used (MRU) which is the default 
  * Buffer number 
  * File name 
  * File extension 
  * Full file path name 
* Delete buffer from list 

For more about options, sort orders, configuration options, etc. please see the supplied VIM help.
 
install details
---------------
Simply unzip bufexplorer.zip into a directory in your 'runtimepath', usually ~/.vim or c:\vimfiles, and restart Vim. This zip file contains plugin\bufexplorer.vim, and doc\bufexplorer.txt.  See ':help add-local-help' on how to add bufexplorer.txt to vim's help system. 

NOTE
----
Version 7.0.12 and above will ONLY work with 7.0 and above of Vim. 

**IMPORTANT**: If you have a version prior to 7.1.2 that contains an autoload\bufexplorer.vim file, please REMOVE the autoload\bufexlorer.vim AND the plugin\bufexplorer.vim files before installing a new version.
