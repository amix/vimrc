yankstack.vim
=============

Author:  Max Brunsfeld <http://www.github.com/maxbrunsfeld>

[Yankstack.vim](https://github.com/maxbrunsfeld/vim-yankstack) is a
lightweight implementation of the Emacs 'kill ring' for Vim.  It allows you to
yank and delete things without worrying about losing the text that you yanked
previously. It effectively turns your default register into a stack, and lets
you cycle through the items in the stack after doing a paste.

This plugin is intended to be a simpler alternative to the
[yankring](https://github.com/chrismetcalf/vim-yankring) plugin. It has a fairly
complete [test suite](https://github.com/maxbrunsfeld/vim-yankstack/blob/master/spec/yankstack/yankstack_spec.rb)
based on [rspec](https://www.relishapp.com/rspec)
and [vimbot](https://github.com/maxbrunsfeld/vimbot).

## Installation ##

I recommend loading your plugins with
[vundle](https://github.com/gmarik/vundle) or 
[pathogen](https://github.com/tpope/vim-pathogen).

## Key Mappings ##

By default, yankstack adds only 2 key bindings, in normal and visual modes:

- ```meta-p```  - cycle *backward* through your history of yanks
- ```meta-shift-p```  - cycle *forwards* through your history of yanks

After pasting some text using ```p``` or ```P```, you can cycle through your
yank history using these commands. Typing either of these keys *without* pasting first 
will do a normal paste (the same as typing `p`). This also works in insert mode.

### the 'meta' key

If you're using MacVim, and you want to use
this plugin's default key bindings (or any bindings involving the `option`
key), you must ```:set macmeta```. On Linux, you may have issues with the meta key if your terminal is running in 7bit mode.
Instructions for dealing with this can be found on the [wiki](https://github.com/maxbrunsfeld/vim-yankstack/wiki/Linux-terminal-configurations-for-correct-meta-key-handling)

## Commands ##

You can see the contents of the yank-stack using the ```:Yanks``` command.
Its output is similar to the ```:registers``` command.

## Configuration ##

Yankstack defines two plugin mappings that you can map to keys of your choosing.
The same mappings work in normal and insert modes.

- ```<Plug>yankstack_substitute_older_paste``` - cycle backwards through your history of yanks
- ```<Plug>yankstack_substitute_newer_paste``` - cycle forwards through your history of yanks

For example, if you wanted to define some mappings based on your 'leader' key,
you could do this:

```
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
```

Also, if you want to load yankstack without the default key mappings, just
``` let g:yankstack_map_keys = 0 ```
in your .vimrc file.

## Compatibility ##

Yankstack works by mapping the yank and paste keys to functions that do some
book-keeping before calling through to the normal yank/paste keys. You may want
to define your own mappings of the yank and paste keys. For example, I like to
map the ```Y``` key to ```y$```, so that it behaves the same as ```D``` and
```C```. The yankstack mappings need to happen **before** you define any such
mappings of your own. To achieve this, just call ```yankstack#setup()``` in
your vimrc, before defining your mappings:

```
call yankstack#setup()
nmap Y y$
" other mappings involving y, d, c, etc
```

You can also prevent certain keys from being remapped by setting the `g:yankstack_yank_keys`
to the keys of your choosing. For example, if you only want Yankstack to remap `y` and `d`:

```
let g:yankstack_yank_keys = ['y', 'd']
```

## Contributing, Feedback ##

I'd enjoy hearing anybody's feedback on yankstack, and welcome any contribution.
Check it out on [github](https://github.com/maxbrunsfeld/vim-yankstack)!

## Changelog ##


### 1.0.6 (2014-08-04)
  - Allow customization of the list of keys to be remapped.

### 1.0.5 (2012-07-19)
  - Fix bug where on certain versions of vim, the first time you tried
    to cycle through your yanks after doing a normal paste, an extra
    paste was created.

### 1.0.4 (2012-07-01)
  - Make it so that yankstack-cycling keys cause a normal paste if they are
    used without pasting first. Fix stack-cycling in insert-mode.

### 1.0.3 (2012-05-04):
  - Fix bug when overwriting text in select mode. This was causing
    problems for snipMate users.

### 1.0.2 (2012-4-20):
  - Add test coverage using rspec and [vimbot](https://github.com/maxbrunsfeld/vimbot)!
  - Perfect the behavior of the yankstack when pasting over text in visual
    mode
  - Fix bug where 's' and 'S' didn't push to the yankstack

### 1.0.1 (2012-2-11):
  - Change default key bindings, update readme, add link to github page.

### 1.0.1 (2011-12-08):
  - Fix bug when displaying empty yanks.

### 1.0.0 (2011-12-04):
  - Remove unnecessary dependency on the undotree() function. Plugin should
    now work on any recent version of vim.

## License ##
Copyright (c) Max Brunsfeld.  Distributed under the same terms as Vim itself.
See the vim license.

 vim:tw=78:ts=8:ft=help:norl:
