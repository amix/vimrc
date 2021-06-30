# Gist.vim

This is a vimscript for creating gists (http://gist.github.com).

For the latest version please see https://github.com/mattn/vim-gist.

## Usage:

- Post current buffer to gist, using default privacy option.

        :Gist

- Post selected text to gist, using default privacy option.
  This applies to all permutations listed below (except multi).

        :'<,'>Gist

- Create a private gist.

        :Gist -p

- Create a public gist.
  (Only relevant if you've set gists to be private by default.)

        :Gist -P

>  This is only relevant if you've set gists to be private by default;
> if you get an empty gist list, try ":Gist --abandon".

- Create a gist anonymously.

        :Gist -a

- Create a gist with all open buffers.

        :Gist -m

- Edit the gist (you need to have opened the gist buffer first).
  You can update the gist with the ":w" command within the gist buffer.

        :Gist -e

- Edit the gist with name 'foo.js' (you need to have opened the gist buffer
  first).

        :Gist -e foo.js

- Post/Edit with the description " (you need to have opened the gist buffer
  first). >

        :Gist -s something
        :Gist -e -s something

- Delete the gist (you need to have opened the gist buffer first).
  Password authentication is needed.

        :Gist -d

- Fork the gist (you need to have opened the gist buffer first).
  Password authentication is needed.

        :Gist -f

- Star the gist (you need to have opened the gist buffer first).
  Password authentication is needed.

        :Gist +1

- Unstar the gist (you need to have opened the gist buffer first).
  Password authentication is needed.

        :Gist -1

- Get gist XXXXX.

        :Gist XXXXX

- Get gist XXXXX and add to clipboard.

        :Gist -c XXXXX

- List your public gists.

        :Gist -l

- List gists from user "mattn".

        :Gist -l mattn

- List everyone's gists.

        :Gist -la

- List gists from your starred gists.

        :Gist -ls

- Open the gist on browser after you post or update it.

        :Gist -b

## List Feature

- Useful mappings on the gist-listing buffer:
    - Both `o` or `Enter` open the gist file in a new buffer, and close the
      vim-gist listing one.
    - `b` opens the gist file in a browser; this is necessary because
      `Shift-Enter` (as was originally) only works for GUI vim.
    - `y` copies the contents of the selected gist to the clipboard, and
      closes the vim-gist buffer.
    - `p` pastes the contents of the selected gist to the buffer from where
      vim-gist was called, and closes the vim-gist buffer.
    - Hitting `Escape` or `Tab` at the vim-gist buffer closes it.

- Gist listing has fixed-length columns now, more amenable to eye inspection.
  Every line on the gist-listing buffer contains the gist id, name and
  description, in that order. Columns are now padded and truncated to offer a
  faster browsing, in the following way:
  - The gist id string is fixed at 32 characters.
  - The length (in characters) of the name of the gist is fixed and
    can be set by the user using, for example:

    `let g:gistvim_namelength = 20`

    The default value for `gistvim_namelength` is 30. If the gist (file)name
    exceeds that length, it is truncated to the specified length.
  - Finally, the gist description is truncated in length to fit the remaining
    of the line, avoiding wrapped lines that mess up the table layout.
  - Note that the gist listing buffer now does not show the field 'code'
    (not sure what that did in the first place).

## Tips:

If you set g:gist_clip_command, gist.vim will copy the gist code with option
'-c'.

- Mac:

        let g:gist_clip_command = 'pbcopy'

- Linux:

        let g:gist_clip_command = 'xclip -selection clipboard'

- Others (cygwin?):

        let g:gist_clip_command = 'putclip'

If you want to detect filetype from the filename:

    let g:gist_detect_filetype = 1

If you want to open browser after the post:

    let g:gist_open_browser_after_post = 1

If you want to change the browser:

    let g:gist_browser_command = 'w3m %URL%'

or:

    let g:gist_browser_command = 'opera %URL% &'

On windows, this should work with your user settings.

If you want to show your private gists with ":Gist -l":

    let g:gist_show_privates = 1

If you want your gist to be private by default:

    let g:gist_post_private = 1

If you want your gist to be anonymous by default:

    let g:gist_post_anonymous = 1

If you want to manipulate multiple files in a gist:

    let g:gist_get_multiplefile = 1

If you want to use on GitHub Enterprise:

    let g:gist_api_url = 'http://your-github-enterprise-domain/api/v3'

You need to either set global git config:

	$ git config --global github.user Username

## License:

    Copyright 2010 by Yasuhiro Matsumoto
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
       this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
    OF THE POSSIBILITY OF SUCH DAMAGE.


## Install:

Copy it to your plugin directory.
gist.vim will create a curl cookie-jar file in your runtimepath.

- rtp:
  - autoload/gist.vim
  - plugin/gist.vim

If you want to uninstall gist.vim, remember to also remove `~/.gist-vim`.

You need to install webapi-vim also:

  http://www.vim.org/scripts/script.php?script_id=4019

If you want to use latest one:

  https://github.com/mattn/webapi-vim

### Install with [Vundle](https://github.com/gmarik/vundle)

Add the following lines to your `.vimrc`.

    Bundle 'mattn/webapi-vim'
    Bundle 'mattn/vim-gist'

Now restart Vim and run `:BundleInstall`.

### Install with [NeoBundle](https://github.com/Shougo/neobundle.vim)

Add the following line to your `.vimrc`.

    NeoBundle 'mattn/vim-gist', {'depends': 'mattn/webapi-vim'}

## Requirements:

- curl command (http://curl.haxx.se/)
- webapi-vim (https://github.com/mattn/webapi-vim)
- and if you want to use your git profile, the git command-line client.

## Setup:

This plugin supports both basic and two-factor authentication using GitHub
API v3. The plugin stores its credentials in `~/.gist-vim`.

First, you need to set your GitHub username in git's global configuration:

    $ git config --global github.user <username>

Then vim-gist will ask for your password in order to create an access
token. If you have two-factor authentication enabled, vim-gist will also
prompt you to enter the two-factor key you receive.

NOTE:
If you want you can set it directly to `g:github_user` and `g:gist_token`.

Whichever type of authentication you use, your GitHub password will not be
stored, only a OAuth access token produced specifically for vim-gist. The
token is stored in `~/.gist-vim`. If you stop using the plugin, you can
easily remove this file. To revoke the associated  GitHub token, go to the
list of ["Authorized applications" on GitHub's "Account Settings"
page][uas].

[uas]: https://github.com/settings/applications

**Note:** the username is optional if you only send anonymous gists.
