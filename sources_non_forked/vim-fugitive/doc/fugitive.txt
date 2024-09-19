*fugitive.txt*  A Git wrapper so awesome, it should be illegal

Author:  Tim Pope <http://tpo.pe/>
License: Same terms as Vim itself (see |license|)

This plugin is only available if 'compatible' is not set.

INTRODUCTION                                    *fugitive*

Whenever you edit a file from a Git repository, a set of commands is defined
that serve as a gateway to Git.

COMMANDS                                        *fugitive-commands*

These commands are local to the buffers in which they work (generally, buffers
that are part of Git repositories).

                                                *fugitive-:G*
:G [args]               Same as :Git, but two characters shorter.

                                                *fugitive-summary*
:Git                    With no arguments, bring up a summary window vaguely
                        akin to git-status.  If a summary window is already
                        open for the current repository, it is focused
                        instead.  Press g?  or see |fugitive-maps| for usage.

                                                *:Git*
:Git {args}             Run an arbitrary git command and display any output.
                        On UNIX this uses a pty and on other platforms it uses
                        a pipe, which will cause some behavior differences
                        such as the absence of progress bars.  Any file the
                        command edits (for example, a commit message) will be
                        loaded into a split window.  Closing that window will
                        resume running the command.  A few Git subcommands
                        have different behavior; these are documented below.

                                                *:Git!*
:Git! {args}            Run an arbitrary git command in the background and
                        stream the output to the preview window.  Requires a
                        Vim with |setbufline()|.  Press CTRL-D during an
                        interactive :Git invocation to switch to this mode
                        retroactively.

                                                *:Git_--paginate* *:Git_-p*
:Git --paginate {args}  Run an arbitrary git command, capture output to a temp
:Git -p {args}          file, and |:split| that temp file.  Pass ++curwin as
                        the first argument to |:edit| the temp file instead.
                        A temp file is always used for commands like diff and
                        log that typically uses a pager, and for any command
                        that has the pager.<cmd> Git configuration option set.

:{range}Git! --paginate {args}
:{range}Git! -p {args}  Run an arbitrary git command, and insert the output
                        after {range} in the current buffer.

                                                *:Git_blame*
:Git blame [flags]      Run git-blame [flags] on the current file and open the
                        results in a scroll-bound vertical split.  The
                        following maps, which work on the cursor line commit
                        where sensible, are provided:

                        g?    show this help
                        A     resize to end of author column
                        C     resize to end of commit column
                        D     resize to end of date/time column
                        gq    close blame, then |:Gedit| to return to work
                              tree version
                        <CR>  close blame, and jump to patch that added line
                              (or directly to blob for boundary commit)
                        o     jump to patch or blob in horizontal split
                        O     jump to patch or blob in new tab
                        p     jump to patch or blob in preview window
                        -     reblame at commit

                        The maps |fugitive_P| and |fugitive_~| are also
                        supported to reblame on a parent commit, but this is
                        inherently fragile, as the line being blamed will no
                        longer exist.  The preferred alternative is to use
                        <CR> to open up the commit, select the corresponding
                        `-` line that you care about, and press <CR> twice
                        more to reblame at that line.  Viewing the commit also
                        gives you additional context as to why the line
                        changed.

                                                *g:fugitive_dynamic_colors*
                        In the GUI or a 256 color terminal, commit hashes will
                        be highlighted in different colors. To disable this:
>
                        let g:fugitive_dynamic_colors = 0
<
:[range]Git blame [...] If a range is given, just that part of the file will
:Git blame [...] {file} be blamed, and a horizontal split without
                        scrollbinding is used.  You can also give an arbitrary
                        filename.

                                                *:Git_difftool*
:Git[!] difftool [args] Invoke `git diff [args]` and load the changes into the
                        quickfix list.  Each changed hunk gets a separate
                        quickfix entry unless you pass an option like
                        --name-only or --name-status.  Jumps to the first
                        change unless [!] is given.

:Git difftool -y [args] Invoke `git diff [args]`, open each changed file in a
                        new tab, and invoke |:Gdiffsplit!| against the
                        appropriate commit.

                                                *:Git_mergetool*
:Git mergetool [args]   Like |:Git_difftool|, but target merge conflicts.

Wrappers for Vim built-ins ~

These all directly map onto a built-in Vim command, and generally have names
that prepend "G" to the command they are wrapping.  For example, :Ggrep is G
plus |:grep|.

                                                *:Ggrep* *:Git_grep*
:Ggrep[!] [args]        An approximation of |:grep|[!] with git-grep as
:Git[!] grep -O [args]  'grepprg'.

:Ggrep[!] --quiet [args]
:Ggrep[!] -q [args]     Like |:Ggrep|, but instead of displaying output, open
                        the quickfix list.

                                                *:Glgrep*
:Glgrep[!] [args]       :Ggrep but for |:lgrep|.
:0Git[!] grep -O [args]

                                                 *:Gclog*
:Gclog[!] [args]        Use git-log [args] to load the commit history into the
                        |quickfix| list.  Jumps to the first commit unless [!]
                        is given.  This command wraps |:cfile|.

                        The quickfix list can be awkward for many use cases
                        and exhibits extremely poor performance with larger
                        data sets.  Consider using |:Git| log --oneline
                        instead.

:{range}Gclog[!] [args] Use git-log -L to load previous revisions of the given
                        range of the current file into the |quickfix| list.
                        The cursor is positioned on the first line of the
                        first diff hunk for each commit.  Use :0Gclog to
                        target the entire file.

                                                *:Gllog*
:Gllog [args]           Like |:Gclog|, but use the location list instead of the
                        |quickfix| list.

                                                *:Gcd*
:Gcd [directory]        |:cd| relative to the repository.

                                                *:Glcd*
:Glcd [directory]       |:lcd| relative to the repository.

                                                *:Gedit* *fugitive-:Ge*
:Gedit [object]         |:edit| a |fugitive-object|.

                                                *:Gsplit*
:Gsplit [object]        |:split| a |fugitive-object|.

                                                *:Gvsplit*
:Gvsplit [object]       |:vsplit| a |fugitive-object|.

                                                *:Gtabedit*
:Gtabedit [object]      |:tabedit| a |fugitive-object|.

                                                *:Gpedit*
:Gpedit [object]        |:pedit| a |fugitive-object|.

                                                *:Gdrop*
:Gdrop [object]         |:drop| a |fugitive-object|.

                                                *:Gread* *fugitive-:Gr*
:Gread [object]         Empty the buffer and |:read| a |fugitive-object|.
                        When the argument is omitted, this is similar to
                        git-checkout on a work tree file or git-add on a stage
                        file, but without writing anything to disk.

:{range}Gread [object]  |:read| in a |fugitive-object| after {range}.

                                                *:Gwrite* *fugitive-:Gw*
:Gwrite                 Write to the current file's path and stage the results.
                        When run in a work tree file, it is effectively git
                        add.  Elsewhere, it is effectively git-checkout.  A
                        great deal of effort is expended to behave sensibly
                        when the work tree or index version of the file is
                        open in another buffer.

:Gwrite {path}          You can give |:Gwrite| an explicit path of where in
                        the work tree to write.  You can also give a path like
                        :0:foo.txt or :0:% to write to just that stage in
                        the index.

                                                *:Gwq*
:Gwq [path]             Like |:Gwrite| followed by |:quit| if the write
                        succeeded.

:Gwq! [path]            Like |:Gwrite|! followed by |:quit|! if the write
                        succeeded.

                                                *:Gdiffsplit*
:Gdiffsplit [object]    Perform a |vimdiff| against the given file, or if a
                        commit is given, the current file in that commit.
                        With no argument, the version in the index or work
                        tree is used, and the work tree version is always
                        placed to the right or bottom, depending on available
                        width.  Use Vim's |do| and |dp| to stage and unstage
                        changes.

                                                *:Gdiffsplit!*
:Gdiffsplit!            Diff against any and all direct ancestors, retaining
                        focus on the current window.  During a merge conflict,
                        this is a three-way diff against the "ours" and
                        "theirs" ancestors.  Additional d2o and d3o maps are
                        provided to obtain the hunk from the "ours" or
                        "theirs" ancestor, respectively.

:Gdiffsplit! {object}   Like |:Gdiffsplit|, but retain focus on the current
                        window.

                                                *:Gvdiffsplit*
:Gvdiffsplit [object]   Like |:Gdiffsplit|, but always split vertically.

                                                *:Ghdiffsplit*
:Gdiffsplit ++novertical [object]
:Ghdiffsplit [object]   Like |:Gdiffsplit|, but with "vertical" removed from
                        'diffopt'.  The split will still be vertical if
                        combined with |:vertical|.

Other commands ~

These do not directly correspond to any built-in Vim command, and have a
capital letter after the "G" to convey this. For example, the file move
operation has nothing to do with the |:move| built-in, so it is named :GMove,
not :Gmove.

                                                *:GMove*
:GMove {destination}    Wrapper around git-mv that renames the buffer
                        afterward.  Add a ! to pass -f.

                                                *:GRename*
:GRename {destination}  Like |:GMove| but operates relative to the parent
                        directory of the current file.

                                                *:GDelete*
:GDelete                Wrapper around git-rm that deletes the buffer
                        afterward.  When invoked in an index file, --cached is
                        passed.  Add a ! to pass -f and forcefully discard the
                        buffer.

                                                *:GRemove* *:GUnlink*
:GRemove                Like |:GDelete|, but keep the (now empty) buffer around.
:GUnlink

                                                *:GBrowse*
:GBrowse                Open the current file, blob, tree, commit, or tag
                        in your browser at the upstream hosting provider.
                        Upstream providers can be added by installing an
                        appropriate Vim plugin.  For example, GitHub can be
                        supported by installing rhubarb.vim, available at
                        <https://github.com/tpope/vim-rhubarb>.

:GBrowse {object}       Like :GBrowse, but for a given |fugitive-object|.

:{range}GBrowse [args]  Appends an anchor to the URL that emphasizes the
                        selected lines. This also forces the URL to include a
                        commit rather than a branch name so it remains valid
                        if the file changes.  You can give a range of "0" to
                        force this behavior without including an anchor.

:GBrowse [...]@{remote} Force using the given remote rather than the remote
                        for the current branch.  The remote is used to
                        determine which upstream repository to link to.

:GBrowse {url}          Open an arbitrary URL in your browser.

:[range]GBrowse! [args] Like :GBrowse, but put the URL on the clipboard rather
                        than opening it.

MAPS                                            *fugitive-maps*

These maps are available in both the |fugitive-summary| buffer and Fugitive
object buffers, although not all maps make sense in all buffers.  Mappings
that operate on the file or hunk under the cursor are generally available in
visual mode to operate on multiple files or partial hunks.

                                                *fugitive-staging-maps*
Staging/unstaging maps ~

                                                *fugitive_s*
s                       Stage (add) the file or hunk under the cursor.

                                                *fugitive_u*
u                       Unstage (reset) the file or hunk under the cursor.

                                                *fugitive_-*
-                       Stage or unstage the file or hunk under the cursor.

                                                *fugitive_U*
U                       Unstage everything.

                                                *fugitive_X*
X                       Discard the change under the cursor.  This uses
                        `checkout` or `clean` under the hood.  A command is
                        echoed that shows how to undo the change.  Consult
                        `:messages` to see it again.  During a merge conflict,
                        use 2X to call `checkout --ours` or 3X to call
                        `checkout --theirs` .

                                                *fugitive_=*
=                       Toggle an inline diff of the file under the cursor.

                                                *fugitive_>*
>                       Insert an inline diff of the file under the cursor.

                                                *fugitive_<*
<                       Remove the inline diff of the file under the cursor.

                                                *fugitive_gI*
gI                      Open .git/info/exclude in a split and add the file
                        under the cursor.  Use a count to open .gitignore.

                                                *fugitive_I*
I                       Invoke |:Git| add --patch or reset --patch on the file
P                       under the cursor. On untracked files, this instead
                        calls |:Git| add --intent-to-add.

                                                *fugitive_d*
Diff maps ~
                                                *fugitive_dp*
dp                      Invoke |:Git| diff on the file under the cursor.
                        Deprecated in favor of inline diffs.

                                                *fugitive_dd*
dd                      Perform a |:Gdiffsplit| on the file under the cursor.

                                                *fugitive_dv*
dv                      Perform a |:Gvdiffsplit| on the file under the cursor.

                                                *fugitive_ds* *fugitive_dh*
ds                      Perform a |:Ghdiffsplit| on the file under the cursor.
dh

                                                *fugitive_dq*
dq                      Close all but the currently focused diff buffer, and
                        invoke |:diffoff|!.

                                                *fugitive_d?*
d?                      Show this help.

                                                *fugitive-navigation-maps*
Navigation maps ~

                                                *fugitive_<CR>*
<CR>                    Open the file or |fugitive-object| under the cursor.
                        In a blob, this and similar maps jump to the patch
                        from the diff where this was added, or where it was
                        removed if a count was given.  If the line is still in
                        the work tree version, passing a count takes you to
                        it.

                                                *fugitive_o*
o                       Open the file or |fugitive-object| under the cursor in
                        a new split.

                                                *fugitive_gO*
gO                      Open the file or |fugitive-object| under the cursor in
                        a new vertical split.

                                                *fugitive_O*
O                       Open the file or |fugitive-object| under the cursor in
                        a new tab.

                                                *fugitive_p*
p                       Open the file or |fugitive-object| under the cursor in
                        a preview window.  In the status buffer, 1p is
                        required to bypass the legacy usage instructions.

                                                *fugitive_~*
~                       Open the current file in the [count]th first ancestor.

                                                *fugitive_P*
P                       Open the current file in the [count]th parent.
                        Experimental:  In the "Unpushed" section of the status
                        buffer, this will populate the command line with a
                        ":Git push" command for the commit under the cursor.

                                                *fugitive_C*
C                       Open the commit containing the current file.

                                                *fugitive_CTRL-P* *fugitive_(*
(                       Jump to the previous file, hunk, or revision.

                                                *fugitive_CTRL-N* *fugitive_)*
)                       Jump to the next file, hunk, or revision.

                                                *fugitive_[c*
[c                      Jump to previous hunk, expanding inline diffs
                        automatically.  (This shadows the Vim built-in |[c|
                        that provides a similar operation in |diff| mode.)

                                                *fugitive_]c*
]c                      Jump to next hunk, expanding inline diffs
                        automatically.  (This shadows the Vim built-in |]c|
                        that provides a similar operation in |diff| mode.)

                                                *fugitive_[/* *fugitive_[m*
[/                      Jump to previous file, collapsing inline diffs
[m                      automatically.  (Mnemonic: "/" appears in filenames,
                        "m" appears in "filenames".)

                                                *fugitive_]/* *fugitive_]m*
]/                      Jump to next file, collapsing inline diffs
]m                      automatically.  (Mnemonic: "/" appears in filenames,
                        "m" appears in "filenames".)

                                                *fugitive_i*
i                       Jump to the next file or hunk, expanding inline diffs
                        automatically.

                                                *fugitive_[[*
[[                      Jump [count] sections backward.

                                                *fugitive_]]*
]]                      Jump [count] sections forward.

                                                *fugitive_[]*
[]                      Jump [count] section ends backward.

                                                *fugitive_][*
][                      Jump [count] section ends forward.

                                                *fugitive_star*
*                       On the first column of a + or - diff line, search for
                        the corresponding - or + line.  Otherwise, defer to
                        built-in |star|.

                                                *fugitive_#*
#                       Same as "*", but search backward.

                                                *fugitive_gu*
gu                      Jump to file [count] in the "Untracked" or "Unstaged"
                        section.

                                                *fugitive_gU*
gU                      Jump to file [count] in the "Unstaged" section.

                                                *fugitive_gs*
gs                      Jump to file [count] in the "Staged" section.

                                                *fugitive_gp*
gp                      Jump to file [count] in the "Unpushed" section.

                                                *fugitive_gP*
gP                      Jump to file [count] in the "Unpulled" section.

                                                *fugitive_gr*
gr                      Jump to file [count] in the "Rebasing" section.

                                                *fugitive_gi*
gi                      Open .git/info/exclude in a split.  Use a count to
                        open .gitignore.

                                                *fugitive_c*
Commit maps ~

cc                      Create a commit.

ca                      Amend the last commit and edit the message.

ce                      Amend the last commit without editing the message.

cw                      Reword the last commit.

cvc                     Create a commit with -v.

cva                     Amend the last commit with -v

cf                      Create a `fixup!` commit for the commit under the
                        cursor.

cF                      Create a `fixup!` commit for the commit under the
                        cursor and immediately rebase it.

cs                      Create a `squash!` commit for the commit under the
                        cursor.

cS                      Create a `squash!` commit for the commit under the
                        cursor and immediately rebase it.

cA                      Create a `squash!` commit for the commit under the
                        cursor and edit the message.

c<Space>                Populate command line with ":Git commit ".

                                                *fugitive_cr*
crc                     Revert the commit under the cursor.

crn                     Revert the commit under the cursor in the index and
                        work tree, but do not actually commit the changes.

cr<Space>               Populate command line with ":Git revert ".

                                                *fugitive_cm*
cm<Space>               Populate command line with ":Git merge ".

c?                      Show this help.

                                                *fugitive_cb*
                                                *fugitive_co*
Checkout/branch maps ~

coo                     Check out the commit under the cursor.

cb<Space>               Populate command line with ":Git branch ".

co<Space>               Populate command line with ":Git checkout ".

cb?                     Show this help.
co?

                                                *fugitive_cz*
Stash maps ~

czz                     Push stash.  Pass a [count] of 1 to add
                        `--include-untracked` or 2 to add `--all`.

czw                     Push stash of the work-tree.  Like `czz` with
                        `--keep-index`.

czs                     Push stash of the stage.  Does not accept a count.

czA                     Apply topmost stash, or stash@{count}.

cza                     Apply topmost stash, or stash@{count}, preserving the
                        index.

czP                     Pop topmost stash, or stash@{count}.

czp                     Pop topmost stash, or stash@{count}, preserving the
                        index.

cz<Space>               Populate command line with ":Git stash ".

cz?                     Show this help.

                                                *fugitive_r*
Rebase maps ~

ri                      Perform an interactive rebase.  Uses ancestor of
u                       commit under cursor as upstream if available.

rf                      Perform an autosquash rebase without editing the todo
                        list.  Uses ancestor of commit under cursor as
                        upstream if available.

ru                      Perform an interactive rebase against @{upstream}.

rp                      Perform an interactive rebase against @{push}.

rr                      Continue the current rebase.

rs                      Skip the current commit and continue the current
                        rebase.

ra                      Abort the current rebase.

re                      Edit the current rebase todo list.

rw                      Perform an interactive rebase with the commit under
                        the cursor set to `reword`.

rm                      Perform an interactive rebase with the commit under
                        the cursor set to `edit`.

rd                      Perform an interactive rebase with the commit under
                        the cursor set to `drop`.

r<Space>                Populate command line with ":Git rebase ".

r?                      Show this help.

                                                *fugitive-misc-maps*
Miscellaneous maps ~

                                                *fugitive_gq* *fugitive_q*
gq                      Close the status buffer.

                                                *fugitive_.*
.                       Start a |:| command line with the file under the
                        cursor prepopulated.

                                                *fugitive_g?*
g?                      Show help for |fugitive-maps|.

                                                *fugitive-global-maps*
Global maps ~

                                                *fugitive_c_CTRL-R_CTRL-G*
<C-R><C-G>              On the command line, recall the path to the current
                        |fugitive-object| (that is, a representation of the
                        object recognized by |:Gedit|).

                                                *fugitive_y_CTRL-G*
["x]y<C-G>              Yank the path to the current |fugitive-object|.

                                                *g:fugitive_no_maps*
Global maps can be disabled with the g:fugitive_no_maps option.
>
        let g:fugitive_no_maps = 1
<
SPECIFYING OBJECTS                      *fugitive-object* *fugitive-revision*

Fugitive objects are either work tree files or Git revisions as defined in the
"SPECIFYING REVISIONS" section in the git-rev-parse man page, with expansions
inspired by |cmdline-special| layered on top.  For commands that accept an
optional object, the default is the file in the index for work tree files and
the work tree file for everything else.  Example objects follow.

Object          Meaning ~
@               The commit referenced by @ aka HEAD
master          The commit referenced by master
master^         The parent of the commit referenced by master
master...other  The merge base of master and other
master:         The tree referenced by master
./master        The file named master in the working directory
:(top)master    The file named master in the work tree
Makefile        The file named Makefile in the work tree
@^:Makefile     The file named Makefile in the parent of HEAD
:Makefile       The file named Makefile in the index (writable)
@~2:%           The current file in the grandparent of HEAD
:%              The current file in the index
:1:%            The current file's common ancestor during a conflict
:2:#            The alternate file in the target branch during a conflict
:3:#5           The file from buffer #5 in the merged branch during a conflict
!               The commit owning the current file
!:Makefile      The file named Makefile in the commit owning the current file
!3^2            The second parent of the commit owning buffer #3
.git/config     The repo config file
:               The |fugitive-summary| buffer
-               A temp file containing the last |:Git| invocation's output
<cfile>         The file or commit under the cursor

STATUSLINE                                      *fugitive-statusline*

                                *FugitiveStatusline()* *fugitive#statusline()*
Add %{FugitiveStatusline()} to your statusline to get an indicator including
the current branch and the currently edited file's commit.  If you don't have
a statusline, this one matches the default when 'ruler' is set:
>
        set statusline=%<%f\ %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%)\ %P
<
AUTOCOMMANDS                                    *fugitive-autocommands*

A handful of |User| |autocommands| are provided to allow extending and
overriding Fugitive behaviors.  Example usage:
>
        autocmd User FugitiveBlob,FugitiveStageBlob call s:BlobOverrides()
<
                                                *User_FugitiveTag*
FugitiveTag             After loading a tag object.

                                                *User_FugitiveCommit*
FugitiveCommit          After loading a commit object.

                                                *User_FugitiveTree*
FugitiveTree            After loading a tree (directory) object.

                                                *User_FugitiveBlob*
FugitiveBlob            After loading a committed blob (file) object.

                                                *User_FugitiveObject*
FugitiveObject          After loading any of the 4 above buffer types.

                                                *User_FugitiveStageBlob*
FugitiveStageBlob       After loading a staged blob (file) object.  These
                        buffers are 'modifiable' and oftentimes don't want the
                        same behavior as the other buffer types.

                                                *User_FugitiveIndex*
FugitiveIndex           After loading the |fugitive-summary| buffer.

                                                *User_FugitivePager*
FugitivePager           After loading a temp file created by a command like
                        :Git --paginate or :Git blame.

                                                *User_FugitiveEditor*
FugitiveEditor          After a :Git command (e.g., :Git commit) edits a file
                        (e.g., the commit message).

                                                *User_FugitiveChanged*
FugitiveChanged         After any event which can potentially change the
                        repository, for example, any invocation of |:Git|.
                        Originally intended for expiring caches, but can have
                        other uses.

API                                             *fugitive-api*

Officially supported functions are documented inline in plugin/fugitive.vim.

DEPRECATIONS                                    *fugitive-deprecated*

The following commands are deprecated in favor of replacements that adhere to
a new naming scheme.  Remember that |:Git| can be shortened to |:G|, so
replacements using it are just one space character longer than the legacy
version.

*:Gremove*      Superseded by |:GRemove|.
*:Gdelete*      Superseded by |:GDelete|.
*:Gmove*        Superseded by |:GMove|.
*:Grename*      Superseded by |:GRename|.
*:Gbrowse*      Superseded by |:GBrowse|.
*:Gdiff*        Superseded by |:Gdiffsplit|
*:Gsdiff*       Superseded by |:Ghdiffsplit|
*:Gvdiff*       Superseded by |:Gvdiffsplit| or |:vert| |:Gdiffsplit|.
*:Gblame*       Superseded by |:Git_blame|.
*:Gcommit*      Superseded by |:Git| commit.
*:Gmerge*       Superseded by |:Git| merge and |:Git_mergetool|.
*:Gpull*        Superseded by |:Git| pull.
*:Grebase*      Superseded by |:Git| rebase.
*:Grevert*      Superseded by |:Git| revert.
*:Gpush*        Superseded by |:Git| push.
*:Gfetch*       Superseded by |:Git| fetch.
*:Glog*         Superseded by |:Gclog|.
*:Gstatus*      Superseded by |:Git| (with no arguments).
*:Gsplit!*      Superseded by |:Git_--paginate|.
*:Gvsplit!*     Superseded by :vert Git --paginate.
*:Gtabsplit!*   Superseded by :tab Git --paginate.
*:Gpedit!*      Superseded by :Git! --paginate.

                                                *User_Fugitive*
Fugitive used to support `:autocmd User Fugitive` to run an autocommand after
loading any buffer belonging to a Git repository, but this has been phased
out.  Instead, one can leverage regular autocommand events like |BufNewFile|
and |BufReadPost|, and check !empty(FugitiveGitDir()) to confirm Fugitive has
found a repository.  See also |fugitive-autocommands| for other, more
selective events.

ABOUT                                           *fugitive-about*

Grab the latest version or report a bug on GitHub:

https://github.com/tpope/vim-fugitive

 vim:tw=78:et:ft=help:norl:
