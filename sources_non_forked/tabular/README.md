Tabular
==============
Sometimes, it's useful to line up text.  Naturally, it's nicer to have the
computer do this for you, since aligning things by hand quickly becomes
unpleasant.  While there are other plugins for aligning text, the ones I've
tried are either impossibly difficult to understand and use, or too simplistic
to handle complicated tasks.  This plugin aims to make the easy things easy
and the hard things possible, without providing an unnecessarily obtuse
interface.  It's still a work in progress, and criticisms are welcome.

See [Aligning Text with Tabular.vim](http://vimcasts.org/episodes/aligning-text-with-tabular-vim/)
for a screencast that shows how Tabular.vim works.

See [doc/Tabular.txt](http://raw.github.com/godlygeek/tabular/master/doc/Tabular.txt)
for detailed documentation.

Installation
==============
If you don't have a preferred installation method, I recommend installing
[pathogen.vim](https://github.com/tpope/vim-pathogen), and then simply
copy and paste:

    mkdir -p ~/.vim/bundle
    cd ~/.vim/bundle
    git clone git://github.com/godlygeek/tabular.git

Once help tags have been generated (either using Pathogen's `:Helptags`
command, or by pointing vim's `:helptags` command at the directory where you
installed Tabular), you can view the manual with `:help tabular`.
