## 0.8.1 (Apr 15, 2014)
- Updates the URL from which Node.vim downloads Node core module source files.  
  Uses <http://rawgit.com> which used to be named <http://rawgithub.com>.  
  Because of Vim Netrw's inability to handle HTTPS, it does so over HTTP. Sorry.

## 0.8.0 (Sep 6, 2013)
- Adds `node` as a core module so you could use `:Nedit node` to open the file Node uses to bootstrap its core.

## 0.7.0 (Aug 28, 2013)
- Adds support for opening core Node.js modules, such as `http`, `util`, etc. with `gf` or `:Nedit`.
  They're shown straight from Node's online repository without you having to download everything.

## 0.6.0 (Aug 23, 2013)
- Adds `:Nedit` command for editing modules or files relative to the Node project root.
  For example: `:Nedit any-module/lib` or `:Nedit ./package`.
- Adds `:Nopen` command which behaves like `:Nedit`, but also `lcd`s to the module's directory.
- Makes `<Plug>NodeGotoFile` available for your mapping in any Node project file, but maps it to `gf` automatically only on JavaScript files.
- Maps `gf` also for JSON files for easy jumping to modules.
- Makes `:Nedit` and `:Nopen` available immediately when starting Vim in a directory of a Node project.

## 0.5.1 (Aug 8, 2013)
- Adds `Node` autocommand.  
  Use it with `autocmd User Node` to customize settings for files in Node projects.
- Adds `<Plug>NodeVSplitGotoFile` for those who want `<C-w>f` to split vertically.

## 0.5.0 (Aug 5, 2013)
- Adds `&include` pattern so Vim can recognize included/required files, e.g. for looking up keywords with `[I`.
- Cleans `&path` from `/usr/include` for JavaScript files.
- Adds a new superb `gf` handler to handle all relative and module paths, incl. support for `require(".")` to open `./index.js`. This is spot on how Node.js finds your requires.
- Adds `<Plug>NodeGotoFile` should you want to remap Node.vim's file opener.
- Opens files before directories should both, e.g. `./foo.js` and `./foo`, exist. This matches Node.js's behavior.
- Adds a full automated integration test suite to Node.vim which is freaking amazing!

## 0.2.0 (Jul 28, 2013)
- Adds full support for navigating to module files by using `gf` on `require("any-module")`.
- Adds `.json` to `&suffixesadd` so you could use `gf` on `require("./package")` to open package.json.

## 0.1.1 (Jul 28, 2013)
- Removes an innocent but forgotten debugging line.

## 0.1.0 (Jul 28, 2013)
- First release to get the nodeballs rolling.
- Sets the filetype to JavaScript for files with Node's shebang (`#!`).
- Adds `.js` to `&suffixesadd` so you could use `gf` on `require("./foo")` to open `foo.js`.
