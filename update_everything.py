import os
import shutil
import subprocess

paths = [
    'sources_plugins/vim-snipmate',
    'sources_plugins/vim-addon-mw-utils',
    'sources_plugins/tlib',
    'sources_plugins/vim-snippets',
    'sources_plugins/bufexplorer.zip',
    'sources_plugins/nerdtree',
    'sources_plugins/vim-surround',
    'sources_plugins/ack.vim',
    'sources_plugins/open_file_under_cursor.vim',
    'sources_plugins/vim-powerline',
    'sources_plugins/ctrlp.vim',
    'sources_misc/vim-coffee-script',
    'sources_misc/vim-less',
    'sources_misc/vim-bundle-mako',
    'sources_colors/vim-colors-solarized',
    'sources_colors/mayansmoke',
    'sources_colors/vim-pyte',
    'sources_misc/vim-markdown'
]

vim_runtime_path = os.path.expanduser('~/.vim_runtime')

for path in paths:
    full_path = os.path.join(vim_runtime_path, path)
    shutil.rmtree(full_path, True)
    os.chdir(vim_runtime_path)

subprocess.call(['git', 'pull', '--rebase'])
subprocess.call(['git', 'submodule', 'init'])
subprocess.call(['git', 'submodule', 'update'])
