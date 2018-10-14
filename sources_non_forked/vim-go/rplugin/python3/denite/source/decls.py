# ============================================================================
# FILE: decls.py
# AUTHOR: delphinus <delphinus@remora.cx>
# License: MIT license
# ============================================================================

import os
import subprocess
import json
import denite.util
from .base import Base

DECLS_SYNTAX_HIGHLIGHT = [
    {'name': 'FilePath', 're': r'[^:]*\ze:', 'link': 'Comment'},
    {'name': 'Line', 're': r'\d\+\ze :', 'link': 'LineNr'},
    {'name': 'WholeFunction', 're': r'\vfunc %(\([^)]+\) )?[^(]+'},
    {'name': 'Function', 'parent': 'WholeFunction',
     're': r'\S\+\ze(', 'link': 'Function'},
    {'name': 'WholeType', 're': r'type \S\+'},
    {'name': 'Type', 'parent': 'WholeType',
     're': r'\v( )@<=\S+', 'link': 'Type'},
    {'name': 'Separator', 're': r':', 'conceal': True},
    {'name': 'SeparatorFunction', 'parent': 'WholeFunction',
     're': r'func ', 'conceal': True},
    {'name': 'SeparatorType', 'parent': 'WholeType',
     're': r'type ', 'conceal': True},
    ]

class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)

        self.name = 'decls'
        self.kind = 'file'

    def gather_candidates(self, context):
        bin_path = self.vim.call('go#path#CheckBinPath', 'motion')
        if bin_path == '':
            return []

        expand = context['args'][0] if context['args'] else '%:p:h'
        target = self.vim.funcs.expand(expand)

        if os.path.isdir(target):
            mode = 'dir'
        elif os.path.isfile(target):
            mode = 'file'
        else:
            return []

        if self.vim.funcs.exists('g:go_decls_includes'):
            include = self.vim.eval('g:go_decls_includes')
        else:
            include = 'func,type'

        command = [bin_path, '-mode', 'decls', '-include', include,
                   '-' + mode, target]

        try:
            cmd = subprocess.run(command, stdout=subprocess.PIPE, check=True)
        except subprocess.CalledProcessError as err:
            denite.util.error(self.vim,
                              'command returned invalid response: ' + str(err))
            return []

        txt = cmd.stdout.decode('utf-8')
        output = json.loads(txt, encoding='utf-8')

        def make_candidates(row):
            name = self.vim.funcs.fnamemodify(row['filename'], ':~:.')
            return {
                'word': '{0} :{1} :{2}'.format(name, row['line'], row['full']),
                'action__path': row['filename'],
                'action__line': row['line'],
                'action__col': row['col'],
                }
        return list(map(make_candidates, output['decls']))

    def highlight(self):
        for syn in DECLS_SYNTAX_HIGHLIGHT:
            containedin = self.syntax_name
            containedin += '_' + syn['parent'] if 'parent' in syn else ''
            conceal = ' conceal' if 'conceal' in syn else ''

            self.vim.command(
                'syntax match {0}_{1} /{2}/ contained containedin={3}{4}'
                .format(self.syntax_name, syn['name'], syn['re'],
                        containedin, conceal))

            if 'link' in syn:
                self.vim.command('highlight default link {0}_{1} {2}'.format(
                    self.syntax_name, syn['name'], syn['link']))
