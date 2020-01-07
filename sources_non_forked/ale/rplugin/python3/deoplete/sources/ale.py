"""
A Deoplete source for ALE completion via tsserver and LSP.
"""
__author__ = 'Joao Paulo, w0rp'

try:
    from deoplete.source.base import Base
except ImportError:
    # Mock the Base class if deoplete isn't available, as mock isn't available
    # in the Docker image.
    class Base(object):
        def __init__(self, vim):
            pass


# Make sure this code is valid in Python 2, used for running unit tests.
class Source(Base):

    def __init__(self, vim):
        super(Source, self).__init__(vim)

        self.name = 'ale'
        self.mark = '[L]'
        self.rank = 1000
        self.is_bytepos = True
        self.min_pattern_length = 1
        self.is_volatile = True
        # Do not forget to update s:trigger_character_map in completion.vim in
        # updating entries in this map.
        self.input_patterns = {
            '_': r'\.\w*$',
            'rust': r'(\.|::)\w*$',
            'typescript': r'(\.|\'|")\w*$',
            'cpp': r'(\.|::|->)\w*$',
        }

    # Returns an integer for the start position, as with omnifunc.
    def get_complete_position(self, context):
        return self.vim.call(
            'ale#completion#GetCompletionPositionForDeoplete', context['input']
        )

    def gather_candidates(self, context):
        # Stop early if ALE can't provide completion data for this buffer.
        if not self.vim.call('ale#completion#CanProvideCompletions'):
            return None

        event = context.get('event')

        if event == 'Async':
            result = self.vim.call('ale#completion#GetCompletionResult')
            return result or []

        if context.get('is_refresh'):
            self.vim.command(
                "call ale#completion#GetCompletions('ale-callback', " + \
                "{'callback': {completions -> deoplete#auto_complete() }})"
            )

        return []
