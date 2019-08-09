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
        self.rank = 100
        self.is_bytepos = True
        self.min_pattern_length = 1

    # Returns an integer for the start position, as with omnifunc.
    def get_completion_position(self):
        return self.vim.call('ale#completion#GetCompletionPosition')

    def gather_candidates(self, context):
        # Stop early if ALE can't provide completion data for this buffer.
        if not self.vim.call('ale#completion#CanProvideCompletions'):
            return None

        if context.get('is_refresh'):
            context['is_async'] = False

        if context['is_async']:
            # Result is the same as for omnifunc, or None.
            result = self.vim.call('ale#completion#GetCompletionResult')

            if result is not None:
                context['is_async'] = False

                return result
        else:
            context['is_async'] = True

            # Request some completion results.
            self.vim.call('ale#completion#GetCompletions', 'deoplete')

        return []
