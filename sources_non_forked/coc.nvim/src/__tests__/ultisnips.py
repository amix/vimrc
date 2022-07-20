import re, os, vim, string, random
from collections import deque, namedtuple

_Placeholder = namedtuple("_FrozenPlaceholder", ["current_text", "start", "end"])
_VisualContent = namedtuple("_VisualContent", ["mode", "text"])
_Position = namedtuple("_Position", ["line", "col"])


class _SnippetUtilCursor(object):
    def __init__(self, cursor):
        self._cursor = [cursor[0] - 1, cursor[1]]
        self._set = False

    def preserve(self):
        self._set = True
        self._cursor = [vim.buf.cursor[0], vim.buf.cursor[1]]

    def is_set(self):
        return self._set

    def set(self, line, column):
        self.__setitem__(0, line)
        self.__setitem__(1, column)

    def to_vim_cursor(self):
        return (self._cursor[0] + 1, self._cursor[1])

    def __getitem__(self, index):
        return self._cursor[index]

    def __setitem__(self, index, value):
        self._set = True
        self._cursor[index] = value

    def __len__(self):
        return 2

    def __str__(self):
        return str((self._cursor[0], self._cursor[1]))


class IndentUtil(object):

    """Utility class for dealing properly with indentation."""

    def __init__(self):
        self.reset()

    def reset(self):
        """Gets the spacing properties from Vim."""
        self.shiftwidth = int(
            vim.eval("exists('*shiftwidth') ? shiftwidth() : &shiftwidth")
        )
        self._expandtab = vim.eval("&expandtab") == "1"
        self._tabstop = int(vim.eval("&tabstop"))

    def ntabs_to_proper_indent(self, ntabs):
        """Convert 'ntabs' number of tabs to the proper indent prefix."""
        line_ind = ntabs * self.shiftwidth * " "
        line_ind = self.indent_to_spaces(line_ind)
        line_ind = self.spaces_to_indent(line_ind)
        return line_ind

    def indent_to_spaces(self, indent):
        """Converts indentation to spaces respecting Vim settings."""
        indent = indent.expandtabs(self._tabstop)
        right = (len(indent) - len(indent.rstrip(" "))) * " "
        indent = indent.replace(" ", "")
        indent = indent.replace("\t", " " * self._tabstop)
        return indent + right

    def spaces_to_indent(self, indent):
        """Converts spaces to proper indentation respecting Vim settings."""
        if not self._expandtab:
            indent = indent.replace(" " * self._tabstop, "\t")
        return indent


class SnippetUtil(object):

    """Provides easy access to indentation, etc.

    This is the 'snip' object in python code.

    """

    def __init__(self, _initial_indent, start, end, context):
        self._ind = IndentUtil()
        self._visual = _VisualContent(
            vim.eval("visualmode()"), vim.eval('get(g:,"coc_selected_text","")')
        )
        self._initial_indent = _initial_indent
        self._reset("")
        self._start = start
        self._end = end
        self._context = context

    def _reset(self, cur):
        """Gets the snippet ready for another update.

        :cur: the new value for c.

        """
        self._ind.reset()
        self._cur = cur
        self._rv = ""
        self._changed = False
        self.reset_indent()

    def shift(self, amount=1):
        """Shifts the indentation level. Note that this uses the shiftwidth
        because thats what code formatters use.

        :amount: the amount by which to shift.

        """
        self.indent += " " * self._ind.shiftwidth * amount

    def unshift(self, amount=1):
        """Unshift the indentation level. Note that this uses the shiftwidth
        because thats what code formatters use.

        :amount: the amount by which to unshift.

        """
        by = -self._ind.shiftwidth * amount
        try:
            self.indent = self.indent[:by]
        except IndexError:
            self.indent = ""

    def mkline(self, line="", indent=None):
        """Creates a properly set up line.

        :line: the text to add
        :indent: the indentation to have at the beginning
                 if None, it uses the default amount

        """
        if indent is None:
            indent = self.indent
            # this deals with the fact that the first line is
            # already properly indented
            if "\n" not in self._rv:
                try:
                    indent = indent[len(self._initial_indent) :]
                except IndexError:
                    indent = ""
            indent = self._ind.spaces_to_indent(indent)

        return indent + line

    def reset_indent(self):
        """Clears the indentation."""
        self.indent = self._initial_indent

    # Utility methods
    @property
    def fn(self):  # pylint:disable=no-self-use,invalid-name
        """The filename."""
        return vim.eval('expand("%:t")') or ""

    @property
    def basename(self):  # pylint:disable=no-self-use
        """The filename without extension."""
        return vim.eval('expand("%:t:r")') or ""

    @property
    def ft(self):  # pylint:disable=invalid-name
        """The filetype."""
        return self.opt("&filetype", "")

    @property
    def rv(self):  # pylint:disable=invalid-name
        """The return value.

        The text to insert at the location of the placeholder.

        """
        return self._rv

    @rv.setter
    def rv(self, value):  # pylint:disable=invalid-name
        """See getter."""
        self._changed = True
        self._rv = value

    @property
    def _rv_changed(self):
        """True if rv has changed."""
        return self._changed

    @property
    def c(self):  # pylint:disable=invalid-name
        """The current text of the placeholder."""
        return self._cur

    @property
    def v(self):  # pylint:disable=invalid-name
        """Content of visual expansions."""
        return self._visual

    @property
    def p(self):
        if "coc_last_placeholder" in vim.vars:
            p = vim.vars["coc_last_placeholder"]
            start = _Position(p["start"]["line"], p["start"]["col"])
            end = _Position(p["end"]["line"], p["end"]["col"])
            return _Placeholder(p["current_text"], start, end)
        return None

    @property
    def context(self):
        return self._context

    def opt(self, option, default=None):  # pylint:disable=no-self-use
        """Gets a Vim variable."""
        if vim.eval("exists('%s')" % option) == "1":
            try:
                return vim.eval(option)
            except vim.error:
                pass
        return default

    def __add__(self, value):
        """Appends the given line to rv using mkline."""
        self.rv += "\n"  # pylint:disable=invalid-name
        self.rv += self.mkline(value)
        return self

    def __lshift__(self, other):
        """Same as unshift."""
        self.unshift(other)

    def __rshift__(self, other):
        """Same as shift."""
        self.shift(other)

    @property
    def snippet_start(self):
        """
        Returns start of the snippet in format (line, column).
        """
        return self._start

    @property
    def snippet_end(self):
        """
        Returns end of the snippet in format (line, column).
        """
        return self._end

    @property
    def buffer(self):
        return vim.buf


class ContextSnippet(object):
    def __init__(self):
        self.buffer = vim.current.buffer
        self.window = vim.current.window
        self.cursor = _SnippetUtilCursor(vim.current.window.cursor)
        self.line = vim.current.window.cursor[0] - 1
        self.column = vim.current.window.cursor[1] - 1
        self.before = vim.eval('strpart(getline("."), 0, col(".") - 1)')
        line = vim.eval('line(".")')
        self.after = line[self.column :]
        if "coc_selected_text" in vim.vars:
            self.visual_mode = vim.eval("visualmode()")
            self.visual_text = vim.vars["coc_selected_text"]
        else:
            self.visual_mode = None
            self.visual_text = ""
        if "coc_last_placeholder" in vim.vars:
            p = vim.vars["coc_last_placeholder"]
            start = _Position(p["start"]["line"], p["start"]["col"])
            end = _Position(p["end"]["line"], p["end"]["col"])
            self.last_placeholder = _Placeholder(p["current_text"], start, end)
        else:
            self.last_placeholder = None
