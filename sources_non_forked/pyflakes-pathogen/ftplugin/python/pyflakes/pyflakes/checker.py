# -*- test-case-name: pyflakes -*-
# (c) 2005-2010 Divmod, Inc.
# See LICENSE file for details

import __builtin__
import os.path
import _ast

from pyflakes import messages


# utility function to iterate over an AST node's children, adapted
# from Python 2.6's standard ast module
try:
    import ast
    iter_child_nodes = ast.iter_child_nodes
except (ImportError, AttributeError):
    def iter_child_nodes(node, astcls=_ast.AST):
        """
        Yield all direct child nodes of *node*, that is, all fields that are nodes
        and all items of fields that are lists of nodes.
        """
        for name in node._fields:
            field = getattr(node, name, None)
            if isinstance(field, astcls):
                yield field
            elif isinstance(field, list):
                for item in field:
                    yield item


class Binding(object):
    """
    Represents the binding of a value to a name.

    The checker uses this to keep track of which names have been bound and
    which names have not. See L{Assignment} for a special type of binding that
    is checked with stricter rules.

    @ivar used: pair of (L{Scope}, line-number) indicating the scope and
                line number that this binding was last used
    """

    def __init__(self, name, source):
        self.name = name
        self.source = source
        self.used = False


    def __str__(self):
        return self.name


    def __repr__(self):
        return '<%s object %r from line %r at 0x%x>' % (self.__class__.__name__,
                                                        self.name,
                                                        self.source.lineno,
                                                        id(self))



class UnBinding(Binding):
    '''Created by the 'del' operator.'''



class Importation(Binding):
    """
    A binding created by an import statement.

    @ivar fullName: The complete name given to the import statement,
        possibly including multiple dotted components.
    @type fullName: C{str}
    """
    def __init__(self, name, source):
        self.fullName = name
        name = name.split('.')[0]
        super(Importation, self).__init__(name, source)



class Argument(Binding):
    """
    Represents binding a name as an argument.
    """



class Assignment(Binding):
    """
    Represents binding a name with an explicit assignment.

    The checker will raise warnings for any Assignment that isn't used. Also,
    the checker does not consider assignments in tuple/list unpacking to be
    Assignments, rather it treats them as simple Bindings.
    """



class FunctionDefinition(Binding):
    pass



class ExportBinding(Binding):
    """
    A binding created by an C{__all__} assignment.  If the names in the list
    can be determined statically, they will be treated as names for export and
    additional checking applied to them.

    The only C{__all__} assignment that can be recognized is one which takes
    the value of a literal list containing literal strings.  For example::

        __all__ = ["foo", "bar"]

    Names which are imported and not otherwise used but appear in the value of
    C{__all__} will not have an unused import warning reported for them.
    """
    def names(self):
        """
        Return a list of the names referenced by this binding.
        """
        names = []
        if isinstance(self.source, _ast.List):
            for node in self.source.elts:
                if isinstance(node, _ast.Str):
                    names.append(node.s)
        return names



class Scope(dict):
    importStarred = False       # set to True when import * is found


    def __repr__(self):
        return '<%s at 0x%x %s>' % (self.__class__.__name__, id(self), dict.__repr__(self))


    def __init__(self):
        super(Scope, self).__init__()



class ClassScope(Scope):
    pass



class FunctionScope(Scope):
    """
    I represent a name scope for a function.

    @ivar globals: Names declared 'global' in this function.
    """
    def __init__(self):
        super(FunctionScope, self).__init__()
        self.globals = {}



class ModuleScope(Scope):
    pass


# Globally defined names which are not attributes of the __builtin__ module.
_MAGIC_GLOBALS = ['__file__', '__builtins__']



class Checker(object):
    """
    I check the cleanliness and sanity of Python code.

    @ivar _deferredFunctions: Tracking list used by L{deferFunction}.  Elements
        of the list are two-tuples.  The first element is the callable passed
        to L{deferFunction}.  The second element is a copy of the scope stack
        at the time L{deferFunction} was called.

    @ivar _deferredAssignments: Similar to C{_deferredFunctions}, but for
        callables which are deferred assignment checks.
    """

    nodeDepth = 0
    traceTree = False

    def __init__(self, tree, filename='(none)'):
        self._deferredFunctions = []
        self._deferredAssignments = []
        self.dead_scopes = []
        self.messages = []
        self.filename = filename
        self.scopeStack = [ModuleScope()]
        self.futuresAllowed = True
        self.handleChildren(tree)
        self._runDeferred(self._deferredFunctions)
        # Set _deferredFunctions to None so that deferFunction will fail
        # noisily if called after we've run through the deferred functions.
        self._deferredFunctions = None
        self._runDeferred(self._deferredAssignments)
        # Set _deferredAssignments to None so that deferAssignment will fail
        # noisly if called after we've run through the deferred assignments.
        self._deferredAssignments = None
        del self.scopeStack[1:]
        self.popScope()
        self.check_dead_scopes()


    def deferFunction(self, callable):
        '''
        Schedule a function handler to be called just before completion.

        This is used for handling function bodies, which must be deferred
        because code later in the file might modify the global scope. When
        `callable` is called, the scope at the time this is called will be
        restored, however it will contain any new bindings added to it.
        '''
        self._deferredFunctions.append((callable, self.scopeStack[:]))


    def deferAssignment(self, callable):
        """
        Schedule an assignment handler to be called just after deferred
        function handlers.
        """
        self._deferredAssignments.append((callable, self.scopeStack[:]))


    def _runDeferred(self, deferred):
        """
        Run the callables in C{deferred} using their associated scope stack.
        """
        for handler, scope in deferred:
            self.scopeStack = scope
            handler()


    def scope(self):
        return self.scopeStack[-1]
    scope = property(scope)

    def popScope(self):
        self.dead_scopes.append(self.scopeStack.pop())


    def check_dead_scopes(self):
        """
        Look at scopes which have been fully examined and report names in them
        which were imported but unused.
        """
        for scope in self.dead_scopes:
            export = isinstance(scope.get('__all__'), ExportBinding)
            if export:
                all = scope['__all__'].names()
                if os.path.split(self.filename)[1] != '__init__.py':
                    # Look for possible mistakes in the export list
                    undefined = set(all) - set(scope)
                    for name in undefined:
                        self.report(
                            messages.UndefinedExport,
                            scope['__all__'].source,
                            name)
            else:
                all = []

            # Look for imported names that aren't used.
            for importation in scope.itervalues():
                if isinstance(importation, Importation):
                    if not importation.used and importation.name not in all:
                        self.report(
                            messages.UnusedImport,
                            importation.source,
                            importation.name)


    def pushFunctionScope(self):
        self.scopeStack.append(FunctionScope())

    def pushClassScope(self):
        self.scopeStack.append(ClassScope())

    def report(self, messageClass, *args, **kwargs):
        self.messages.append(messageClass(self.filename, *args, **kwargs))

    def handleChildren(self, tree):
        for node in iter_child_nodes(tree):
            self.handleNode(node, tree)

    def isDocstring(self, node):
        """
        Determine if the given node is a docstring, as long as it is at the
        correct place in the node tree.
        """
        return isinstance(node, _ast.Str) or \
               (isinstance(node, _ast.Expr) and
                isinstance(node.value, _ast.Str))

    def handleNode(self, node, parent):
        node.parent = parent
        if self.traceTree:
            print '  ' * self.nodeDepth + node.__class__.__name__
        self.nodeDepth += 1
        if self.futuresAllowed and not \
               (isinstance(node, _ast.ImportFrom) or self.isDocstring(node)):
            self.futuresAllowed = False
        nodeType = node.__class__.__name__.upper()
        try:
            handler = getattr(self, nodeType)
            handler(node)
        finally:
            self.nodeDepth -= 1
        if self.traceTree:
            print '  ' * self.nodeDepth + 'end ' + node.__class__.__name__

    def ignore(self, node):
        pass

    # "stmt" type nodes
    RETURN = DELETE = PRINT = WHILE = IF = WITH = RAISE = TRYEXCEPT = \
        TRYFINALLY = ASSERT = EXEC = EXPR = handleChildren

    CONTINUE = BREAK = PASS = ignore

    # "expr" type nodes
    BOOLOP = BINOP = UNARYOP = IFEXP = DICT = SET = YIELD = COMPARE = \
    CALL = REPR = ATTRIBUTE = SUBSCRIPT = LIST = TUPLE = handleChildren

    NUM = STR = ELLIPSIS = ignore

    # "slice" type nodes
    SLICE = EXTSLICE = INDEX = handleChildren

    # expression contexts are node instances too, though being constants
    LOAD = STORE = DEL = AUGLOAD = AUGSTORE = PARAM = ignore

    # same for operators
    AND = OR = ADD = SUB = MULT = DIV = MOD = POW = LSHIFT = RSHIFT = \
    BITOR = BITXOR = BITAND = FLOORDIV = INVERT = NOT = UADD = USUB = \
    EQ = NOTEQ = LT = LTE = GT = GTE = IS = ISNOT = IN = NOTIN = ignore

    # additional node types
    COMPREHENSION = EXCEPTHANDLER = KEYWORD = handleChildren

    def addBinding(self, loc, value, reportRedef=True):
        '''Called when a binding is altered.

        - `loc` is the location (an object with lineno and optionally
          col_offset attributes) of the statement responsible for the change
        - `value` is the optional new value, a Binding instance, associated
          with the binding; if None, the binding is deleted if it exists.
        - if `reportRedef` is True (default), rebinding while unused will be
          reported.
        '''
        if (isinstance(self.scope.get(value.name), FunctionDefinition)
                    and isinstance(value, FunctionDefinition)):
            self.report(messages.RedefinedFunction,
                        loc, value.name, self.scope[value.name].source)

        if not isinstance(self.scope, ClassScope):
            for scope in self.scopeStack[::-1]:
                existing = scope.get(value.name)
                if (isinstance(existing, Importation)
                        and not existing.used
                        and (not isinstance(value, Importation) or value.fullName == existing.fullName)
                        and reportRedef):

                    self.report(messages.RedefinedWhileUnused,
                                loc, value.name, scope[value.name].source)

        if isinstance(value, UnBinding):
            try:
                del self.scope[value.name]
            except KeyError:
                self.report(messages.UndefinedName, loc, value.name)
        else:
            self.scope[value.name] = value

    def GLOBAL(self, node):
        """
        Keep track of globals declarations.
        """
        if isinstance(self.scope, FunctionScope):
            self.scope.globals.update(dict.fromkeys(node.names))

    def LISTCOMP(self, node):
        # handle generators before element
        for gen in node.generators:
            self.handleNode(gen, node)
        self.handleNode(node.elt, node)

    GENERATOREXP = SETCOMP = LISTCOMP

    # dictionary comprehensions; introduced in Python 2.7
    def DICTCOMP(self, node):
        for gen in node.generators:
            self.handleNode(gen, node)
        self.handleNode(node.key, node)
        self.handleNode(node.value, node)

    def FOR(self, node):
        """
        Process bindings for loop variables.
        """
        vars = []
        def collectLoopVars(n):
            if isinstance(n, _ast.Name):
                vars.append(n.id)
            elif isinstance(n, _ast.expr_context):
                return
            else:
                for c in iter_child_nodes(n):
                    collectLoopVars(c)

        collectLoopVars(node.target)
        for varn in vars:
            if (isinstance(self.scope.get(varn), Importation)
                    # unused ones will get an unused import warning
                    and self.scope[varn].used):
                self.report(messages.ImportShadowedByLoopVar,
                            node, varn, self.scope[varn].source)

        self.handleChildren(node)

    def NAME(self, node):
        """
        Handle occurrence of Name (which can be a load/store/delete access.)
        """
        # Locate the name in locals / function / globals scopes.
        if isinstance(node.ctx, (_ast.Load, _ast.AugLoad)):
            # try local scope
            importStarred = self.scope.importStarred
            try:
                self.scope[node.id].used = (self.scope, node)
            except KeyError:
                pass
            else:
                return

            # try enclosing function scopes

            for scope in self.scopeStack[-2:0:-1]:
                importStarred = importStarred or scope.importStarred
                if not isinstance(scope, FunctionScope):
                    continue
                try:
                    scope[node.id].used = (self.scope, node)
                except KeyError:
                    pass
                else:
                    return

            # try global scope

            importStarred = importStarred or self.scopeStack[0].importStarred
            try:
                self.scopeStack[0][node.id].used = (self.scope, node)
            except KeyError:
                if ((not hasattr(__builtin__, node.id))
                        and node.id not in _MAGIC_GLOBALS
                        and not importStarred):
                    if (os.path.basename(self.filename) == '__init__.py' and
                        node.id == '__path__'):
                        # the special name __path__ is valid only in packages
                        pass
                    else:
                        self.report(messages.UndefinedName, node, node.id)
        elif isinstance(node.ctx, (_ast.Store, _ast.AugStore)):
            # if the name hasn't already been defined in the current scope
            if isinstance(self.scope, FunctionScope) and node.id not in self.scope:
                # for each function or module scope above us
                for scope in self.scopeStack[:-1]:
                    if not isinstance(scope, (FunctionScope, ModuleScope)):
                        continue
                    # if the name was defined in that scope, and the name has
                    # been accessed already in the current scope, and hasn't
                    # been declared global
                    if (node.id in scope
                            and scope[node.id].used
                            and scope[node.id].used[0] is self.scope
                            and node.id not in self.scope.globals):
                        # then it's probably a mistake
                        self.report(messages.UndefinedLocal,
                                    scope[node.id].used[1],
                                    node.id,
                                    scope[node.id].source)
                        break

            if isinstance(node.parent,
                          (_ast.For, _ast.comprehension, _ast.Tuple, _ast.List)):
                binding = Binding(node.id, node)
            elif (node.id == '__all__' and
                  isinstance(self.scope, ModuleScope)):
                binding = ExportBinding(node.id, node.parent.value)
            else:
                binding = Assignment(node.id, node)
            if node.id in self.scope:
                binding.used = self.scope[node.id].used
            self.addBinding(node, binding)
        elif isinstance(node.ctx, _ast.Del):
            if isinstance(self.scope, FunctionScope) and \
                   node.id in self.scope.globals:
                del self.scope.globals[node.id]
            else:
                self.addBinding(node, UnBinding(node.id, node))
        else:
            # must be a Param context -- this only happens for names in function
            # arguments, but these aren't dispatched through here
            raise RuntimeError(
                "Got impossible expression context: %r" % (node.ctx,))


    def FUNCTIONDEF(self, node):
        # the decorators attribute is called decorator_list as of Python 2.6
        if hasattr(node, 'decorators'):
            for deco in node.decorators:
                self.handleNode(deco, node)
        else:
            for deco in node.decorator_list:
                self.handleNode(deco, node)
        self.addBinding(node, FunctionDefinition(node.name, node))
        self.LAMBDA(node)

    def LAMBDA(self, node):
        for default in node.args.defaults:
            self.handleNode(default, node)

        def runFunction():
            args = []

            def addArgs(arglist):
                for arg in arglist:
                    if isinstance(arg, _ast.Tuple):
                        addArgs(arg.elts)
                    else:
                        if arg.id in args:
                            self.report(messages.DuplicateArgument,
                                        node, arg.id)
                        args.append(arg.id)

            self.pushFunctionScope()
            addArgs(node.args.args)
            # vararg/kwarg identifiers are not Name nodes
            if node.args.vararg:
                args.append(node.args.vararg)
            if node.args.kwarg:
                args.append(node.args.kwarg)
            for name in args:
                self.addBinding(node, Argument(name, node), reportRedef=False)
            if isinstance(node.body, list):
                # case for FunctionDefs
                for stmt in node.body:
                    self.handleNode(stmt, node)
            else:
                # case for Lambdas
                self.handleNode(node.body, node)
            def checkUnusedAssignments():
                """
                Check to see if any assignments have not been used.
                """
                for name, binding in self.scope.iteritems():
                    if (not binding.used and not name in self.scope.globals
                        and isinstance(binding, Assignment)):
                        self.report(messages.UnusedVariable,
                                    binding.source, name)
            self.deferAssignment(checkUnusedAssignments)
            self.popScope()

        self.deferFunction(runFunction)


    def CLASSDEF(self, node):
        """
        Check names used in a class definition, including its decorators, base
        classes, and the body of its definition.  Additionally, add its name to
        the current scope.
        """
        # decorator_list is present as of Python 2.6
        for deco in getattr(node, 'decorator_list', []):
            self.handleNode(deco, node)
        for baseNode in node.bases:
            self.handleNode(baseNode, node)
        self.pushClassScope()
        for stmt in node.body:
            self.handleNode(stmt, node)
        self.popScope()
        self.addBinding(node, Binding(node.name, node))

    def ASSIGN(self, node):
        self.handleNode(node.value, node)
        for target in node.targets:
            self.handleNode(target, node)

    def AUGASSIGN(self, node):
        # AugAssign is awkward: must set the context explicitly and visit twice,
        # once with AugLoad context, once with AugStore context
        node.target.ctx = _ast.AugLoad()
        self.handleNode(node.target, node)
        self.handleNode(node.value, node)
        node.target.ctx = _ast.AugStore()
        self.handleNode(node.target, node)

    def IMPORT(self, node):
        for alias in node.names:
            name = alias.asname or alias.name
            importation = Importation(name, node)
            self.addBinding(node, importation)

    def IMPORTFROM(self, node):
        if node.module == '__future__':
            if not self.futuresAllowed:
                self.report(messages.LateFutureImport, node,
                            [n.name for n in node.names])
        else:
            self.futuresAllowed = False

        for alias in node.names:
            if alias.name == '*':
                self.scope.importStarred = True
                self.report(messages.ImportStarUsed, node, node.module)
                continue
            name = alias.asname or alias.name
            importation = Importation(name, node)
            if node.module == '__future__':
                importation.used = (self.scope, node)
            self.addBinding(node, importation)
