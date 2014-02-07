
from _ast import PyCF_ONLY_AST

from twisted.trial.unittest import TestCase

from pyflakes import messages as m, checker
from pyflakes.test import harness


class Test(harness.Test):
    def test_undefined(self):
        self.flakes('bar', m.UndefinedName)

    def test_definedInListComp(self):
        self.flakes('[a for a in range(10) if a]')


    def test_functionsNeedGlobalScope(self):
        self.flakes('''
        class a:
            def b():
                fu
        fu = 1
        ''')

    def test_builtins(self):
        self.flakes('range(10)')


    def test_magicGlobalsFile(self):
        """
        Use of the C{__file__} magic global should not emit an undefined name
        warning.
        """
        self.flakes('__file__')


    def test_magicGlobalsBuiltins(self):
        """
        Use of the C{__builtins__} magic global should not emit an undefined
        name warning.
        """
        self.flakes('__builtins__')


    def test_magicGlobalsName(self):
        """
        Use of the C{__name__} magic global should not emit an undefined name
        warning.
        """
        self.flakes('__name__')


    def test_magicGlobalsPath(self):
        """
        Use of the C{__path__} magic global should not emit an undefined name
        warning, if you refer to it from a file called __init__.py.
        """
        self.flakes('__path__', m.UndefinedName)
        self.flakes('__path__', filename='package/__init__.py')


    def test_globalImportStar(self):
        '''Can't find undefined names with import *'''
        self.flakes('from fu import *; bar', m.ImportStarUsed)

    def test_localImportStar(self):
        '''A local import * still allows undefined names to be found in upper scopes'''
        self.flakes('''
        def a():
            from fu import *
        bar
        ''', m.ImportStarUsed, m.UndefinedName)

    def test_unpackedParameter(self):
        '''Unpacked function parameters create bindings'''
        self.flakes('''
        def a((bar, baz)):
            bar; baz
        ''')

    def test_definedByGlobal(self):
        '''"global" can make an otherwise undefined name in another function defined'''
        self.flakes('''
        def a(): global fu; fu = 1
        def b(): fu
        ''')
    test_definedByGlobal.todo = ''

    def test_globalInGlobalScope(self):
        """
        A global statement in the global scope is ignored.
        """
        self.flakes('''
        global x
        def foo():
            print x
        ''', m.UndefinedName)

    def test_del(self):
        '''del deletes bindings'''
        self.flakes('a = 1; del a; a', m.UndefinedName)

    def test_delGlobal(self):
        '''del a global binding from a function'''
        self.flakes('''
        a = 1
        def f():
            global a
            del a
        a
        ''')

    def test_delUndefined(self):
        '''del an undefined name'''
        self.flakes('del a', m.UndefinedName)

    def test_globalFromNestedScope(self):
        '''global names are available from nested scopes'''
        self.flakes('''
        a = 1
        def b():
            def c():
                a
        ''')

    def test_laterRedefinedGlobalFromNestedScope(self):
        """
        Test that referencing a local name that shadows a global, before it is
        defined, generates a warning.
        """
        self.flakes('''
        a = 1
        def fun():
            a
            a = 2
            return a
        ''', m.UndefinedLocal)

    def test_laterRedefinedGlobalFromNestedScope2(self):
        """
        Test that referencing a local name in a nested scope that shadows a
        global declared in an enclosing scope, before it is defined, generates
        a warning.
        """
        self.flakes('''
            a = 1
            def fun():
                global a
                def fun2():
                    a
                    a = 2
                    return a
        ''', m.UndefinedLocal)


    def test_intermediateClassScopeIgnored(self):
        """
        If a name defined in an enclosing scope is shadowed by a local variable
        and the name is used locally before it is bound, an unbound local
        warning is emitted, even if there is a class scope between the enclosing
        scope and the local scope.
        """
        self.flakes('''
        def f():
            x = 1
            class g:
                def h(self):
                    a = x
                    x = None
                    print x, a
            print x
        ''', m.UndefinedLocal)


    def test_doubleNestingReportsClosestName(self):
        """
        Test that referencing a local name in a nested scope that shadows a
        variable declared in two different outer scopes before it is defined
        in the innermost scope generates an UnboundLocal warning which
        refers to the nearest shadowed name.
        """
        exc = self.flakes('''
            def a():
                x = 1
                def b():
                    x = 2 # line 5
                    def c():
                        x
                        x = 3
                        return x
                    return x
                return x
        ''', m.UndefinedLocal).messages[0]
        self.assertEqual(exc.message_args, ('x', 5))


    def test_laterRedefinedGlobalFromNestedScope3(self):
        """
        Test that referencing a local name in a nested scope that shadows a
        global, before it is defined, generates a warning.
        """
        self.flakes('''
            def fun():
                a = 1
                def fun2():
                    a
                    a = 1
                    return a
                return a
        ''', m.UndefinedLocal)

    def test_nestedClass(self):
        '''nested classes can access enclosing scope'''
        self.flakes('''
        def f(foo):
            class C:
                bar = foo
                def f(self):
                    return foo
            return C()

        f(123).f()
        ''')

    def test_badNestedClass(self):
        '''free variables in nested classes must bind at class creation'''
        self.flakes('''
        def f():
            class C:
                bar = foo
            foo = 456
            return foo
        f()
        ''', m.UndefinedName)

    def test_definedAsStarArgs(self):
        '''star and double-star arg names are defined'''
        self.flakes('''
        def f(a, *b, **c):
            print a, b, c
        ''')

    def test_definedInGenExp(self):
        """
        Using the loop variable of a generator expression results in no
        warnings.
        """
        self.flakes('(a for a in xrange(10) if a)')



class NameTests(TestCase):
    """
    Tests for some extra cases of name handling.
    """
    def test_impossibleContext(self):
        """
        A Name node with an unrecognized context results in a RuntimeError being
        raised.
        """
        tree = compile("x = 10", "<test>", "exec", PyCF_ONLY_AST)
        # Make it into something unrecognizable.
        tree.body[0].targets[0].ctx = object()
        self.assertRaises(RuntimeError, checker.Checker, tree)
