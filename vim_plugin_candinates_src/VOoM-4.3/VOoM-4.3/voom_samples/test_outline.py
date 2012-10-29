# :Voom python
# VOoM test file for Python outlining mode
# no gotchas--oultine operations do not kill or create headlines
# vim: et ts=4 sw=4 sts=4 fdm=manual

"""
docstring
    def func_in_docstring():
        pass
    # not COMMENT
after func_in_docstring
"""

a = [1, # COMMENT, headline
        # COMMENT, not headline
        2]
# line continuation
s = "oosp\
        class Fake\
           pass \
       "

s2 = """
xx
yy
closing " " " must not be mistaken for start of docstring
"""

### if 1
# NEXT LINE IS FOR TEST SUITE -- DO NOT MOVE OR EDIT
# VO.levels=[1, 1, 2, 3, 3, 1, 1, 1, 2, 3, 3, 3, 2, 2, 1, 1, 1, 2, 3, 3, 1, 1, 1, 1, 1]
if 1:
    ### if 1
    if 1:
        def func1(a,
                       b, 
                   c):
            """
            docstring
            """
            pass
        #--- headline 1 

def func_with_string(a, b, c=False, d="NoName",
                                e=None, f=0, g='Oopsy'):
    """
text text text text 
    """

#---- headline before Class1
class Class1:
    b = []
    def func2():
        ### headline 2
        a = 'a'
        def func3():
            pass
        #----- headline 3
    #----headline 4
    def func4(): pass

#----- headline 5
# not headline
def func4(f):
    '''
badly indented docstring
'''
    pass

class Class2:
    u" perversely formatted docstring \
perversely formatted docstring"
    b = []
    def func5():
        pass
        def func6():
            pass
        #--- headline 6
#---- headline 7
# not a headline

def func7(func):
    a = \
"perverted continuation"
    pass

@func7
def func8(): # <-- headline
    a = 1
    b = [1,
2, # <-- false headline
3]
    c = 4

### if __name__=='__main__':
if __name__=='__main__':
    print Class2.__doc__
