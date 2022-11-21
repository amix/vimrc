# Should be an error
function = 42
var = 42

# Shouldn't be an error
abc.with = 42
function: 42
var: 42

# Keywords shouldn't be highlighted
abc.function
abc.do
abc.break
abc.true

abc::function
abc::do
abc::break
abc::true

abc:: function
abc. function

# Numbers should be highlighted
def.42
def .42
def::42
