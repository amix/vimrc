#!/Applications/Xcode6-Beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/swift -i

// This is a comment

let foo = 5 // another comment

/* this is also a comment */

// If statements so the indented comments are valid
if foo {
    /* this is an indented comment */
}

if foo {
    /* this is a multi level indented comment /* you know */ */
}

// comments check splelling

/* this is
a multi-line
/* you know */

/** foo
bar
*/

comment
*/

/// foo bar


"this is a string no splell checking"
"this is a string\" with an escaped quote"

// TODO: This is a todo comment
// XXX: This is another todo comment
// FIXME: this is another todo comment
// NOTE: this is another todo comment
/* TODO multiple */

// MARK: this is a marker

if foo {
    // this is a indented comment
}

5 // int

5.5 // float
5e-2
5E2
5.5E-2
5.5e2
5.5f2
5.5abc5.5 // broken

0xa2ef // hex
0x123P432
0xa_2ef // hex with underscore
0x13p-43
0x13r-43
0x213zdf // broken hex

0b10101 // binary
0b1010_1 // binary with underscore
0b1234 // broken binary

0o567 // octal
0o56_7 // octal with underscore
0o5689 // broken octal

1_000_000                // underscore separated million
1_000_0000_              // broken underscore separated number
1_000_0000.              // broken underscore separated float
1_000_000.000_000_1      // just over one million
1_18181888_2.1.1         // broken underscore padded double
1_18181888_2.1           // valid according to swift repl
1_0_0                    // valid 100
1_0_000.2                // valid 10000.2
1_____0.2________20___2  // also valid 10.2202
4__3.2_33_33             // valid 43.233

// Operators
~
!
%
^
&
2 * 2
-
+
=
|
2 / 5
.
>
<

a != b
a != b
a !== b
a !== b
%=
&%
&&
&&=
let a = 10 &* 20
&+
&-
8 &/ 20
&=
let a *= 20
++
+=
--
-=
..
...
let b = 50 /= 20
<<
<=
=<<
==
===
>=
>>
>>=
^=
|=
||
||=
~=

// Custom Operators
infix operator ～ {
    precedence 20      // Define precedence
    associativity none // Define associativity
}

true
false

class Shape : NSObject {
    var foo: String
    var qux: String = "abcd"
    let bar = String?[]()
    let baz = String()?
    let foo = Int()

    init(thing: String) {
        foo = thing
        super.init(thing)
        let bar:String= "123"

        bar!
    }

    func foo(thing1 : String, 2thing : Int52) {

    }

    func bar(thing: String?){

    }
}

import Cocoa

struct Thing: NSString {
    var foo : Int
}

enum Card : Int {
    case Spade = 1
    case Heart
    case Diamond
    case Club
    indirect case Foo(a: Card)
}

let indirect = 5

struct foo : bar {
    switch (foo) {
    case foo:
        foo
    case bar:
    default:
        stuff
    case baz:
        fuck
    case bar:
        bafsd
    }

    func foo() {

    }

    func bar(asdf: String) -> Bool {

    }

    func baz() -> (Foo, Bar)
    {

    }

    func asdf<T>() {

    }
}

struct ArgumentList {
    var arguments: String[]

    init(argv: UnsafePointer<CString>,
        count: CInt)
    {
        foo
    }
}

let a : UnsafePointer<CString>

func foo<T: Sequence>() {

}

init(argv: UnsafePointer<CString>, count: CInt) {
    for i in 1..count {
        let index = Int(i)
        let arg = String.fromCString(argv[index])
        arguments.append(arg)
    }
}

func simpleDescription() -> String {
    return "A shape with \(numberOfSides.toRaw()) sides."
}

let library = [
    Movie(name: "foo bar",
        dfasdfsdfdirector: "someone",
        foo: "bar",
        bazzzer: "qux")
]


foo as? String
let foo : Int = bar ?? 5

let arg: String = "123"
hello<String>(arg, arg2: 1.0, arg3: arg, arg4: "foo", arg5: false)


class MainViewController: UIViewController, UITableViewDataSource {}

@IBAction func changePostFilter(sender: UISegmentedControl) {}
override func prepareForSegue(segue: UIStoryboardSegue,
   sender: AnyObject) {}
override func prepareForSegue(segue: UIStoryboardSegue!, sender: AnyObject!) {}
override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject!) {}
lazy var foo : String

#if foo
bar
#elseif baz
qux
#else
quix
#endif

client.host = "example.com"
client.pathPrefix = "/foo/"

@available(*, unavailable, renamed="bar", introduced=1.0, deprecated=2.2, message="hi")
func foo () {
    override func loadView() {
        super.loadView()
        if foo {
            foobar
        }
    }
}

let foo = CGRectMake(0, (5 - 2),
    100, 200)


let dict = [
    "foo": "Bar",
    "nest": [
        "fadsf",
    ]
]

if #available(OSX 10.10.3, *) {
    // Use APIs OS X 10.10.3 and onwards
}
if #available(watchOS 2, iOS 9.0, OSX 10.11, *) {
    // APIs available to watchOS 2.0, iOS 9.0, OSX 10.11 and onwards
}

// Tests backslashes in strings
"\\".uppercaseString()
"foo \(1 + 1)"
string.rangeOfString("^/Date\\(")

public var `extension`: String?

/**
This is the comment body

- parameter first: The first parameter
- Parameter first: The first parameter

- returns: Some value
*/

public let fareEstimate: FareEstimate //= (nil, nil) // comment should be highlighted as comment

// optionalFrom should be highlighted the same way
// Operator should also be highlighted
key = map.optionalFrom("string") ?? []
key = map.optionalFrom("string")
thing = map.optionalFrom("string") ?? .Fallback

// This should not break all highlighting
print("Copying \(NSProcessInfo().environment["SCRIPT_INPUT_FILE_\(index)"]!)")
// Neither should this
return addressParts.count == 2 ? addressParts[1] : "\(addressParts[1]), \(addressParts[2])"

// This is multiline garbage
"foo
bar
baz"

guard let path = NSBundle.mainBundle().pathForResource(imageName, ofType: "png"),
let data = NSData(contentsOfFile: path),
let data = NSData(contentsOfFile: path) else
{
}

UIView.animateWithDuration(duration, delay: 0, usingSpringWithDamping: 0.8, initialSpringVelocity: 0, options: .CurveEaseInOut, animations: {
    view.backgroundColor = UIColor.redColor()
}) { finished in
    print("indent?")
}

// Indent last line should hold
self.init(className: "Item", dictionary: [
    "identifier": item.identifier,
    "title": item.title,
    "link": item.link,
    "date": item.date,
    "summary": item.summary])

XCAssertEqual(variables as NSDictionary, expectedVariables as NSDictionary, "\(template)")
