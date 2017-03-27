# Swift.vim

Syntax and indent files for [Swift](https://developer.apple.com/swift/)

If you don't have a preferred installation method check out
[vim-plug](https://github.com/junegunn/vim-plug).

## Examples

![](https://raw.githubusercontent.com/keith/swift.vim/master/screenshots/screen.png)
![](https://raw.githubusercontent.com/keith/swift.vim/master/screenshots/screen2.png)

## [Syntastic](https://github.com/scrooloose/syntastic/) Integration

swift.vim can show errors inline from
[swift package manager](https://github.com/apple/swift-package-manager/)
or from [swiftlint](https://github.com/realm/SwiftLint) using
[syntastic](https://github.com/scrooloose/syntastic/).

![](https://raw.githubusercontent.com/keith/swift.vim/master/screenshots/screen3.png)

### Usage

- Install [syntastic](https://github.com/scrooloose/syntastic/)

- swiftpm integration will be automatically enabled if you're running vim
from a directory containing a `Package.swift` file.

- SwiftLint integration will be automatically enabled if you have
SwiftLint installed and if you're running vim from a directory
containing a `.swiftlint.yml` file.

- To enable both at once add this to your vimrc:

```vim
let g:syntastic_swift_checkers = ['swiftpm', 'swiftlint']
```
