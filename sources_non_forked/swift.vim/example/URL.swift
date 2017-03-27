import UIKit

class Foo: FooViewController, BarScrollable {

    var canScroll = false

    override func viewDidLoad() {
        baseURL = "http://www.oaklandpostonline.com/search/?q=&t=article&c[]=blogs*"
        super.viewDidLoad()
    }
}
