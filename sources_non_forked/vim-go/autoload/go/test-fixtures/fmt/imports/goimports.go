package main

import (
	"fmt"
)

func Foo(log *logging.TestLogger) {
log.Debug("vim-go")
}

func main() {
		fmt.Println("vim-go")
}
