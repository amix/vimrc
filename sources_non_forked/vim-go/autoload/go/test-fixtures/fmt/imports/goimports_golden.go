package main

import (
	"fmt"

	logging "gh.com/gi/foo-logging"
)

func Foo(log *logging.TestLogger) {
	log.Debug("vim-go")
}

func main() {
	fmt.Println("vim-go")
}
