package main

import (
	"fmt"
)

func main() {
	io.Copy(ioutil.Discard, os.Stdin)
	fmt.Println("vim-go")
}
