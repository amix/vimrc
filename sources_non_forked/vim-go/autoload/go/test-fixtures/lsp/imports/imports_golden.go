package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
)

func main() {
	io.Copy(ioutil.Discard, os.Stdin)
	fmt.Println("vim-go")
}
