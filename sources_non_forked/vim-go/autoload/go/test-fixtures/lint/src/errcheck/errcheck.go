package errcheck

import (
	"io"
	"os"
)

func foo() {
	io.Copy(os.Stdout, os.Stdin)
}
