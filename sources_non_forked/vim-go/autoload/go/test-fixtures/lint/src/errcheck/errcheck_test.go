package errcheck

import (
	"io"
	"os"
	"testing"
)

func TestFoo(t *testing.T) {
	io.Copy(os.Stdout, os.Stdin)
}
