package mock

import "testing"

func Fail(t *testing.T) {
	t.Fatal("another package badness")
}
