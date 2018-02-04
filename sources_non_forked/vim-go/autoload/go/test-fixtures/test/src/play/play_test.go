package play

import (
	"sync"
	"testing"

	"play/mock"
)

func TestTopSubHelper(t *testing.T) {
	t.Run("sub", func(t *testing.T) {
		t.Log("log message")
		t.Error("sub badness")
	})
	t.Error("badness")
	helper(t)
}

func TestMultiline(t *testing.T) {
	t.Error("this is an error\nand a second line, too")
	t.Error("\nthis is another error")
}

func TestSub(t *testing.T) {
	t.Run("indented", func(t *testing.T) {
		t.Error("this is a sub-test error\nand a second line, too")
	})
}

func TestOK(t *testing.T) {
	t.Run("log", func(t *testing.T) {
		t.Log("goodness")
	})
}

// TestMocked tests behavior similar to what users may experience when using
// github.com/golang/mock/gomock.
func TestMocked(t *testing.T) {
	mock.Fail(t)
}

func TestPanic(t *testing.T) {
	panic("worst ever")
}

func TestConcurrentPanic(t *testing.T) {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		panic("concurrent fail")
		wg.Done()
	}()
	wg.Wait()
}

func helper(t *testing.T) {
	t.Helper()
	t.Fatal("helper badness")
}
