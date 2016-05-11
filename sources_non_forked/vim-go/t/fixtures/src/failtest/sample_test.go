package pkg

import "testing"

func TestSample(t *testing.T) {
	Sample()
	t.Fatal("itwillfail")
}
