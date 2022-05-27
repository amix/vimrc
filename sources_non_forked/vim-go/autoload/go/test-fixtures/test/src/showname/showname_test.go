package main

import "testing"

func TestHelloWorld(t *testing.T) {
	t.Error("so long")

	t.Run("sub", func(t *testing.T) {
		t.Error("thanks for all the fish")
	})
}
