package main

import (
	"io/ioutil"
	"testing"
)

func TestSomething(t *testing.T) {
	r := struct{}{}
	ioutil.ReadAll(r)
}
