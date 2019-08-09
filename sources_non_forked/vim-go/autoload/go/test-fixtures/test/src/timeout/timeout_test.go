// Run a few parallel tests, all in parallel, using multiple techniques for
// causing the test to take a while so that the stacktraces resulting from a
// test timeout will contain several goroutines to avoid giving a false sense
// of confidence or creating error formats that don't account for the more
// complex scenarios that can occur with timeouts.

package main

import (
	"testing"
	"time"
)

func TestSleep(t *testing.T) {
	t.Parallel()
	time.Sleep(15 * time.Second)
	t.Log("expected panic if run with timeout < 15s")
}

func TestRunning(t *testing.T) {
	t.Parallel()
	c := time.After(15 * time.Second)
Loop:
	for {
		select {
		case <-c:
			break Loop
		default:
		}
	}

	t.Log("expected panic if run with timeout < 15s")
}

func TestRunningAlso(t *testing.T) {
	t.Parallel()
	c := time.After(15 * time.Second)
Loop:
	for {
		select {
		case <-c:
			break Loop
		default:
		}
	}
	t.Log("expected panic if run with timeout < 15s")
}
