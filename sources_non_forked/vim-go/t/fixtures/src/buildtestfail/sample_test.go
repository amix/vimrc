// go1.5.3 example output:
//   GOPATH=`pwd`/fixtures go test --coverprofile=log.out buildtestfail
//   # buildtestfail
//   fixtures/src/buildtestfail/sample_test.go:14: undefined: IT_SHOULD_BE_BUILD_FAILED
//   FAIL    buildtestfail [build failed]
//   echo $?
//   2

package pkg

import "testing"

func TestSample(t *testing.T) {
	IT_SHOULD_BE_BUILD_FAILED
}
