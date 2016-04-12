// set gopath before
//go:generate go test --coverprofile=sample.out
// go1.5.3 example output:
//   GOPATH=`pwd`/fixtures go test --coverprofile=log.out buildfail
//   # buildfail
//   /tmp/go-build264733986/buildfail/_test/_obj_test/sample.go:7: undefined: IT_SHOULD_BE_BUILD_FAILED
//   /tmp/go-build264733986/buildfail/_test/_obj_test/sample.go:8: missing return at end of function
//   FAIL    buildfail [build failed]
package pkg

func Sample() int {
	IT_SHOULD_BE_BUILD_FAILED
}
