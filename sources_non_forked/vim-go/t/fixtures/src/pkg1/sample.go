// set gopath before
//go:generate go test --coverprofile=sample.out
package pkg1

func Sample() int {
	if false {
		return 0
	} else if false {
		return 0
	}
	return 1
}
