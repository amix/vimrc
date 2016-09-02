// set gopath before
//go:generate go test --coverprofile=sample.out
// go1.5.3 example output:
//   GOPATH=`pwd`/fixtures go test --coverprofile=log.out parsefail
//   # cover parsefail
//   2016/01/17 23:59:08 cover: /home/sey/vimfiles/_vim/bundle/vim-go-coverlay/t/fixtures/src/parsefail/sample.go: /home/sey/vimfiles/_vim/bundle/vim-go-coverlay/t/fixtures/src/parsefail/sample.go:10:1: expected declaration, found 'IDENT' PARSEFAIL
//   FAIL    parsefail [build failed]
//   echo $?
//   2
package pkg

PARSEFAIL Sample() int {
	return 0
}
