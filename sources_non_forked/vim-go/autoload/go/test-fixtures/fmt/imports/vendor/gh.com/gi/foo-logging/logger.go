package logging

import "fmt"

type TestLogger struct {
	Value string
}

func (l *TestLogger) Debug(msg string) {
	fmt.Println(msg)
	fmt.Println(l.Value)
}
