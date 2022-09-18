package nats_plumber

import (
	"fmt"
	"reflect"
	"testing"
)

func TestParseAttributes(t *testing.T) {
	for _, testCase := range []struct {
		input  string
		output map[string]string
		err    error
	}{
		{
			input:  "",
			output: map[string]string{},
		},
		{
			input: "foo=bar baz=quux",
			output: map[string]string{
				"foo": "bar",
				"baz": "quux",
			},
		},
		{
			input: "foo='bar' ba'z=qu'ux",
			output: map[string]string{
				"foo": "bar",
				"baz": "quux",
			},
		},
		{
			input: "foobar",
			err:   NoEquals,
		},
		{
			input: "foo='",
			err:   UnterminatedQuote,
		},
		{
			input: "foo=''",
			output: map[string]string{
				"foo": "",
			},
		},
		{
			input: "foo=''''",
			output: map[string]string{
				"foo": "'",
			},
		},
		{
			input: "\tfo''o='hello'' world'!\t  bar'='''quux   ",
			output: map[string]string{
				"foo": "hello' world!",
				"bar": "'quux",
			},
		},
	} {
		t.Run(fmt.Sprintf("parsing %q", testCase.input), func(t *testing.T) {
			out, err := ParseAttributes(testCase.input)
			if err != testCase.err {
				t.Errorf("unexpected error state: %v", err)
			}
			if testCase.output != nil {
				if !reflect.DeepEqual(testCase.output, out) {
					t.Errorf("wrong output.  Expected:\n%v\nGot:\n%v", testCase.output, out)
				}
			}
		})
	}
}
