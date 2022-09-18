package nats_plumber

import (
	"fmt"
	"reflect"
	"testing"
)

func TestParseAttributes(t *testing.T) {
	for _, testCase := range []struct {
        	claim string
		input  string
		output map[string]string
		err    error
	}{
		{
        		claim: "an empty string contains no attributes",
			input:  "",
			output: map[string]string{},
		},
		{
        		claim: "attributes are separated by whitespace",
			input: "foo=bar baz=quux",
			output: map[string]string{
				"foo": "bar",
				"baz": "quux",
			},
		},
		{
        		claim: "parts of attributes can be single-quoted",
			input: "foo='bar' ba'z=qu'ux",
			output: map[string]string{
				"foo": "bar",
				"baz": "quux",
			},
		},
		{
        		claim: "every attribute must have an '='",
			input: "foobar",
			err:   NoEquals,
		},
		{
        		claim: "unbalanced quotes are an error",
			input: "foo='",
			err:   UnterminatedQuote,
		},
		{
        		claim: "an empty string can be single-quoted",
			input: "foo=''",
			output: map[string]string{
				"foo": "",
			},
		},
		{
        		claim: "doubled single quotes represent a literal single quote",
			input: "foo=''''",
			output: map[string]string{
				"foo": "'",
			},
		},
		{
        		claim: "???",
			input: "\tfo''o='hello'' world'!\t  bar'='''quux   ",
			output: map[string]string{
				"foo": "hello' world!",
				"bar": "'quux",
			},
		},
	} {
		t.Run(fmt.Sprintf(testCase.claim, testCase.input), func(t *testing.T) {
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
