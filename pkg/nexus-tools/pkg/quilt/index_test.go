package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/quad"
	"github.com/stretchr/testify/assert"
)

func Test_Contains_finds_nodes_in_all_substores(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	index := qs.NodesAllIterator().Lookup()
	defer index.Close()

	for _, iriname := range []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	} {
		value, err := qs.ValueOf(quad.Raw(iriname))
		assert.NoError(t, err)
		found := index.Contains(context.TODO(), value)
		assert.Truef(t, found, "want it.Contains(%q) to be true", iriname)
	}
}
