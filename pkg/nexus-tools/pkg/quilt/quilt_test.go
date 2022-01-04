package quilt

import (
	"testing"

	"github.com/cayleygraph/cayley"
)

func Test_Can_create_quilt_backend(t *testing.T) {
	qs, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []map[string]interface{}{
			{
				"type": "memstore",
			},
		},
	})
	if err != nil {
		t.Errorf("want err = nil, got %v", err)
	}
	qs.Close()
}
