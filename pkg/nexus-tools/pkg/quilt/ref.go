package quilt

import (
	"github.com/cayleygraph/cayley/graph/refs"
)

type quiltsubref struct {
	substore int
	subref   refs.Ref
}

type quiltref []quiltsubref

var _ refs.Ref = quiltref{}

func (qr quiltref) Key() interface{} {
	if qr == nil {
		return nil
	}
	return qr[0]
}
