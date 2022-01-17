// quilt implements a quadstore backend which stiches together multiple other backends.
package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
	"github.com/cayleygraph/quad"
)

const (
	QuadStoreType = "quilt"
)

func init() {
	graph.RegisterQuadStore(QuadStoreType, graph.QuadStoreRegistration{
		NewFunc: newQuadStore,
		//FIXME: Other functions
	})
}

func newQuadStore(path string, options graph.Options) (graph.QuadStore, error) {
	var qs QuadStore
	qs.substores = options["substores"].([]graph.QuadStore)
	if len(qs.substores) <= 0 {
		panic("the quilt driver requires at least one substore configuration")
	}
	return &qs, nil
}

type QuadStore struct {
	substores []graph.QuadStore
}

// graph.Namer

func (qs *QuadStore) ValueOf(v quad.Value) (graph.Ref, error) {
	var ref quiltref
	for i := range qs.substores {
		if subref, err := qs.substores[i].ValueOf(v); err == nil && subref != nil {
			ref = append(ref, quiltsubref{substore: i, subref: subref})
		}
	}
	if ref == nil {
		return nil, nil
	}
	return ref, nil
}

func (qs *QuadStore) NameOf(ref graph.Ref) (quad.Value, error) {
	if ref == nil {
		return nil, nil
	}
	qr := ref.(quiltref)
	return qs.substores[qr[0].substore].NameOf(qr[0].subref)
}

// graph.QuadIndexer

func (qs *QuadStore) Quad(ref graph.Ref) (quad.Quad, error) {
	qr := ref.(quiltref)
	return qs.substores[qr[0].substore].Quad(qr[0].subref)
}

func (qs *QuadStore) QuadIterator(d quad.Direction, ref graph.Ref) iterator.Shape {
	var subiterators []iterator.Shape
	for _, subref := range ref.(quiltref) {
		subiterators = append(subiterators, qs.substores[subref.substore].QuadIterator(d, subref.subref))
	}
	return newShape(subiterators, qs.broadenQuadRef)
}

func (qs *QuadStore) QuadIteratorSize(ctx context.Context, d quad.Direction, ref graph.Ref) (refs.Size, error) {
	return qs.substores[0].QuadIteratorSize(ctx, d, ref) //FIXME:
}

func (qs *QuadStore) QuadDirection(id graph.Ref, d quad.Direction) (graph.Ref, error) {
	var result quiltref
	for _, subref := range id.(quiltref) {
		if ref, err := qs.substores[subref.substore].QuadDirection(subref.subref, d); err == nil && ref != nil {
			result = append(result, quiltsubref{substore: subref.substore, subref: ref})
		}
	}
	if result == nil {
		return nil, nil
	}
	return result, nil
}

// Stats sums the stats for all substores in the quilt.
//
// It is possible for a node or a quad to appear in more than one store, so these stats
// may overestimate slightly.
func (qs *QuadStore) Stats(ctx context.Context, exact bool) (graph.Stats, error) {
	result, err := qs.substores[0].Stats(ctx, exact)
	for i := 1; i < len(qs.substores); i++ {
		subresult, looperr := qs.substores[i].Stats(ctx, exact)
		if looperr != nil {
			err = looperr
		}
		result.Nodes.Exact = result.Nodes.Exact && subresult.Nodes.Exact
		result.Nodes.Value += subresult.Nodes.Value
		result.Quads.Exact = result.Quads.Exact && subresult.Quads.Exact
		result.Quads.Value += subresult.Quads.Value
	}
	return result, err
}

// graph.QuadStore

func (qs *QuadStore) ApplyDeltas(in []graph.Delta, opts graph.IgnoreOpts) error {
	return qs.substores[0].ApplyDeltas(in, opts)
}

func (qs *QuadStore) NewQuadWriter() (quad.WriteCloser, error) {
	return qs.substores[0].NewQuadWriter()
}

func (qs *QuadStore) NodesAllIterator() iterator.Shape {
	subiterators := make([]iterator.Shape, len(qs.substores))
	for i := range subiterators {
		subiterators[i] = qs.substores[i].NodesAllIterator()
	}
	return newShape(subiterators, qs.broadenNodeRef)
}

func (qs *QuadStore) QuadsAllIterator() iterator.Shape {
	subiterators := make([]iterator.Shape, len(qs.substores))
	for i := range subiterators {
		subiterators[i] = qs.substores[i].QuadsAllIterator()
	}
	return newShape(subiterators, qs.broadenQuadRef)
}

func (qs *QuadStore) Close() error {
	var err error
	for i := range qs.substores {
		if looperr := qs.substores[i].Close(); looperr != nil {
			err = looperr
		}
	}
	return err
}

func (qs *QuadStore) broadenNodeRef(ref graph.Ref) graph.Ref {
	return ref
}

func (qs *QuadStore) broadenQuadRef(ref graph.Ref) graph.Ref {
	return ref
}
