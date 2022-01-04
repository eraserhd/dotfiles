package github

import (
	"context"
	"errors"
	"net/http"
	"time"

	"github.com/cayleygraph/cayley/graph"
	_ "github.com/cayleygraph/cayley/graph/memstore"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc/rdf"
	"github.com/google/uuid"
	"github.com/shurcooL/githubv4"
	"golang.org/x/oauth2"
)

const (
	QuadStoreType = "github"

	IRIPrefix = `https://docs.github.com/en/graphql/reference/objects#`

	NodePrefix          = IRIPrefix + `node.`
	NodeId     quad.IRI = NodePrefix + `id`

	PullRequestType      quad.IRI = IRIPrefix + `pullrequest`
	PullRequestCreatedAt quad.IRI = PullRequestType + `.createdAt`
	PullRequestTitle     quad.IRI = PullRequestType + `.title`
	PullRequestPermalink quad.IRI = PullRequestType + `.permalink`
)

func init() {
	graph.RegisterQuadStore(QuadStoreType, graph.QuadStoreRegistration{
		NewFunc:      newQuadStore,
		UpgradeFunc:  nil,
		InitFunc:     nil,
		IsPersistent: false,
	})
}

var ReadOnlyError = errors.New("github store is read-only")

type QuadStore struct {
	memstore    graph.QuadStore
	githubToken string
}

// Namer

func (qs *QuadStore) ValueOf(v quad.Value) graph.Ref {
	return qs.memstore.ValueOf(v)
}

func (qs *QuadStore) NameOf(ref graph.Ref) quad.Value {
	return qs.memstore.NameOf(ref)
}

func (qs *QuadStore) Quad(ref graph.Ref) quad.Quad {
	return qs.memstore.Quad(ref)
}

func (qs *QuadStore) QuadDirection(id graph.Ref, d quad.Direction) graph.Ref {
	return qs.memstore.QuadDirection(id, d)
}

func (qs *QuadStore) QuadIterator(d quad.Direction, ref graph.Ref) graph.Iterator {
	return qs.memstore.QuadIterator(d, ref)
}

func (qs *QuadStore) QuadIteratorSize(ctx context.Context, d quad.Direction, ref graph.Ref) (graph.Size, error) {
	return qs.memstore.QuadIteratorSize(ctx, d, ref)
}

func (qs *QuadStore) ApplyDeltas(in []graph.Delta, opts graph.IgnoreOpts) error {
	return ReadOnlyError
}

func (qs *QuadStore) NewQuadWriter() (quad.WriteCloser, error) {
	return nil, ReadOnlyError
}

func (qs *QuadStore) NodesAllIterator() graph.Iterator {
	return qs.memstore.NodesAllIterator()
}

func (qs *QuadStore) QuadsAllIterator() graph.Iterator {
	return qs.memstore.QuadsAllIterator()
}

func (qs *QuadStore) Stats(ctx context.Context, exact bool) (graph.Stats, error) {
	return qs.memstore.Stats(ctx, exact)
}

func (qs *QuadStore) Close() error {
	return qs.memstore.Close()
}

func newQuadStore(path string, options graph.Options) (graph.QuadStore, error) {
	var qs QuadStore
	var err error
	qs.memstore, err = graph.NewQuadStore("memstore", path, options)
	if err != nil {
		return nil, err
	}
	if x, found := options["token"]; found {
		if token, ok := x.(string); ok {
			qs.githubToken = token
		} else {
			return nil, errors.New("token for github quadstore should be a string")
		}
	}

	var prs openPullRequestsQuery
	if err := prs.Fetch(qs.githubToken); err != nil {
		return nil, err
	}

	if err := prs.addQuads(qs.memstore); err != nil {
		return nil, err
	}

	return &qs, nil
}

type (
	openPullRequestsQuery struct {
		Organization struct {
			Repository struct {
				PullRequests struct {
					Edges []struct {
						Node struct {
							Id        string
							CreatedAt time.Time
							Title     string
							Permalink string
						}
					}
				} `graphql:"pullRequests(first: 100, states: [OPEN])"`
			} `graphql:"repository(name: \"nexus\")"`
		} `graphql:"organization(login: \"coding-boot-camp\")"`
	}
)

var prDomain = uuid.MustParse("fda3daa0-7252-4cce-883d-a8c438156032")

func (q *openPullRequestsQuery) Fetch(token string) error {
	var httpClient *http.Client
	if token == "" {
		httpClient = &http.Client{}
	} else {
		httpClient = oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(
			&oauth2.Token{AccessToken: token},
		))
	}
	client := githubv4.NewClient(httpClient)
	return client.Query(context.Background(), q, nil)
}

func (q *openPullRequestsQuery) addQuads(qs graph.QuadStore) error {
	w, err := qs.NewQuadWriter()
	if err != nil {
		return err
	}
	for _, edge := range q.Organization.Repository.PullRequests.Edges {
		id := quad.IRI(edge.Node.Permalink)
		if err := w.WriteQuad(quad.Make(id, quad.IRI(rdf.Type), PullRequestType, nil)); err != nil {
			return err
		}
		if err := w.WriteQuad(quad.Make(id, NodeId, quad.String(edge.Node.Id), nil)); err != nil {
			return err
		}
		if err := w.WriteQuad(quad.Make(id, PullRequestTitle, quad.String(edge.Node.Title), nil)); err != nil {
			return err
		}
		if err := w.WriteQuad(quad.Make(id, PullRequestCreatedAt, quad.Time(edge.Node.CreatedAt), nil)); err != nil {
			return err
		}
	}
	return err
}
