package main

import (
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
	_ "github.com/cayleygraph/quad/nquads"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/github"
)

const (
	PullRequestId quad.IRI = "https://example.com/Id"
	Title         quad.IRI = "https://example.com/Title"
)

func main() {
	var query github.OpenPullRequestsQuery
	if err := query.Fetch(os.Getenv("GITHUB_TOKEN")); err != nil {
		log.Fatalln(err)
	}

	g, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalln(err)
	}

	for _, edge := range query.Organization.Repository.PullRequests.Edges {
		id := quad.IRI(edge.Node.Permalink)
		g.AddQuad(quad.Make(id, PullRequestId, quad.String(edge.Node.Id), nil))
		g.AddQuad(quad.Make(id, Title, quad.String(edge.Node.Title), nil))
	}

	qr := graph.NewQuadStoreReader(g.QuadStore)
	defer qr.Close()
	format := quad.FormatByName("nquads")
	w := format.Writer(os.Stdout)
	defer w.Close()

	if _, err := quad.Copy(w, qr); err != nil {
		log.Fatalln(err)
	}
}
