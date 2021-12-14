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

func main() {
	var query github.OpenPullRequestsQuery
	if err := query.Fetch(os.Getenv("GITHUB_TOKEN")); err != nil {
		log.Fatalln(err)
	}

	g, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalln(err)
	}

	if err := query.AddQuads(g); err != nil {
		log.Fatalln(err)
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
