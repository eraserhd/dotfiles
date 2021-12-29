package main

import (
	"context"
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/query"
	_ "github.com/cayleygraph/cayley/query/gizmo"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/github"
)

func main() {
	var prs github.OpenPullRequestsQuery
	if err := prs.Fetch(os.Getenv("GITHUB_TOKEN")); err != nil {
		log.Fatalln(err)
	}

	g, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalln(err)
	}

	if err := prs.AddQuads(g); err != nil {
		log.Fatalln(err)
	}

	it, err := query.Execute(context.TODO(), g, "gizmo", `g.V()
          .has("<rdf:type>", "<https://docs.github.com/en/graphql/reference/objects#pullrequest>")
          .all();`, query.Options{
		Collation: query.REPL,
	})
	if err != nil {
		log.Fatalln(err)
	}
	defer it.Close()

	for it.Next(context.TODO()) {
		log.Println(it.Result())
	}
	if err := it.Err(); err != nil {
		log.Fatalln(err)
	}
}
