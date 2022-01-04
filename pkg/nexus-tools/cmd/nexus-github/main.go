package main

import (
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc/rdf"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/github"
)

func main() {
	var prs github.OpenPullRequestsQuery
	if err := prs.Fetch(os.Getenv("GITHUB_TOKEN")); err != nil {
		log.Fatalln(err)
	}

	g, err := cayley.NewGraph("github", "", map[string]interface{}{
		"token": os.Getenv("GITHUB_TOKEN"),
	})
	if err != nil {
		log.Fatalln(err)
	}

	if err := prs.AddQuads(g); err != nil {
		log.Fatalln(err)
	}

	if err := cayley.StartPath(g).
		Has(quad.IRI(rdf.Type), github.PullRequestType).
		Iterate(nil).
		EachValue(g, func(v quad.Value) {
			log.Println(v)
		}); err != nil {
		log.Fatalln(err)
	}
}
