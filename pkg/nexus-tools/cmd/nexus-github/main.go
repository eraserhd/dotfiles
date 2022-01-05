package main

import (
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc/rdf"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/github"
	_ "github.com/eraserhd/dotfiles/nexus/tools/pkg/quilt"
)

func main() {
	ghstore, err := graph.NewQuadStore("github", "", map[string]interface{}{
		"token": os.Getenv("GITHUB_TOKEN"),
	})
	if err != nil {
		log.Fatalln(err)
	}

	g, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []graph.QuadStore{ghstore},
	})
	if err != nil {
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
