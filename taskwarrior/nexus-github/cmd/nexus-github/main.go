package main

import (
	"fmt"
	"os"

	"github.com/eraserhd/dotfiles/taskwarrior/nexus-github/pkg/github"
)

func main() {
	var query github.OpenPullRequestsQuery
	err := query.Fetch(os.Getenv("GITHUB_TOKEN"))
	if err != nil {
		panic(err)
	}

	for _, edge := range query.Organization.Repository.PullRequests.Edges {
		fmt.Printf("id = %s\n", edge.Node.Id)
	}
}
