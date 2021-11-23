package main

import (
	"context"
	"fmt"
	"os"

	"github.com/eraserhd/dotfiles/taskwarrior/nexus-github/pkg/github"

	"github.com/shurcooL/githubv4"
	"golang.org/x/oauth2"
)

func main() {
	httpClient := oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: os.Getenv("GITHUB_TOKEN")},
	))

	client := githubv4.NewClient(httpClient)

	var query github.OpenPullRequestsQuery
	err := client.Query(context.Background(), &query, nil)
	if err != nil {
		panic(err)
	}

	for _, edge := range query.Organization.Repository.PullRequests.Edges {
		fmt.Printf("id = %s\n", edge.Node.Id)
	}

}
