package main

import (
	"context"
	"fmt"
	"os"

	"github.com/shurcooL/githubv4"
	"golang.org/x/oauth2"
)

func main() {
	httpClient := oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: os.Getenv("GITHUB_TOKEN")},
	))

	client := githubv4.NewClient(httpClient)

	var query struct {
		Organization struct {
			Repository struct {
				PullRequests struct {
					Edges []struct {
						Node struct {
							Id     string
							Number int
						}
					}
				} `graphql:"pullRequests(first: 100)"`
			} `graphql:"repository(name: \"nexus\")"`
		} `graphql:"organization(login: \"coding-boot-camp\")"`
	}
	err := client.Query(context.Background(), &query, nil)
	if err != nil {
		panic(err)
	}

	for _, edge := range query.Organization.Repository.PullRequests.Edges {
		fmt.Printf("id = %s\n", edge.Node.Id)
	}

}
