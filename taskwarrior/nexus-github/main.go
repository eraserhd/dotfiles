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
				Name string
			} `graphql:"repository(name: \"nexus\")"`
		} `graphql:"organization(login: \"coding-boot-camp\")"`
	}
	err := client.Query(context.Background(), &query, nil)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Got: %s\n", query.Organization.Repository.Name)
}
