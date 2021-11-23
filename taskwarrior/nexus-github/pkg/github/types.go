package github

import (
	"context"
	"os"

	"github.com/shurcooL/githubv4"
	"golang.org/x/oauth2"
)

type (
	OpenPullRequestsQuery struct {
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
)

func (q *OpenPullRequestsQuery) Fetch(token string) error {
	httpClient := oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: os.Getenv("GITHUB_TOKEN")},
	))
	client := githubv4.NewClient(httpClient)
	return client.Query(context.Background(), q, nil)
}
