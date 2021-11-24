package github

import (
	"context"
	"os"
	"time"

	"github.com/eraserhd/dotfiles/taskwarrior/nexus-github/pkg/taskwarrior"
	"github.com/google/uuid"
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
							Id        string
							CreatedAt time.Time
							Title     string
							Permalink string
						}
					}
				} `graphql:"pullRequests(first: 100)"`
			} `graphql:"repository(name: \"nexus\")"`
		} `graphql:"organization(login: \"coding-boot-camp\")"`
	}
)

var prDomain = uuid.MustParse("fda3daa0-7252-4cce-883d-a8c438156032")

func (q *OpenPullRequestsQuery) Fetch(token string) error {
	httpClient := oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: os.Getenv("GITHUB_TOKEN")},
	))
	client := githubv4.NewClient(httpClient)
	return client.Query(context.Background(), q, nil)
}

func (q *OpenPullRequestsQuery) Tasks() ([]taskwarrior.Task, error) {
	var tasks []taskwarrior.Task
	for _, edge := range q.Organization.Repository.PullRequests.Edges {
		uuid := uuid.NewSHA1(prDomain, []byte(edge.Node.Id))
		tasks = append(tasks, taskwarrior.Task{
			Uuid:        uuid,
			Entry:       taskwarrior.Date(edge.Node.CreatedAt),
			Description: edge.Node.Title,
			Project:     "nexus",
			Status:      "pending",
			Tags:        []string{"github"},
			Annotation:  []string{edge.Node.Permalink},
		})
	}
	return tasks, nil
}
