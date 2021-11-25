package github

import (
	"context"
	"os"
	"time"

	"github.com/eraserhd/dotfiles/nexus/tools/pkg/jira"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/taskwarrior"
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
				} `graphql:"pullRequests(first: 100, states: [OPEN])"`
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

func (q *OpenPullRequestsQuery) UpdateTasks(tasks *taskwarrior.Tasks) error {
	for _, edge := range q.Organization.Repository.PullRequests.Edges {
		uuid := uuid.NewSHA1(prDomain, []byte(edge.Node.Id))
		task := tasks.FindOrCreateByUUID(uuid)
		entry := taskwarrior.Date(edge.Node.CreatedAt)
		annotations := []taskwarrior.Annotation{{
			Entry:       entry,
			Description: edge.Node.Permalink,
		}}
		tickets, _ := jira.TicketsForBranchName(edge.Node.Title)
		for _, ticket := range tickets {
			annotations = append(annotations, taskwarrior.Annotation{
				Entry:       entry,
				Description: jira.Link(ticket),
			})
		}
		task.Entry = entry
		task.Description = edge.Node.Title
		task.Project = "nexus"
		task.Status = "pending"
		task.Tags = []string{"github", "next"}
		task.Annotations = annotations
	}
	return nil
}
