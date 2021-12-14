package github

import (
	"context"
	"time"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc/rdf"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/jira"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/rules"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/taskwarrior"
	"github.com/google/uuid"
	"github.com/shurcooL/githubv4"
	"golang.org/x/oauth2"
)

const (
	IRIPrefix = `https://docs.github.com/en/graphql/reference/objects#`

	NodePrefix          = IRIPrefix + `node.`
	NodeId     quad.IRI = NodePrefix + `id`

	PullRequestType      quad.IRI = IRIPrefix + `pullrequest`
	PullRequestCreatedAt quad.IRI = PullRequestType + `.createdAt`
	PullRequestTitle     quad.IRI = PullRequestType + `.title`
	PullRequestPermalink quad.IRI = PullRequestType + `.permalink`
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
		&oauth2.Token{AccessToken: token},
	))
	client := githubv4.NewClient(httpClient)
	return client.Query(context.Background(), q, nil)
}

// rules:
// * UUID
// * JIRA tickets
func UpdateTasks(handle *cayley.Handle, tasks *taskwarrior.Tasks) error {
	return cayley.StartPath(handle).
		Has(quad.IRI(rdf.Type), PullRequestType).
		Tag("pr").Out(NodeId).Tag("id").
		Back("pr").Out(PullRequestTitle).Tag("title").
		Back("pr").Out(PullRequestCreatedAt).Tag("createdAt").
		Iterate(nil).
		TagValues(handle, func(result map[string]quad.Value) {
			pr := string(result["pr"].Native().(quad.IRI))
			id := result["id"].Native().(string)
			createdAt := result["createdAt"].Native().(time.Time)
			title := result["title"].Native().(string)
			uuid := uuid.NewSHA1(prDomain, []byte(id))
			task := tasks.FindOrCreateByUUID(uuid)
			entry := taskwarrior.Date(createdAt)
			annotations := []taskwarrior.Annotation{{
				Entry:       entry,
				Description: pr,
			}}
			tickets, _ := jira.TicketsForBranchName(title)
			for _, ticket := range tickets {
				annotations = append(annotations, taskwarrior.Annotation{
					Entry:       entry,
					Description: jira.Link(ticket),
				})
			}
			task.Entry = entry
			task.Description = title
			task.Project = "nexus"
			task.Status = "pending"
			task.Tags = []string{"github", "next"}
			task.Annotations = annotations
		})
}

func (q *OpenPullRequestsQuery) AddQuads(h *cayley.Handle) error {
	for _, edge := range q.Organization.Repository.PullRequests.Edges {
		id := quad.IRI(edge.Node.Permalink)
		if err := h.AddQuad(quad.Make(id, quad.IRI(rdf.Type), PullRequestType, nil)); err != nil {
			return err
		}
		if err := h.AddQuad(quad.Make(id, NodeId, quad.String(edge.Node.Id), nil)); err != nil {
			return err
		}
		if err := h.AddQuad(quad.Make(id, PullRequestTitle, quad.String(edge.Node.Title), nil)); err != nil {
			return err
		}
		if err := h.AddQuad(quad.Make(id, PullRequestCreatedAt, quad.Time(edge.Node.CreatedAt), nil)); err != nil {
			return err
		}
	}
	_, err := rules.Process(h)
	return err
}
