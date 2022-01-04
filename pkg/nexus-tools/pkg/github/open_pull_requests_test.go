package github

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

type (
	Scenario struct {
		t     *testing.T
		query openPullRequestsQuery
	}
)

const (
	sampleQueryData = `{
		"Organization": {
			"Repository": {
				"PullRequests": {
					"Edges": [ {
						"Node": {
							"Id": "MDExOlB1bGxSZXF1ZXN0MjEwNzk3NTAx",
							"CreatedAt": "2021-11-04T14:43:03Z",
							"Title": "mw-bcts4-1574-97",
							"Permalink": "https://example.com/pull/42"
						}
					} ]
				}
			}
		}
        }`
)

func NewScenario(t *testing.T) *Scenario {
	s := Scenario{
		t: t,
	}
	if err := json.Unmarshal([]byte(sampleQueryData), &s.query); err != nil {
		s.t.Fatalf("unmarshal: %v", err)
	}
	return &s
}

func (s *Scenario) QuadStore() *cayley.Handle {
	h, err := cayley.NewMemoryGraph()
	if err != nil {
		s.t.Fatalf("error creating memory graph: %v", err)
	}
	if err := s.query.addQuads(h); err != nil {
		s.t.Fatalf("error getting quads: %v", err)
	}
	return h
}

func (s *Scenario) Has(predicate quad.IRI, object interface{}) {
	qs := NewScenario(s.t).QuadStore()
	path := cayley.StartPath(qs, quad.IRI("https://example.com/pull/42")).Out(predicate)
	result, err := path.Iterate(nil).FirstValue(qs)
	if err != nil {
		s.t.Fatalf("error getting result: %v", err)
	}
	value, _ := quad.AsValue(result)
	wantValue, _ := quad.AsValue(object)
	if wantValue.String() != value.String() {
		s.t.Errorf("want %v, got %v", wantValue.String(), value.String())
	}
}

func Test_PR_entity_has_NodeId(t *testing.T) {
	NewScenario(t).Has(NodeId, "MDExOlB1bGxSZXF1ZXN0MjEwNzk3NTAx")
}

func Test_PR_entity_has_title(t *testing.T) {
	NewScenario(t).Has(PullRequestTitle, "mw-bcts4-1574-97")
}

func Test_PR_entity_has_createdAt(t *testing.T) {
	NewScenario(t).Has(PullRequestCreatedAt, time.Date(2021, 11, 4, 14, 43, 3, 0, time.UTC))
}
