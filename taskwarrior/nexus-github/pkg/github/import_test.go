package github

import (
	"encoding/json"
	"testing"

	"github.com/google/uuid"
)

func queryResults(t *testing.T, text string) OpenPullRequestsQuery {
	var pulls OpenPullRequestsQuery
	err := json.Unmarshal([]byte(text), &pulls)
	if err != nil {
		t.Errorf("unmarshal: %v", err)
	}
	return pulls
}

const (
	singlePullWithId1 = `{
		"Organization": {
			"Repository": {
				"PullRequests": {
					"Edges": [ {
						"Node": {
							"Id": "MDExOlB1bGxSZXF1ZXN0MjEwNzk3NTAx"
						}
					} ]
				}
			}
		}
        }`

	singlePullWithId2 = `{
		"Organization": {
			"Repository": {
				"PullRequests": {
					"Edges": [ {
						"Node": {
							"Id": "MDExOlB1bGxSZXF1ZXN0MjE3MDE1MDk5"
						}
					} ]
				}
			}
		}
        }`
)

func Test_Uuid_is_repeatably_computed_from_PullRequest_Id(t *testing.T) {
	pulls := queryResults(t, singlePullWithId1)
	tasks, err := pulls.Tasks()
	if err != nil {
		t.Errorf("wanted err == nil, got %v", err)
	}
	if len(tasks) != 1 {
		t.Errorf("wanted len(tasks) == 1, got %d", len(tasks))
	}
	var zeroUuid uuid.UUID
	if tasks[0].Uuid == zeroUuid {
		t.Errorf("wanted non-zero UUID, but got a zero UUID")
	}
	tasks2, err := pulls.Tasks()
	if err != nil {
		t.Errorf("wanted err == nil, got %v", err)
	}
	if tasks[0].Uuid != tasks2[0].Uuid {
		t.Error("wanted uuid to be repeatable, but it was not")
	}
	pulls3 := queryResults(t, singlePullWithId2)
	tasks3, err := pulls3.Tasks()
	if err != nil {
		t.Errorf("wanted err == nil, got %v", err)
	}
	if tasks[0].Uuid == tasks3[0].Uuid {
		t.Errorf("wanted tasks[0].Uuid != tasks3.Uuid, both are %v", tasks3[0].Uuid)
	}
}
