package github

import (
	"encoding/json"
	"testing"

	"github.com/google/uuid"
)

func Test_Uuid_is_repeatably_computed_from_PullRequest_Id(t *testing.T) {
	pulls := OpenPullRequestsQuery{}
	err := json.Unmarshal([]byte(`{
		"Organization": {
        		"Repository": {
                		"PullRequests": {
                        		"Edges": [ {
                                		"Id": ""
                        		} ]
                		}
        		}
		}
        }`), &pulls)
	if err != nil {
		t.Errorf("wanted err == nil, got %v", err)
	}
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
}
