package github

import (
	"encoding/json"
	"regexp"
	"strings"
	"testing"

	"github.com/google/uuid"

	"github.com/eraserhd/dotfiles/nexus/tools/pkg/taskwarrior"
)

type (
	Scenario struct {
		t     *testing.T
		query OpenPullRequestsQuery
	}

	TestableTask struct {
		t *testing.T
		taskwarrior.Task
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

func (s *Scenario) WithId(id string) *Scenario {
	s.query.Organization.Repository.PullRequests.Edges[0].Node.Id = id
	return s
}

func (s *Scenario) SingleTask() TestableTask {
	var tasks taskwarrior.Tasks
	if err := s.query.UpdateTasks(&tasks); err != nil {
		s.t.Fatalf("wanted err == nil, got %v", err)
	}
	if len(tasks) != 1 {
		s.t.Fatalf("wanted len(tasks) == 1, got %d", len(tasks))
	}
	return TestableTask{s.t, tasks[0]}
}

func queryResults(t *testing.T, text string) OpenPullRequestsQuery {
	var pulls OpenPullRequestsQuery
	if err := json.Unmarshal([]byte(text), &pulls); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	return pulls
}

func Test_Uuid_is_repeatably_computed_from_PullRequest_Id(t *testing.T) {
	task := NewScenario(t).SingleTask()
	var zeroUuid uuid.UUID
	if task.Uuid == zeroUuid {
		t.Errorf("wanted non-zero UUID, but got a zero UUID")
	}
	task2 := NewScenario(t).SingleTask()
	if task.Uuid != task2.Uuid {
		t.Error("wanted uuid to be repeatable, but it was not")
	}
	task3 := NewScenario(t).WithId("MDExOlB1bGxSZXF1ZXN0MjE3MDE1MDk5").SingleTask()
	if task.Uuid == task3.Uuid {
		t.Errorf("wanted task.Uuid != task3.Uuid, both are %v", task3.Uuid)
	}
}

var uuidPattern = regexp.MustCompile(`^"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}"$`)

func Test_Task_Uuid_serialies_lower_case_and_dashed(t *testing.T) {
	task := NewScenario(t).SingleTask()
	bytes, err := json.Marshal(task.Uuid)
	if err != nil {
		t.Errorf("wanted err == nil, got %v", err)
	}
	if !uuidPattern.Match(bytes) {
		t.Errorf("wanted uuidPattern.Match(%q)", string(bytes))
	}
}

func Test_An_existing_task_is_not_created_twice(t *testing.T) {
	pulls := queryResults(t, sampleQueryData)
	tasks := taskwarrior.Tasks{{
		Uuid: uuid.MustParse("06292007-ace9-5854-ac4e-732370e890da"),
	}}
	if err := pulls.UpdateTasks(&tasks); err != nil {
		t.Fatalf("wanted err == nil, got %v", err)
	}
	if len(tasks) != 1 {
		t.Fatalf("wanted len(tasks) == 1, got %+v", tasks)
	}
}

func Test_Entry_date_is_pull_request_creation_date(t *testing.T) {
	task := NewScenario(t).SingleTask()
	bytes, err := json.Marshal(task.Entry)
	if err != nil {
		t.Fatalf("wanted err == nil, got %v", err)
	}
	if string(bytes) != `"20211104T144303Z"` {
		t.Errorf("wanted task.Entry = \"20211104T144303Z\", got %q\n", string(bytes))
	}
}

func Test_Status_is_pending(t *testing.T) {
	task := NewScenario(t).SingleTask()
	if task.Status != "pending" {
		t.Errorf("wanted task.Status == \"pending\", got %q", task.Status)
	}
}

func Test_Description_contains_pull_request_title(t *testing.T) {
	task := NewScenario(t).SingleTask()
	if !strings.Contains(task.Description, "mw-bcts4-1574") {
		t.Errorf("want strings.Contains(%q, \"mw-bcts4-1574\")", task.Description)
	}
}

func Test_Project_is_nexus(t *testing.T) {
	task := NewScenario(t).SingleTask()
	if task.Project != "nexus" {
		t.Errorf("wanted task.Project == \"nexus\", got %q", task.Project)
	}
}

func assertHasTag(t *testing.T, task taskwarrior.Task, tag string) {
	for _, existingTag := range task.Tags {
		if existingTag == tag {
			return
		}
	}
	t.Errorf("wanted tags to include %q, got %+v", tag, task.Tags)
}

func (tt TestableTask) HasTag(tag string) {
	for _, existingTag := range tt.Tags {
		if existingTag == tag {
			return
		}
	}
	tt.t.Errorf("wanted tags to include %q, got %+v", tag, tt.Tags)
}

func Test_Has_github_tag(t *testing.T) {
	NewScenario(t).SingleTask().HasTag("github")
}

func Test_Has_next_tag(t *testing.T) {
	NewScenario(t).SingleTask().HasTag("next")
}

func assertHasAnnotation(t *testing.T, task taskwarrior.Task, needle string) {
	for _, s := range task.Annotations {
		if needle == s.Description {
			return
		}
	}
	t.Errorf(`want annotations to include %q, got %+v`, needle, task.Annotations)
}

func Test_Annotation_contains_pull_request_URL(t *testing.T) {
	task := NewScenario(t).SingleTask()
	assertHasAnnotation(t, task.Task, "https://example.com/pull/42")
}

func Test_Annotation_contains_JIRA_URLs(t *testing.T) {
	task := NewScenario(t).SingleTask()
	assertHasAnnotation(t, task.Task, "https://jira.2u.com/browse/BCTS4-1574")
	assertHasAnnotation(t, task.Task, "https://jira.2u.com/browse/BCTS4-97")
}
