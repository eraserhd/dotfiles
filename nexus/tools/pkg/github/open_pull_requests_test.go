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
		tasks taskwarrior.Tasks
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

func (s *Scenario) WithExistingTask(task taskwarrior.Task) *Scenario {
	s.tasks = append(s.tasks, task)
	return s
}

func (s *Scenario) SingleTask() TestableTask {
	if err := s.query.UpdateTasks(&s.tasks); err != nil {
		s.t.Fatalf("wanted err == nil, got %v", err)
	}
	if len(s.tasks) != 1 {
		s.t.Fatalf("wanted len(tasks) == 1, got %d", len(s.tasks))
	}
	return TestableTask{s.t, s.tasks[0]}
}

func Test_New_UUID_is_not_zero_UUID(t *testing.T) {
	var zeroUuid uuid.UUID
	if NewScenario(t).SingleTask().Uuid == zeroUuid {
		t.Errorf("wanted non-zero UUID, but got a zero UUID")
	}
}

func Test_UUID_is_repeatably_computed(t *testing.T) {
	if NewScenario(t).SingleTask().Uuid != NewScenario(t).SingleTask().Uuid {
		t.Error("wanted uuid to be repeatable, but it was not")
	}
}

func Test_UUID_is_unique_to_the_pull_request(t *testing.T) {
	if NewScenario(t).SingleTask().Uuid == NewScenario(t).WithId("MDExOlB1bGxSZXF1ZXN0MjE3MDE1MDk5").SingleTask().Uuid {
		t.Error("wanted task.Uuid != task3.Uuid, both are same")
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
	NewScenario(t).
		WithExistingTask(taskwarrior.Task{Uuid: uuid.MustParse("06292007-ace9-5854-ac4e-732370e890da")}).
		SingleTask()
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

func (tt TestableTask) HasAnnotation(needle string) {
	for _, s := range tt.Annotations {
		if needle == s.Description {
			return
		}
	}
	tt.t.Errorf(`want annotations to include %q, got %+v`, needle, tt.Annotations)
}

func Test_Annotation_contains_pull_request_URL(t *testing.T) {
	NewScenario(t).SingleTask().HasAnnotation("https://example.com/pull/42")
}

func Test_Annotation_contains_JIRA_URLs(t *testing.T) {
	NewScenario(t).SingleTask().HasAnnotation("https://jira.2u.com/browse/BCTS4-1574")
	NewScenario(t).SingleTask().HasAnnotation("https://jira.2u.com/browse/BCTS4-97")
}
