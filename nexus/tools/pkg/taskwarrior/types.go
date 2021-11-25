package taskwarrior

import (
	"github.com/google/uuid"
	"time"
)

type (
	Date time.Time

	Annotation struct {
		Entry       Date   `json:"entry"`
		Description string `json:"description"`
	}

	Task struct {
		Uuid        uuid.UUID    `json:"uuid"`
		Entry       Date         `json:"entry"`
		Description string       `json:"description"`
		Project     string       `json:"project"`
		Status      string       `json:"status"`
		Tags        []string     `json:"tags"`
		Annotations []Annotation `json:"annotations"`
	}

	Tasks []Task
)

func (d Date) MarshalJSON() ([]byte, error) {
	s := "\"" + time.Time(d).Format("20060102T150405Z") + "\""
	return []byte(s), nil
}

func (ts *Tasks) FindByUUID(id uuid.UUID) *Task {
	for i := range *ts {
		if (*ts)[i].Uuid == id {
			return &(*ts)[i]
		}
	}
	return nil
}

func (ts *Tasks) FindOrCreateByUUID(id uuid.UUID) *Task {
	task := ts.FindByUUID(id)
	if task != nil {
		return task
	}
	*ts = append(*ts, Task{Uuid: id})
	return &(*ts)[len(*ts)-1]
}
