package taskwarrior

import (
	"github.com/google/uuid"
	"time"
)

type (
	Date time.Time

	Task struct {
		Uuid       uuid.UUID `json:"uuid"`
		Entry      Date      `json:"entry"`
		Project    string    `json:"project"`
		Status     string    `json:"status"`
		Tags       []string  `json:"tags"`
		Annotation []string  `json:"annotation"`
	}
)

func (d Date) MarshalJSON() ([]byte, error) {
	s := "\"" + time.Time(d).Format("20060102T150405Z") + "\""
	return []byte(s), nil
}
