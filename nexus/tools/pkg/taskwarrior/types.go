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
)

func (d Date) MarshalJSON() ([]byte, error) {
	s := "\"" + time.Time(d).Format("20060102T150405Z") + "\""
	return []byte(s), nil
}
