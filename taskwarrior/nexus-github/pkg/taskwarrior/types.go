package taskwarrior

import (
	"github.com/google/uuid"
)

type (
	Task struct {
		Uuid    uuid.UUID
		Project string
		Status  string
	}
)
