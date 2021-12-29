package taskwarrior

import (
	"errors"

	"github.com/google/uuid"
)

var TaskNotFound = errors.New("Task not found")

type Provider struct {
}

func (p *Provider) GetTaskByUUID(id uuid.UUID) (Task, error) {
	return Task{}, TaskNotFound
}
