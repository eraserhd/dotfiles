package taskwarrior

import (
	"errors"
	"testing"

	"github.com/google/uuid"
)

var nonExistantUuid = uuid.MustParse("13b0b3cc-21e0-4e4b-af07-862300741c6f")

func Test_GetTaskByUUID_returns_nil(t *testing.T) {
	provider := &Provider{}
	_, err := provider.GetTaskByUUID(nonExistantUuid)
	if !errors.Is(err, TaskNotFound) {
		t.Errorf("wanted provider.GetTaskByUUI(nonExistantUuid) to have error %v, got %v", TaskNotFound, err)
	}
}
