package jira

import (
	"fmt"
)

func Link(ticket string) string {
	return fmt.Sprintf("https://jira.2u.com/browse/%s", ticket)
}
