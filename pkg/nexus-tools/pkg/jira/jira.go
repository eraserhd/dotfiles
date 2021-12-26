package jira

import (
	"fmt"
	"strings"
	"unicode"
)

func Link(ticket string) string {
	return fmt.Sprintf("https://jira.2u.com/browse/%s", ticket)
}

func isAllDigits(s string) bool {
	if len(s) == 0 {
		return false
	}
	for _, r := range s {
		if !unicode.IsDigit(r) {
			return false
		}
	}
	return true
}

func TicketsForBranchName(branch string) ([]string, error) {
	parts := strings.Split(strings.Trim(strings.ToUpper(branch), " \t\r\n\v"), "-")
	if len(parts) < 2 {
		return nil, fmt.Errorf("%q does not have enough parts to be a branch name", branch)
	}
	var firstNumericPart int
	for i, part := range parts {
		if isAllDigits(part) {
			firstNumericPart = i
			break
		}
	}
	if firstNumericPart == 0 {
		return nil, fmt.Errorf("%q does not have a project name part", branch)
	}
	project := parts[firstNumericPart-1]
	var tickets []string
	for i := firstNumericPart; i < len(parts); i++ {
		tickets = append(tickets, fmt.Sprintf("%s-%s", project, parts[i]))
	}
	return tickets, nil
}
