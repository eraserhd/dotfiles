package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)

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

func JiraTickets(branch string) ([]string, error) {
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

func JiraLink(ticket string) string {
	return fmt.Sprintf("https://jira.2u.com/browse/%s", ticket)
}

func branchArgs() []string {
	branches := flag.Args()
	if len(branches) > 0 {
		return branches
	}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		branch := scanner.Text()
		branches = append(branches, branch)
	}
	return branches
}

func ticketArgs() []string {
	var tickets []string
	for _, branch := range branchArgs() {
		branchTickets, err := JiraTickets(branch)
		if err != nil {
			log.Printf("parsing %q: %v", branch, err)
			continue
		}
		tickets = append(tickets, branchTickets...)
	}
	return tickets
}

func main() {
	singleResult := flag.Bool("1", false, "find one ticket link")
	ticketNameOnly := flag.Bool("ticket", false, "print ticket name instead of full link")
	flag.Parse()

	formatTicket := JiraLink
	if *ticketNameOnly {
		formatTicket = func(ticket string) string {
			return ticket
		}
	}

	tickets := ticketArgs()
	if *singleResult {
		if len(tickets) == 0 {
			log.Fatal("no tickets found.\n")
		}
		fmt.Println(formatTicket(tickets[0]))
		return
	}
	for _, ticket := range tickets {
		fmt.Println(formatTicket(ticket))
	}
}
