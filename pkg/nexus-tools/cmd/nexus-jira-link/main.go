package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/eraserhd/dotfiles/nexus/tools/pkg/jira"
)

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
		branchTickets, err := jira.TicketsForBranchName(branch)
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

	formatTicket := jira.Link
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
