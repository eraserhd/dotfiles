package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/eraserhd/dotfiles/nexus/tools/pkg/github"
	"github.com/eraserhd/dotfiles/nexus/tools/pkg/taskwarrior"
)

func main() {
	var query github.OpenPullRequestsQuery
	if err := query.Fetch(os.Getenv("GITHUB_TOKEN")); err != nil {
		panic(err)
	}

	var tasks taskwarrior.Tasks
	if err := query.UpdateTasks(&tasks); err != nil {
		panic(err)
	}

	for _, task := range tasks {
		out, err := json.Marshal(task)
		if err != nil {
			panic(err)
		}
		fmt.Println(string(out))
	}
}
