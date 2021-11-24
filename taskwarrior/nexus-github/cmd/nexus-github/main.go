package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/eraserhd/dotfiles/taskwarrior/nexus-github/pkg/github"
)

func main() {
	var query github.OpenPullRequestsQuery
	err := query.Fetch(os.Getenv("GITHUB_TOKEN"))
	if err != nil {
		panic(err)
	}

	tasks, err := query.Tasks()
	if err != nil {
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
