package github

type (
	OpenPullRequestsQuery struct {
		Organization struct {
			Repository struct {
				PullRequests struct {
					Edges []struct {
						Node struct {
							Id     string
							Number int
						}
					}
				} `graphql:"pullRequests(first: 100)"`
			} `graphql:"repository(name: \"nexus\")"`
		} `graphql:"organization(login: \"coding-boot-camp\")"`
	}
)
