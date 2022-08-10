package nats_plumber

type Message struct {
	Source           string            `json:"source"`
	Destination      string            `json:"destination"`
	MediaType        string            `json:"mediaType"`
	WorkingDirectory string            `json:"workingDirectory"`
	Data             string            `json:"data"`
	Attributes       map[string]string `json:"attributes"`
}

func ParseAttributes(s string) (map[string]string, error) {
	result := make(map[string]string)

	return result, nil
}
