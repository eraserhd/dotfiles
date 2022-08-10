package nats_plumber

import (
	"errors"
	"io"
	"strings"
	"unicode"
)

type Message struct {
	Source           string            `json:"source"`
	Destination      string            `json:"destination"`
	MediaType        string            `json:"mediaType"`
	WorkingDirectory string            `json:"workingDirectory"`
	Data             string            `json:"data"`
	Attributes       map[string]string `json:"attributes"`
}

var (
        NoEqualsError = errors.New("no '=' in attribute string")
)

func skipWhitespace(rdr *strings.Reader) error {
	for {
		ch, _, err := rdr.ReadRune()
		if errors.Is(err, io.EOF) {
			return err
		}
		if unicode.IsSpace(ch) {
			continue
		}
		return rdr.UnreadRune()
	}
}

func tokenize(s string) ([]string, error) {
        if s == "" {
                return []string{}, nil
        }
	return strings.Split(s, " "), nil
}

func ParseAttributes(s string) (map[string]string, error) {
	result := make(map[string]string)

	tokens, err := tokenize(s)
	if err != nil {
		return result, err
	}

	for _, token := range tokens {
		parts := strings.SplitN(token, "=", 2)
		if len(parts) != 2 {
			return result, NoEqualsError
		}
		result[parts[0]] = parts[1]
	}

	return result, nil
}
