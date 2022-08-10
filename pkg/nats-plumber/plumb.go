package nats_plumber

import (
	"errors"
	"strings"
	"unicode"
)

const (
	tokenizeStateInWhitespace = iota
	tokenizeStateInToken
	tokenizeStateInQuote
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
	NoEqualsError          = errors.New("no '=' in attribute string")
	UnterminatedQuoteError = errors.New("unterminated quote")
)

func tokenize(s string) ([]string, error) {
	state := tokenizeStateInWhitespace
	result := []string{}
	for _, ch := range s {
		switch state {
		case tokenizeStateInWhitespace:
			if ch == '\'' {
				state = tokenizeStateInQuote
			} else if !unicode.IsSpace(ch) {
				state = tokenizeStateInToken
				result = append(result, string(ch))
			}
		case tokenizeStateInToken:
			if ch == '\'' {
				state = tokenizeStateInQuote
			} else if unicode.IsSpace(ch) {
				state = tokenizeStateInWhitespace
			} else {
				result[len(result)-1] += string(ch)
			}
		case tokenizeStateInQuote:
			if ch == '\'' {
				state = tokenizeStateInToken
			} else {
				result[len(result)-1] += string(ch)
			}
		}
	}
	if state == tokenizeStateInQuote {
		return result, UnterminatedQuoteError
	}
	return result, nil
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
