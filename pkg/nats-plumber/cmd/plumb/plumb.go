package main

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

var (
	attr      = flag.String("a", "", "set message attributes")
	src       = flag.String("s", "plumb", "set message source (default is plumb)")
	dst       = flag.String("d", "", "set message destination (default is empty)")
	mediaType = flag.String("t", "text/plain", "set the media type (default is text/plain)")
	wdir      = flag.String("w", "", "set message working directory (default is current directory)")
	showdata  = flag.Bool("i", false, "read data from stdin and add action=showdata attribute if not already set")
)

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

func main() {
	flag.Parse()

	msg := Message{
		Source:           *src,
		Destination:      *dst,
		MediaType:        *mediaType,
		WorkingDirectory: *wdir,
	}

	var err error
	msg.Attributes, err = ParseAttributes(*attr)
	if err != nil {
		log.Fatalf("parsing attributes: %v", err)
	}

	if *showdata {
		if _, ok := msg.Attributes["action"]; !ok {
			msg.Attributes["action"] = "showdata"
		}
		bytes, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			log.Fatalf("reading stdin: %v", err)
		}
		msg.Data = string(bytes)
	} else {
		msg.Data = strings.Join(flag.Args(), " ")
	}

	bytes, err := json.Marshal(msg)
	if err != nil {
		log.Fatalf("encoding JSON: %v", err)
	}

	println(string(bytes))

	// Open NATS
}
