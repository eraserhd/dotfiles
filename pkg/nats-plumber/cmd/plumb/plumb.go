package main

import (
	"encoding/json"
	"os"
	"flag"
	"fmt"
)

var (
	attr     = flag.String("a", "", "set message attributes")
	src      = flag.String("s", "plumb", "set message source (default is plumb)")
	dst      = flag.String("d", "", "set message destination (default is empty)")
	type     = flag.String("t", "text/plain", "set message type (default is text/plain)")
	wdir     = flag.String("w", "", "set message working directory (default is current directory)")
	showdata = flag.Bool("i", false, "read data from stdin and add action=showdata attribute if not already set")
)

type Message struct {
	Source           string `json:"source"`
	Destination      string `json:"destination"`
	Type             string `json:"type"`
	WorkingDirectory string `json:"workingDirectory"`
	Data             []byte `json:"data"`
	Attributes       map[string]string `json:"attributes"`
}

func ParseAttributes(s string) (map[string]string, error) {
	result := new(map[string]string)
	
	return result	
}

func main() {
	flag.Parse()
	
	msg := Message{
		Source: *src,
		Destination: *dst,
		Type: *type,
		WorkingDirectory: *wdir,
	}
	
	var err error
	msg.Attributes, err = ParseAttributes(*attr)
	if err != nil {
		log.Fatalf("parsing attributes: %w", err)
	}

	if *showdata {
		if _, ok := msg.Attributes["action"]; !ok {
			msg.Attributes["action"] = "showdata"
		}
		msg.Data, err = ioutil.ReadAll(os.Stdin)
		if err != nil {
			log.Fatalf("reading stdin: %w", err)
		}
	} else {
		msg.Data = []byte(strings.Join(flag.Args(), " "))
	}
	
	bytes, err := json.Marshal(msg)
	if err != nil {
		log.Fatalf("encoding JSON: %w", err)
	}
	
	// Open NATS
}