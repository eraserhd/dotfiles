package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"

	"github.com/nats-io/nats.go"

	"github.com/eraserhd/dotfiles/pkg/nats-plumber"
)

var (
	attr      = flag.String("a", "", "set message attributes")
	src       = flag.String("s", "plumb", "set message source (default is plumb)")
	dst       = flag.String("d", "", "set message destination (default is empty)")
	mediaType = flag.String("t", "text/plain", "set the media type (default is text/plain)")
	wdir      = flag.String("w", "", "set message working directory (default is current directory)")
	showdata  = flag.Bool("i", false, "read data from stdin and add action=showdata attribute if not already set")
)

func workingDirectory() (string, error) {
	if *wdir != "" {
		return *wdir, nil
	}
	hostname, _ := os.Hostname()
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("file://%s%s", hostname, dir), nil
}

func main() {
	flag.Parse()

	subject := "plumb.click"
	if *showdata {
		subject = "plumb.showdata"
	}

	msg := nats.NewMsg(subject)
	msg.Header.Add("Source", *src)
	msg.Header.Add("Content-Type", *mediaType)
	wdir, err := workingDirectory()
	if err != nil {
		log.Fatal(err)
	}
	msg.Header.Add("Working-Directory", wdir)

	attributes, err := nats_plumber.ParseAttributes(*attr)
	if err != nil {
		log.Fatalf("parsing attributes: %v", err)
	}
	for k, v := range attributes {
		msg.Header.Add(k, v)
	}

	if *showdata {
		bytes, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			log.Fatalf("reading stdin: %v", err)
		}
		msg.Data = bytes
	} else {
		msg.Data = []byte(strings.Join(flag.Args(), " "))
	}

	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatalf("connecting to NATS: %v", err)
	}
	defer nc.Close()

	if err := nc.PublishMsg(msg); err != nil {
		log.Fatalf("sending NATS message: %v", err)
	}
}
