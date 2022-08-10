package main

import (
	"log"

	"github.com/nats-io/nats.go"

	"github.com/eraserhd/dotfiles/pkg/nats-plumber"
)

func main() {
	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatalf("connecting to NATS: %v", err)
	}
	conn, err := nats.NewEncodedConn(nc, nats.JSON_ENCODER)
	defer conn.Close()

	recvChan := make(chan *nats_plumber.Message)
	if _, err := conn.BindRecvChan("plumb", recvChan); err != nil {
		log.Fatalf("error binding receive channel: %v", err)
	}

	for {
		msg := <-recvChan
		log.Printf("got: %v", msg)
	}
}
