package main

import (
	"log"

	"github.com/nats-io/nats.go"
)

func main() {
	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatalf("connecting to NATS: %v", err)
	}
	defer nc.Close()

	ch := make(chan *nats.Msg)
	sub, err := nc.ChanSubscribe("plumb.click", ch)
	if err != nil {
		log.Fatalf("error binding receive channel: %v", err)
	}
	defer sub.Drain()

	for {
		msg := <-ch
		log.Printf("received %q", string(msg.Data))

		next := nats.NewMsg("browser.open")
		next.Reply = msg.Reply
		next.Header = msg.Header
		next.Data = msg.Data
		if err := nc.PublishMsg(next); err != nil {
			log.Printf("error publishing: %v", err)
			reply := nats.NewMsg(msg.Reply)
			reply.Header.Add("Status", err.Error())
			nc.PublishMsg(reply)
		}
	}
}
