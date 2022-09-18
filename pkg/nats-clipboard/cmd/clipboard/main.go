package main

import (
	"context"
	"log"

	"github.com/nats-io/nats.go"
	"golang.design/x/clipboard"
)

func main() {
	if err := clipboard.Init(); err != nil {
		log.Fatal(err)
	}

	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	getCh := make(chan *nats.Msg, 32)
	getSub, err := nc.ChanSubscribe("clipboard.get", getCh)
	if err != nil {
		log.Fatal(err)
	}
	defer getSub.Drain()

	setCh := make(chan *nats.Msg, 32)
	setSub, err := nc.ChanSubscribe("clipboard.set", setCh)
	if err != nil {
		log.Fatal(err)
	}
	defer setSub.Drain()

	changeCh := clipboard.Watch(context.Background(), clipboard.FmtText)

	var contents []byte
	for {
		select {
		case msg := <-getCh:
			reply := nats.NewMsg(msg.Reply)
			reply.Data = contents
			if err := nc.PublishMsg(reply); err != nil {
				log.Printf("error sending get reply: %v", err)
			}
		case msg := <-setCh:
			clipboard.Write(clipboard.FmtText, msg.Data)
			reply := nats.NewMsg(msg.Reply)
			if err := nc.PublishMsg(reply); err != nil {
				log.Printf("error sending set reply: %v", err)
			}
		case contents = <-changeCh:
			event := nats.NewMsg("clipboard.changed")
			event.Data = contents
			if err := nc.PublishMsg(event); err != nil {
				log.Printf("error sending changed event: %v", err)
			}
		}
	}
}
