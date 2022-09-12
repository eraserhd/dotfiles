package main

import (
	"log"

	"github.com/nats-io/nats.go"
)

func main() {
	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	getCh := make(chan *nats.Msg)
	getSub, err := nc.ChanSubscribe("clipboard.get", getCh)
	if err != nil {
		log.Fatal(err)
	}
	defer getSub.Drain()

	setCh := make(chan *nats.Msg)
	setSub, err := nc.ChanSubscribe("clipboard.set", setCh)
	if err != nil {
		log.Fatal(err)
	}
	defer setSub.Drain()

	for {
        	select {
                case msg := <-getCh:

                case msg := <-setCh:

        	}
	}


}
