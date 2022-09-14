package main

import (
	"log"
	"os"

	"github.com/ichiban/prolog"
	"github.com/nats-io/nats.go"
)

const logic = `
plumb(msg('browser.open',Data,Headers)) :- plumb(msg('plumb.click',Data,Headers)).
`

func addFactsFromMsg(p *prolog.Interpreter, msg *nats.Msg) error {
	stmt := "plumb(msg(?,?,["
	args := []interface{}{msg.Subject, string(msg.Data)}
	first := true
	for k, vs := range msg.Header {
		for _, v := range vs {
			if !first {
				stmt += ","
			}
			first = false
			stmt += "header(?,?)"
			args = append(args, k, v)
		}
	}
	stmt += "]))."
	if err := p.Exec(stmt, args...); err != nil {
		return err
	}
	return nil
}

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

		p := prolog.New(os.Stdin, os.Stdout)

		if err := p.Exec(logic); err != nil {
			log.Printf("error loading logic: %v", err)
			continue
		}

		if err := addFactsFromMsg(p, msg); err != nil {
			log.Printf("error setting input values: %v", err)
			continue
		}

		sols, err := p.Query(`plumb(msg(Subject,Data,_)), Subject \= 'plumb.click'.`)
		if err != nil {
			log.Printf("error querying solutions: %v", err)
		}
		for sols.Next() {
			var sol struct {
				Subject string
				Data    string
			}
			if err := sols.Scan(&sol); err != nil {
				log.Printf("error during scan: %v", err)
			}
			log.Printf("send %q %q", sol.Subject, sol.Data)
		}
		if err := sols.Close(); err != nil {
			log.Printf("error closing solutions: %v", err)
		}

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
