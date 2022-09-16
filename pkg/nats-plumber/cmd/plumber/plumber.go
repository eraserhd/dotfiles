package main

import (
	_ "embed"
	"log"
	"os"

	"github.com/ichiban/prolog"
	"github.com/nats-io/nats.go"
)

//go:embed routes.pl
var routes string

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
			stmt += "? - ?"
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

		if err := p.Exec(routes); err != nil {
			log.Printf("error loading logic: %v", err)
			continue
		}

		if err := addFactsFromMsg(p, msg); err != nil {
			log.Printf("error setting input values: %v", err)
			continue
		}

		sols, err := p.Query(`
			send(msg(Subject,Data,Headers)),
				maplist(arg(1),Headers,HeaderKeys),
				maplist(arg(2),Headers,HeaderValues).
		`)
		if err != nil {
			log.Printf("error querying solutions: %v", err)
		}
		sentAny := false
		for sols.Next() {
			var sol struct {
				Subject      string
				Data         string
				HeaderKeys   []string
				HeaderValues []string
			}
			if err := sols.Scan(&sol); err != nil {
				log.Printf("error during scan: %v", err)
				continue
			}
			next := nats.NewMsg(sol.Subject)
			next.Reply = msg.Reply
			next.Data = []byte(sol.Data)
			for i := range sol.HeaderKeys {
				next.Header.Add(sol.HeaderKeys[i], sol.HeaderValues[i])
			}
			log.Printf("send %q %q", sol.Subject, sol.Data)
			if err := nc.PublishMsg(next); err != nil {
				log.Printf("error publishing: %v", err)
			} else {
				sentAny = true
			}
		}
		if err := sols.Close(); err != nil {
			log.Printf("error closing solutions: %v", err)
		}

		if !sentAny {
			reply := nats.NewMsg(msg.Reply)
			reply.Header.Add("Status", "Failed")
			nc.PublishMsg(reply)
			//indicate error
		}
	}
}
