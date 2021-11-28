// Rules are logical implications producing facts
package rules

import (
	"github.com/cayleygraph/cayley"
	"sync"
)

type (
	Rule struct {
		Update func(handle *cayley.Handle) (modified bool, err error)
	}
)

var lock sync.Mutex
var rules []Rule

func RegisterRule(rule Rule) error {
	lock.Lock()
	defer lock.Unlock()
	rules = append(rules, rule)
	return nil
}

func Process(handle *cayley.Handle) (bool, error) {
	var modified bool
	updatedLastPass := true
	for updatedLastPass {
		updatedLastPass = false
		for _, rule := range rules {
			ruleModified, err := rule.Update(handle)
			if ruleModified {
				modified = true
				updatedLastPass = true
			}
			if err != nil {
				return modified, err
			}
		}
	}
	return modified, nil
}
