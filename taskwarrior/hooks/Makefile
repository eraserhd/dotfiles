prefix		= /usr/local
bin_PROGRAMS	= on-modify-plumb

on-modify-plumb: on-modify-plumb.scm json.scm
	gsc -exe -o $@ $^

install: on-modify-plumb
	mkdir -p $(prefix)/bin
	cp on-modify-plumb $(prefix)/bin/taskwarrior-hooks-on-modify-plumb
