
all: deps
	./rebar compile

deps:
	./rebar get-deps

dist:
	rm -rf rel/esyslog
	./rebar generate

clean:
	./rebar clean
