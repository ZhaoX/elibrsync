all:
	./rebar get-deps
	./rebar compile
	mkdir .eunit && cp -r test/data .eunit/
	./rebar eunit
	rm -rf .eunit/data

compile:
	./rebar compile

test:
	mkdir -p .eunit && cp -r test/data .eunit/
	./rebar eunit
	rm -rf .eunit/data

clean:
	./rebar clean

xref: 
	./rebar xref
