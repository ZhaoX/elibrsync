all:
	./rebar compile
	./rebar eunit

compile:
	./rebar compile

test:
	./rebar eunit

clean:
	./rebar clean

xref: 
	./rebar xref
