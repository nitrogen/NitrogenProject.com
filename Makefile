all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile
	(cd static; rm -rf nitrogen; mkdir nitrogen; cp -r ../deps/nitrogen_core/www/* nitrogen)
	(cd static; rm -rf doc; mkdir doc; cp -r ../deps/nitrogen_core/doc/html/* doc)

clean:
	./rebar clean

run:
	erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-config "app.config" \
	-name nitrogen@127.0.0.1 \
	-eval "application:start(nitrogen_website)."
