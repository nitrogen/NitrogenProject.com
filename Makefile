all: get-deps compile copy-static

get-deps:
	./rebar get-deps

compile:
	./rebar compile

copy-static:
	(cd static; rm -rf nitrogen; mkdir nitrogen; cp -r ../deps/nitrogen_core/www/* nitrogen)
	(cd static; rm -rf doc; mkdir doc; cp -r ../deps/nitrogen_core/doc/html/* doc)

clean:
	./rebar clean

run:
	erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-config "app.config" \
	-name nitrogen@127.0.0.1 \
	-env ERL_FULLSWEEP_AFTER 0 \
	-sync sync_mode nitrogen \
	-eval "application:start(nitrogen_website)."
