REBAR:=./rebar

## If rebar.config file doesn't exist, just default to mochiweb backend
all:
ifeq ("$(wildcard rebar.config)","")
	@(echo "No backend specified. Defaulting to mochiweb")
	@($(MAKE) mochiweb)
else
	@($(MAKE) get-deps compile copy-static)
endif

help:
	@(echo)
	@(echo "Build NitrogenProject.com with a custom backend")
	@(echo)
	@(echo "   make [cowboy|inets|mochiweb|webmachine|yaws]")
	@(echo)
	@(echo "Execute NitrogenProject.com")
	@(echo)
	@(echo "   make run")
	@(echo)

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

compile:
	$(REBAR) compile

link-static:
	(cd static; rm -fr nitrogen; ln -s ../deps/nitrogen_core/www nitrogen)
	(cd static; rm -fr doc; ln -s ../deps/nitrogen_core/doc/html doc)

copy-static:
	(cd static; rm -rf nitrogen; mkdir nitrogen; cp -r ../deps/nitrogen_core/www/* nitrogen)
	(cd static; rm -rf doc; mkdir doc; cp -r ../deps/nitrogen_core/doc/html/* doc)

clean:
	$(REBAR) clean

wipe_deps:
	rm -fr deps/*

cowboy:
	@($(MAKE) platform PLATFORM=cowboy)

inets:
	@($(MAKE) platform PLATFORM=inets)

mochiweb:
	rm -fr deps/mochiweb
	@($(MAKE) platform PLATFORM=mochiweb)

webmachine:
	rm -fr deps/mochiweb
	@($(MAKE) platform PLATFORM=webmachine)

yaws:
	@($(MAKE) platform PLATFORM=yaws)

platform:
	@(echo "Fetching initial dependencies...")
	($(REBAR) --config rebar.base.config get-deps)
	@(deps/simple_bridge/rebar_deps/merge_deps.escript rebar.base.config deps/simple_bridge/rebar_deps/$(PLATFORM).deps rebar.config)
	@(echo "Updating app.config...")
	@(sed 's/{backend, [a-z]*}/{backend, $(PLATFORM)}/' < app.config > app.config.temp)
	@(mv app.config.temp app.config)
	@($(MAKE) get-deps compile copy-static)


upgrade: update-deps compile copy-static

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib sasl crypto compiler syntax_tools

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@(dialyzer --statistics --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r ./deps/)

dialyzer: $(DEPS_PLT)
	@(dialyzer --statistics --verbose --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin)


ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: $(ERLANG_VERSION)

17:
	$(MAKE) inets
	$(MAKE) mochiweb
	$(MAKE) webmachine
	$(MAKE) yaws
18: 17
19: cowboy 17
	$(MAKE) cowboy
20: 19
	$(MAKE) wipe_deps clean
	$(MAKE) dialyzer
21: 20


TESTLOG:=testlog.log

run:
	erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-config "app.config" \
	-name nitrogen@127.0.0.1 \
	-testlog "$(TESTLOG)" \
	-env ERL_FULLSWEEP_AFTER 0 \
	-eval "inets:start()" \
	-eval "application:start(nitrogen_website)."

test:
	erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-config "app.config" \
	-name nitrogen@127.0.0.1 \
	-env ERL_FULLSWEEP_AFTER 0 \
	-testlog "$(TESTLOG)" \
	-eval "inets:start()" \
	-eval "application:start(nitrogen_website)." \
	-eval "wf_test:start_all(nitrogen_website)."

TESTLOGDIR:=testlogs/$(shell date +"%Y-%m-%d.%H%M%S")

test_inets:
	$(MAKE) inets test TESTLOG="$(TESTLOGDIR)/inets.log"

test_cowboy:
	$(MAKE) cowboy test TESTLOG="$(TESTLOGDIR)/cowboy.log"

test_mochiweb:
	rm -fr deps/mochiweb
	$(MAKE) mochiweb test TESTLOG="$(TESTLOGDIR)/mochiweb.log"

test_webmachine:
	rm -fr deps/mochiweb deps/ibrowse
	$(MAKE) webmachine test TESTLOG="$(TESTLOGDIR)/webmachine.log"

test_yaws:
	$(MAKE) yaws test TESTLOG="$(TESTLOGDIR)/yaws.log"

test_all:
	$(MAKE) test_cowboy test_inets test_mochiweb test_webmachine test_yaws TESTLOGDIR=$(TESTLOGDIR)
	@(grep SUMMARY $(TESTLOGDIR)/*.log)
	@(echo "All tests summarized in $(TESTLOGDIR)")
