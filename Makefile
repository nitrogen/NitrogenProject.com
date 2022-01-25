REBAR:=./rebar3

## If rebar.config file doesn't exist, just default to mochiweb backend
all: cowboy

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

compile:
	$(REBAR) compile

link-static:
	(rm -fr priv/static/nitrogen; ln -s `pwd`/_build/default/lib/nitrogen_core/www priv/static/nitrogen)
	(rm -fr priv/static/doc; ln -s `pwd`/_build/default/lib/nitrogen_core/doc/markdown priv/static/doc)

copy-static:
	(rm -rf priv/static/nitrogen; mkdir priv/static/nitrogen; cp -r `pwd`/_build/default/lib/nitrogen_core/www/* priv/static/nitrogen)
	(rm -rf priv/static/doc; mkdir priv/static/doc; cp -r `pwd`/_build/default/lib/nitrogen_core/doc/markdown/* priv/static/doc)

deps:
	$(REBAR) deps

clean:
	$(REBAR) clean

unlock:
	$(REBAR) unlock --all

cowboy:
	@($(MAKE) platform PLATFORM=cowboy)

inets:
	@($(MAKE) platform PLATFORM=inets)

mochiweb:
	@($(MAKE) platform PLATFORM=mochiweb)

webmachine:
	@($(MAKE) platform PLATFORM=webmachine)

yaws:
	@($(MAKE) platform PLATFORM=yaws)

platform: unlock
	@(echo $(PLATFORM) > last_platform)
	@(echo "Updating app.config...")
	@(sed 's/{backend, [a-z]*}/{backend, $(PLATFORM)}/' < app.config > app.config.temp)
	@(mv app.config.temp app.config)
	$(REBAR) as $(PLATFORM) deps
	make copy-static
	$(REBAR) as $(PLATFORM) compile

upgrade: update-deps compile copy-static

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib sasl crypto compiler syntax_tools

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@(dialyzer --statistics --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r ./deps/)

dialyzer: $(DEPS_PLT)
	@(dialyzer --statistics --verbose --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin)


travis: test

TESTLOG:=testlog.log

run:
	$(REBAR) as `cat last_platform` run

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
	$(MAKE) mochiweb test TESTLOG="$(TESTLOGDIR)/mochiweb.log"

test_webmachine:
	$(MAKE) webmachine test TESTLOG="$(TESTLOGDIR)/webmachine.log"

test_yaws:
	$(MAKE) yaws test TESTLOG="$(TESTLOGDIR)/yaws.log"

test_all:
	$(MAKE) test_cowboy test_inets test_mochiweb test_webmachine test_yaws TESTLOGDIR=$(TESTLOGDIR)
	@(grep SUMMARY $(TESTLOGDIR)/*.log)
	@(echo "All tests summarized in $(TESTLOGDIR)")
