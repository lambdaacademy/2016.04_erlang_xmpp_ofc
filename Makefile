.PHONY: compile upgrade clean deep-clean dialyzer shell rel ct

compile: rebar3
	./rebar3 compile

upgrade: rebar3
	./rebar3 upgrade

clean: rebar3
	./rebar3 clean

deep-clean:
	./rebar3 clean -a

dialyzer: rebar3
	./rebar3 dialyzer

shell: compile
	erl -pa _build/default/lib/*/ebin \
	-config config/sys \
	-eval "application:ensure_all_started(xmpp_ofc)"

rebar3:
	wget -c https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3


rel:
	./rebar3 release

ct:
	./rebar3 ct
