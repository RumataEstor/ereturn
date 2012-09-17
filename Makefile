
compile:
	./rebar compile
	@if [[ -n "${FILE}" ]]; then erlc -pa ebin -E -DDEBUG -DTEST ${FILE}; fi

clean:
	./rebar clean

dialyzer: compile
	echo "-*- mode: compilation-minor -*-">dialyzer_result_a
	-dialyzer ebin --get_warnings -Wrace_conditions -Wunderspecs>>dialyzer_result_a
	grep -x -v -f .dialyzer_exceptions<dialyzer_result_a>dialyzer_result

test: compile always_make
	find test -name '*.erl' \! -name '*_test.erl' | xargs touch
	./rebar eunit app=ereturn

always_make:
