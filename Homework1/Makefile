TESTS="h1_tests"

CMD=$(subst filename,${TESTS},"eunit:test(filename, [verbose])")

all:
	erl -make

test: all
	erl -noshell -pa ebin -eval ${CMD} -s init stop