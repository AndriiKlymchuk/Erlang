TEST_FILE="h1_tests"

CMD=$(subst filename,${TEST_FILE},"eunit:test(filename, [verbose])")

all:
	erl -make

test: all
	erl -noshell -pa ebin -eval ${CMD} -s init stop

clean:
	rm -rf ebin/*
