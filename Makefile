.PHONY: csv clean all

doc:
	echo Run make all to run all tests
csv: all.csv

all.csv: $(wildcard results.*)
	grep -a -h '^+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

html: index.html benchmark.html csv

index.html: all.csv graph.scm
	chez --script "graph.scm"

clean:
	rm -f results.* all.csv outputs/*

chicken-dependencies:
	chicken-install r7rs srfi-1 srfi-13 srfi-14 srfi-69

racket-dependencies:
	sudo raco pkg install --scope installation r7rs

include Makefile.schemes
