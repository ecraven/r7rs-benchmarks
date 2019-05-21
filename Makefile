.PHONY: csv clean all

doc:
	echo Run make all to run all tests
csv: all.csv

all.csv:
	grep -a -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

html: index.html benchmark.html csv

index.html: all.csv
	mit-scheme --load "graph.scm" --eval '(%exit 0)'

clean:
	rm -f results.* all.csv outputs/*

chicken-dependencies:
	chicken-install r7rs srfi-1 srfi-13 srfi-14 srfi-69

include Makefile.schemes
