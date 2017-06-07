.PHONY: csv clean all

doc:
	echo Run make all to run all tests
all.csv:
	grep -a -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv
clean:
	rm -f results.* all.csv outputs/*
benchmark.html: all.csv
	mit-scheme --load "graph.scm" --eval '(%exit 0)'
include Makefile.schemes
