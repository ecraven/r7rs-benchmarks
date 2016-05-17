.PHONY: csv clean all

doc:
	echo Run make all to run all tests
csv:
	grep -a -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv
clean:
	rm -f results.* all.csv outputs/*

include Makefile.schemes
