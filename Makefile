all:

.PHONY: csv clean

csv:
	grep -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

clean:
	rm -f results.* all.csv outputs/*
