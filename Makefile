.PHONY: csv clean all

all:
	./bench "bigloo bones chez chibi chicken foment gambitc gauche guile kawa larceny mit petite picrin racket sagittarius scheme48 vicare" all

csv:
	grep -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

clean:
	rm -f results.* all.csv outputs/*
