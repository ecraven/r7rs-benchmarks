.PHONY: csv clean all

all:
	./bench "bigloo bones chez chibi chicken foment gambitc gauche guile ironscheme kawa larceny mit mosh petite picrin racket sagittarius scheme48 vicare ypsilon" all

csv:
	grep -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

clean:
	rm -f results.* all.csv outputs/*
