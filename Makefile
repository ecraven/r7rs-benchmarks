.PHONY: csv clean all

all:
	./bench "bigloo bones chez chibi chicken foment gambitc gauche guile ironscheme kawa larceny mit mosh petite picrin racket rhizome rscheme sagittarius scheme48 s9fes vicare ypsilon" all

csv:
	grep -h '+!CSVLINE' results.* | sed 's/+!CSVLINE!+//' > all.csv

clean:
	rm -f results.* all.csv outputs/*
