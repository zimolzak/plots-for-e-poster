analyses = Routputs.txt
analyses_unmentioned = Rplots.pdf

.PHONY: all

all: $(analyses)

positively_renamed.csv: ../rename_vars.pl ../pcr-chart-data-no-phi-id.csv
	../rename_vars.pl ../pcr-chart-data-no-phi-id.csv > $@

#### Analyses

Routputs.txt: ../plot.R positively_renamed.csv
	Rscript $< > Routputs.txt
