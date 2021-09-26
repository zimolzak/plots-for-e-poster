analyses = Rplots.pdf
analyses_unmentioned = Routputs.txt

.PHONY: all

all: $(analyses)

#### Analyses

Rplots.pdf: ../plot.R
	Rscript $< > Routputs.txt
