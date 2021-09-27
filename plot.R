library(dplyr)
library(ggplot2)
library(here)
library(tidyr)




#### Functions

filename2df <- function(fname) {
    df = read.csv(here("outputs", fname), sep=",",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

say = function(s) {
	sL = c('\n', s, ' ----\n')
	cat(paste(sL, collapse=''))
}




#### Load data

input_df = filename2df('positively_renamed.csv')

say('Dimensions of input')
dim(input_df)

say('Example')
head(input_df)




#### Write to plot files

say('\n\n----\n\nEnd of text output. Now plotting.')
#pdf(here("outputs", "Rplots.pdf"))
#derp
#dev.off()
# ggsave png here if needed
