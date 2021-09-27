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

input_df %>%
gather(matches("_test"), key="test_type", value="test_time", na.rm=TRUE) %>%
rename(pcr.n = patient.number.for.PCR.chart, charac.n = Characteristics.chart.,
    pre_post = Transplant.Status..Pre.1..Post.2) %>%
mutate(test_name = case_when(grepl("pcr", test_type) ~ "pcr", grepl("ab", test_type) ~ "ab")) %>%
mutate(test_result = case_when(grepl("pos", test_type) ~ "pos", grepl("neg", test_type) ~ "neg")) -> out

say('tidy')
out %>%
arrange(ID)




#### Plot

ggplot(out, aes(x = test_time, y = ID, shape = test_name, color = test_result)) +
 geom_point() -> simple




#### Write to plot files

say('\n\n----\n\nEnd of text output. Now plotting.')
pdf(here("outputs", "Rplots.pdf"))
simple
dev.off()

# ggsave png here if needed
