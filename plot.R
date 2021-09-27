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
mutate(test_result = case_when(grepl("pos", test_type) ~ "pos", grepl("neg", test_type) ~ "neg")) %>%
unite(test_name, test_result, col="test_and_result") -> out

say('tidy')
out %>%
arrange(ID)

# heart, lung, liver, pre

out %>% filter(grepl("heart", ORGAN) & pre_post == 2) -> heart
out %>% filter(grepl("lung", ORGAN) & pre_post == 2) -> lung
out %>% filter(grepl("liver", ORGAN) & pre_post == 2 & ! grepl("Multi", ORGAN)) -> liver
out %>% filter(pre_post == 1) -> pre




#### Plot

ggplot(heart, aes(x = test_time, y = ID, shape = test_and_result)) + labs(title="Heart") + geom_point(alpha=0.8) +
 scale_shape_manual(values=c(2,17,1,16)) -> heartplot
#                           ^  ^^  O OO
#                           a- a+  p- p+

ggplot(lung, aes(x = test_time, y = ID, shape = test_and_result)) + labs(title="Lung") + geom_point() +
 scale_shape_manual(values=c(17,16)) -> lungplot
#                           ad hoc
 
ggplot(liver, aes(x = test_time, y = ID, shape = test_and_result)) + labs(title="Liver") + geom_point(alpha=0.8) +
 scale_shape_manual(values=c(17,1,16)) -> liverplot
 #          ad hoc, no ab_neg
 
ggplot(pre, aes(x = test_time, y = ID, shape = test_and_result)) + labs(title="Pre-transplant") + geom_point(alpha=0.8) +
 scale_shape_manual(values=c(2,17,1,16)) -> preplot




#### Write to plot files

say('\n\n----\n\nEnd of text output. Now plotting.')
pdf(here("outputs", "Rplots.pdf"))
heartplot
lungplot
liverplot
preplot
dev.off()

# ggsave png here if needed
