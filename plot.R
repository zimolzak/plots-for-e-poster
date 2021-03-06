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

say = function(mystr) {
    mystr_formatted = c('\n', mystr, ' ----\n')
    cat(paste(mystr_formatted, collapse=''))
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
mutate_at(vars(ORGAN), funs(tolower(.))) %>%
unite(test_name, test_result, col="test_and_result") %>%
filter(! grepl("multi", ORGAN)) -> out

say('tidy')
out %>%
arrange(ID)

out %>% filter(grepl("heart", ORGAN) & pre_post == 2) -> heart
out %>% filter(grepl("kidney", ORGAN) & pre_post == 2 & ! grepl("Multi", ORGAN)) -> kidney
out %>% filter(grepl("liver", ORGAN) & pre_post == 2 & ! grepl("Multi", ORGAN)) -> liver
out %>% filter(pre_post == 1) -> pre




#### Plots

plot_timeline = function(df, mytitle, myshapes) {
    XLABEL = "Days after positive SARS-CoV-2 PCR"
    MYALPHA = 0.6
    POINTSIZE = 5
    XCUTOFF = 150
    ggplot(df, aes(x = test_time, y = ID, shape = test_and_result)) +
        labs(y = "", x = XLABEL, title = mytitle) +
        geom_point(alpha = MYALPHA, size = POINTSIZE) +
        scale_shape_manual(values = myshapes) +
        scale_x_continuous(breaks = (0:5) * 30, limits = c(0, XCUTOFF)) +
        theme(legend.position = "none", axis.text.y = element_blank(),
            panel.background = element_blank(), axis.line = element_line()) -> myplot
    return(myplot)
}

plot_timeline(heart, "Heart", c(2, 17, 1, 16)) -> heartplot
plot_timeline(kidney, "Kidney", c(2, 17, 1, 16)) -> kidneyplot
plot_timeline(liver, "Liver", c(17, 1, 16)) -> liverplot  # ad hoc, no ab_neg
plot_timeline(pre, "Pre-transplant", c(2, 17, 1, 16)) -> preplot




#### Write to plot files

MYWIDTH = 11
MYHEIGHT = 7

say('\n\n----\n\nEnd of text output. Now plotting.')
pdf(here("outputs", "Rplots.pdf"), MYWIDTH, MYHEIGHT)
heartplot
kidneyplot
liverplot
preplot
dev.off()

ggsave("heart.png", heartplot, width = MYWIDTH, height = MYHEIGHT)
ggsave("kidney.png", kidneyplot, width = MYWIDTH, height = MYHEIGHT)
ggsave("liver.png", liverplot, width = MYWIDTH, height = MYHEIGHT)
ggsave("pretransplant.png", preplot, width = MYWIDTH, height = MYHEIGHT)
