library(ggplot2)
library(dplyr)
library(tidyr)

setwd(".")
dat <- read.csv("data/gpe_tfidf_binned_yearly.csv")
term_buckets <- read.csv("data/term_buckets.csv")
pops <- read.csv("data/pops.csv")

xlim_min <- 1990
xlim_max <- 2012


plot_freq <- function(dat, the_trait) {
    print(the_trait)
    ggplot(dat) + 
        geom_line(aes(Year, Pd, colour="desc"), data=subset(dat, trait==the_trait),stat="identity") +
        geom_line(aes(Year, Pa, colour="anc"), data=subset(dat, trait==the_trait),stat="identity") +
        coord_cartesian(xlim=c(xlim_min, xlim_max)) + 
        xlab("Year") +
        ylab("Number of patents") +
        ggtitle(the_trait) +
        scale_colour_manual(values = c("desc"="blue",
                                       "anc"="green"))
    filename <- paste("images/", the_trait, "_freq", ".png", sep="")
    ggsave(filename)
}

for (term_bucket in names(term_buckets)) {
    for (trait in term_buckets[[term_bucket]]) {
        if (trait != "" && trait != "rout") {
            plot_freq(dat, trait)
        }
    }
}
#plot_freq(dat, "internet")

