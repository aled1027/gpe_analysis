library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("ggplot2")

setwd(".")
dat <- read.csv("data/gpe_tfidf_binned_yearly.csv")
term_buckets <- read.csv("data/term_buckets.csv")

ylim_min <- -0.003
ylim_max <- 0.004
xlim_min <- 1990
xlim_max <- 2012

ylim_max_x_bar_d <- 0.015
ylim_min_x_bar_d <- 0.0

plot_group_x_bar <- function(df, traits, title, filename) {
  ggplot(subset(df,trait %in% traits)) + 
    geom_line(aes(Year, X_bar_d, group=trait, colour=trait)) +
    coord_cartesian(xlim=c(xlim_min, xlim_max), ylim=c(ylim_min_x_bar_d, ylim_max_x_bar_d)) +
    xlab("Year") +
    ylab("X_bar_d") +
    ggtitle(title)
  ggsave(filename)
}

plot_group_delta_x <- function(df, traits, title, filename) {
  ggplot(subset(df,trait %in% traits)) + 
    geom_line(aes(Year, total, group=trait, colour=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("delta X") +
    ggtitle(title)
  ggsave(filename)
}

plot_group_t2 <- function(df, traits, title, filename) {
  ggplot(subset(df,trait %in% traits)) + 
    geom_line(aes(Year, t2, group=trait, colour=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 2") +
    ggtitle(title)
  ggsave(filename)
}

plot_group_t3 <- function(df, traits, title, filename) {
  ggplot(subset(df,trait %in% traits)) + 
    geom_line(aes(Year, t3, group=trait, colour=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 3") +
    ggtitle(title)
  ggsave(filename)
}

plot_group_t1 <- function(df, traits, title, filename) {
  ggplot(subset(df,trait %in% traits)) + 
    geom_smooth(aes(Year, t1, group=trait, colour=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 1") +
    ggtitle(title)
  ggsave(filename)
}

plot_average_t1 <- function(df, traits, title, filename) {
  ggplot(subset(df, trait %in% traits)) +
    geom_smooth(aes(Year, t1)) +
    geom_point(aes(Year, t1, group=trait, color=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 1") +
    ggtitle(title)
  ggsave(filename)
}

plot_average_t2 <- function(df, traits, title, filename) {
  ggplot(subset(df, trait %in% traits)) +
    geom_smooth(aes(Year, t2)) +
    geom_point(aes(Year, t2, group=trait, color=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 2") +
    ggtitle(title)
  ggsave(filename)
}

plot_average_t3 <- function(df, traits, title, filename) {
  ggplot(subset(df, trait %in% traits)) +
    geom_smooth(aes(Year, t3)) +
    geom_point(aes(Year, t3, group=trait, color=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 3") +
    ggtitle(title)
  ggsave(filename)
}

plot_average_delta_x <- function(df, traits, title, filename) {
  ggplot(subset(df, trait %in% traits)) +
    geom_smooth(aes(Year, total)) +
    geom_point(aes(Year, total, group=trait, color=trait)) +
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("delta X") +
    ggtitle(title)
  ggsave(filename)
}

plot_average_x_bar <- function(df, traits, title, filename) {
  ggplot(subset(df, trait %in% traits)) +
    geom_smooth(aes(Year, X_bar_d)) +
    geom_point(aes(Year, X_bar_d, group=trait, color=trait)) +
    coord_cartesian(xlim=c(xlim_min, xlim_max), ylim=c(ylim_min_x_bar_d, ylim_max_x_bar_d)) +
    xlab("Year") +
    ylab("X_bar_d") +
    ggtitle(title)
  ggsave(filename)
}

make_row_plots <- function(dat, term_buckets) {
  for (term_bucket in names(term_buckets)) {
    term = "t1"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, ".png", sep="")
    plot_group_t1(dat, term_buckets[[term_bucket]], title, filename)
    
    term = "t2"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, ".png", sep="")
    plot_group_t2(dat, term_buckets[[term_bucket]], title, filename)
    
    term = "t3"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, ".png", sep="")
    plot_group_t3(dat, term_buckets[[term_bucket]], title, filename)
    
    term = "delta_x"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, ".png", sep="")
    plot_group_delta_x(dat, term_buckets[[term_bucket]], title, filename)
    
    term = "x_bar_d"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, ".png", sep="")
    plot_group_x_bar(dat, term_buckets[[term_bucket]], title, filename)
    
    term = "t1"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, "_avg", ".png", sep="")
    plot_average_t1(dat, term_buckets[[term_bucket]], title, filename) 

    term = "t2"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, "_avg", ".png", sep="")
    plot_average_t2(dat, term_buckets[[term_bucket]], title, filename) 

    term = "t3"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, "_avg", ".png", sep="")
    plot_average_t3(dat, term_buckets[[term_bucket]], title, filename) 

    term = "delta_x"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, "_avg", ".png", sep="")
    plot_average_delta_x(dat, term_buckets[[term_bucket]], title, filename) 

    term = "x_bar"
    title <- paste(term_bucket, term, sep=" ")
    filename <- paste("group_plots/", term_bucket, "_", term, "_avg", ".png", sep="")
    plot_average_x_bar(dat, term_buckets[[term_bucket]], title, filename) 
  }
}

aggregate_t1 <- function(dat, term_buckets) {
  ggplot() +
    geom_smooth(data=subset(dat, trait %in% term_buckets$chemical), aes(Year, t1, color="chemical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$internet), aes(Year, t1, color="internet")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$computers), aes(Year, t1, color="computers")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$electrical), aes(Year, t1, color="electrical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$mechanical), aes(Year, t1, color="mechanical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$medical), aes(Year, t1, color="medical")) +
    scale_colour_manual(values = c("chemical"="black",
                                   "internet"="orange",
                                   "computers"="blue", 
                                   "electrical" = "purple",
                                   "mechanical"="green",
                                   "medical"="red")) + 
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 1") +
    ggtitle("Term 1")
  ggsave("group_plots/aggregate_t1.png")
}

aggregate_t2 <- function(dat, term_buckets) {
  ggplot() +
    geom_smooth(data=subset(dat, trait %in% term_buckets$chemical), aes(Year, t2, color="chemical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$internet), aes(Year, t2, color="internet")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$computers), aes(Year, t2, color="computers")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$electrical), aes(Year, t2, color="electrical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$mechanical), aes(Year, t2, color="mechanical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$medical), aes(Year, t2, color="medical")) +
    scale_colour_manual(values = c("chemical"="black",
                                   "internet"="orange",
                                   "computers"="blue", 
                                   "electrical" = "purple",
                                   "mechanical"="green",
                                   "medical"="red")) + 
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 2") +
    ggtitle("Term 2")
  ggsave("group_plots/aggregate_t2.png")
}

aggregate_t3 <- function(dat, term_buckets) {
  ggplot() +
    geom_smooth(data=subset(dat, trait %in% term_buckets$chemical), aes(Year, t3, color="chemical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$internet), aes(Year, t3, color="internet")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$computers), aes(Year, t3, color="computers")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$electrical), aes(Year, t3, color="electrical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$mechanical), aes(Year, t3, color="mechanical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$medical), aes(Year, t3, color="medical")) +
    scale_colour_manual(values = c("chemical"="black",
                                   "internet"="orange",
                                   "computers"="blue", 
                                   "electrical" = "purple",
                                   "mechanical"="green",
                                   "medical"="red")) + 
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("term 3") +
    ggtitle("Term 3")
  ggsave("group_plots/aggregate_t3.png")
}

aggregate_delta_x <- function(dat, term_buckets) {
  ggplot() +
    geom_smooth(data=subset(dat, trait %in% term_buckets$chemical), aes(Year, total, color="chemical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$internet), aes(Year, total, color="internet")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$computers), aes(Year, total, color="computers")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$electrical), aes(Year, total, color="electrical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$mechanical), aes(Year, total, color="mechanical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$medical), aes(Year, total, color="medical")) +
    scale_colour_manual(values = c("chemical"="black",
                                   "internet"="orange",
                                   "computers"="blue", 
                                   "electrical" = "purple",
                                   "mechanical"="green",
                                   "medical"="red")) + 
    coord_cartesian(xlim=c(xlim_min,xlim_max), ylim=c(ylim_min,ylim_max)) + 
    xlab("Year") +
    ylab("delta X") +
    ggtitle("Delta X")
  ggsave("group_plots/aggregate_delta_x.png")
}

aggregate_x_bar <- function(dat, term_buckets) {
  ggplot() +
    geom_smooth(data=subset(dat, trait %in% term_buckets$chemical), aes(Year, X_bar_d, color="chemical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$internet), aes(Year, X_bar_d, color="internet")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$computers), aes(Year, X_bar_d, color="computers")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$electrical), aes(Year, X_bar_d, color="electrical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$mechanical), aes(Year, X_bar_d, color="mechanical")) +
    geom_smooth(data=subset(dat, trait %in% term_buckets$medical), aes(Year, X_bar_d, color="medical")) +
    scale_colour_manual(values = c("chemical"="black",
                                   "internet"="orange",
                                   "computers"="blue", 
                                   "electrical" = "purple",
                                   "mechanical"="green",
                                   "medical"="red")) + 
    coord_cartesian(xlim=c(xlim_min, xlim_max), ylim=c(ylim_min_x_bar_d, ylim_max_x_bar_d)) +
    xlab("Year") +
    ylab("X_bar_d") +
    ggtitle("X_bar_d")
  ggsave("group_plots/aggregate_x_bar_d.png")
}


aggregate_t1(dat, term_buckets)
aggregate_t2(dat, term_buckets)
aggregate_t3(dat, term_buckets)
aggregate_delta_x(dat, term_buckets)
aggregate_x_bar(dat, term_buckets)
make_row_plots(dat, term_buckets)
