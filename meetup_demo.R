# From Article: Learning Seattle’s Work Habits from Bicycle Counts (with R!)
# http://rinzewind.org/blog-en/2015/learning-seattles-work-habits-from-bicycle-counts-with-r.html

library(RCurl)
library(ggplot2)
theme_set(theme_bw(12))
library(reshape2)
library(mclust)
# Output should not be in Spanish
Sys.setlocale(category = "LC_ALL", locale = "C")
# knitr options
library(knitr)
opts_chunk$set(fig.width = 10, fig.height = 7)

# read in from .csv
bikes = read.csv("bikeData.csv", header = T, sep = ",")
summary(bikes)