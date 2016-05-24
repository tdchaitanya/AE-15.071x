#1.1
wiki <- read.csv("wiki.csv",stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
summary(wiki)
#1.2
library(tm)
