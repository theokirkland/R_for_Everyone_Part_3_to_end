
library(tidyverse)
acs <- read.table("http://jaredlander.com/data/acs_ny.csv",
sep="," , header=TRUE, stringsAsFactors=FALSE)

acs$Income <- with(acs, FamilyIncome > 150000)
library(useful)


ggplot(acs, aes(x=FamilyIncome)) +
geom_density(fill="grey", color="grey") +
geom_vline(xintercept=150000) +
scale_x_continuous(label=multiple.dollar, limits=c(0, 1000000))
