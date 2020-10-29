
library(tidyverse)
acs <- read.table("http://jaredlander.com/data/acs_ny.csv",
sep="," , header=TRUE, stringsAsFactors=FALSE)

acs$Income <- with(acs, FamilyIncome > 150000)
library(useful)


ggplot(acs, aes(x=FamilyIncome)) +
geom_density(fill="grey", color="grey") +
geom_vline(xintercept=150000) +
scale_x_continuous(label=multiple.dollar, limits=c(0, 1000000))

head(acs)


income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType,
data=acs, family=binomial(link="logit"))

summary(income1)


library(coefplot)
coefplot(income1)


#Interpreting the coefficients from a logistic regression necessitates taking the inverse logit.

invlogit <- function(x)
  {
 1 / (1 + exp(-x))
  }

invlogit(income1$coefficients)

ggplot(acs, aes(x=NumChildren)) +
         geom_histogram(binwidth = 1)

children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
data=acs, family=poisson(link="log"))

summary(children1)

coefplot(children1)


#A particular concern with Poisson regression is overdispersion, which means that the variability seen in the data is greater than is theorized by the Poisson distribution where the mean and variance are the same.

children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data=acs, family=quasipoisson(link="log"))

multiplot(children1, children2)

#Survival

library(survival)
head(bladder)
bladder[100:105, ]

#Surv function
survObject <- with(bladder[100:105, ], Surv(stop, event))
survObject
#matrix
survObject[ , 1:2]

#This shows that for the first three rows where an event occurred, the
#time is known to be 12, whereas the bottom two rows had no event, so the time is censored because an event could have occurred afterward. Perhaps the most common modelling technique in survival analysis is using a Cox proportional hazards model, which in R is done with coxph. The model is fitted using the familiar formula interface supplied to coxph. The survfit function builds the survival curve that can then be plotted as shown in Figure 20.6. The survival curve shows the percentage of participants surviving at a given time. The summary is similar to other summaries but tailored to survival analysis.

cox1 <- coxph(Surv(stop, event) ~ rx + number + size + enum, data = bladder)
summary(cox1)

plot(survfit(cox1), xlab="Days", ylab="Survival Rate", conf.int=TRUE)

cox2 <- coxph(Surv(stop, event) ~ strata(rx) + number + size + enum, data = bladder)
summary(cox2)

plot(survfit(cox2), xlab="Days", ylab="Survival Rate", conf.int=TRUE, col=1:2)

legend("bottomleft", legend=c(1, 2), lty=1, col=1:2, text.col=1:2, title="rx")

#Testing the assumption of proportional hazards is done with cox.zph.

cox.zph(cox1)

cox.zph(cox2)


head(bladder2)

#Anderson_Gill survival curves for bladder2 data
ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size + enum + cluster(id), data=bladder2)


ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum + cluster(id), data=bladder2)

plot(survfit(ag1), conf.int = TRUE)
plot(survfit(ag2), conf.int = TRUE, col=1:2)
legend("bottomleft", legend=c(1, 2), lty=1, col=1:2, text.col=1:2, title="rx")
