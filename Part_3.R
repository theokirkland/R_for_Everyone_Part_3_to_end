
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

#Evaluating models

housing <- read.table("http://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,stringsAsFactors = FALSE)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
"SqFt", "Income", "IncomePerSqFt", "Expense",
"ExpensePerSqFt", "NetIncome", "Value",
"ValuePerSqFt", "Boro")

head(housing)

housing <- housing[housing$Units < 1000, ]

head(housing)

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)

library(coefplot)
coefplot(house1)


#For linear regression, three important residual plots are: fitted values against residuals, Q-Q plots and the histogram of the residuals. The first is easy enough with ggplot2. Fortunately, ggplot2 has a handy trick for dealing with lm models. We can use the model as the data source and ggplot2 “fortifies” it, creating new columns, for easy plotting.

library(ggplot2)

head(fortify(house1))
h1 <- ggplot(aes(x=.fitted, y=.resid), data = house1) +
geom_point() +
geom_hline(yintercept = 0) +
geom_smooth(se = FALSE) +
labs(x="Fitted Values", y="Residuals")
h1
h1 + geom_point(aes(color=Boro))

plot(house1, which=2)

ggplot(house1, aes(sample=.stdresid)) + stat_qq() + geom_abline()

#poor fit at extremes show model is poor

ggplot(house1, aes(x=.resid)) + geom_histogram()

#Residuals not normally distributed
names(housing)
house2 <- lm(ValuePerSqFt ~ Units *SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data = housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class, data = housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data = housing)


multiplot(house1, house2, house3, house4, house5)

#Anova is sort of a cheater approach

anova(house1, house2, house3, house4, house5)

#This shows that the fourth model, house4, has the lowest RSS, meaning it is the best model of the bunch. The problem with RSS is
#that it always improves when an additional variable is added to the model. This can lead to excessive model complexity and overfitting. Another metric, which penalizes model complexity, is the Akaike Information Criterion (AIC). As with RSS, the model with the lowest AIC—even negative values—is considered optimal. The BIC (Bayesian Information Criterion) is a similar measure where, once again, lower is better.


#The AIC and BIC for our models are calculated using the AIC and BIC functions, respectively.

AIC(house1, house2, house3, house4, house5)

#AIC of house4 is lowest, so that is the best of these 5 models

BIC(house1, house2, house3, house4, house5)

#Same with BIC

#When called on glm models, anova returns the deviance of the model, which is another measure of error. The general rule of thumb—according to Andrew Gelman—is that for every added variable in the model, the deviance should drop by two. For categorical (factor)
#variables, the deviance should drop by two for each level. To illustrate we make a binary variable out of ValuePerSqFt and fit a few logistic regression models.

housing$HighValue <- housing$ValuePerSqFt >= 150
 high1 <- glm(HighValue ~ Units + SqFt + Boro, data = housing, family = binomial(link = "logit"))
 
 high2 <- glm(HighValue ~ Units * SqFt + Boro, data = housing, family = binomial(link = "logit"))
 
 high3 <- glm(HighValue ~ Units + SqFt * Boro + Class, data = housing, family = binomial(link = "logit"))

 high4 <- glm(HighValue ~ Units + SqFt * Boro + SqFt *Class, data = housing, family = binomial(link = "logit")) 
 

 high5 <- glm(HighValue ~ Boro + Class,  data = housing, family = binomial(link = "logit")) 

anova(high1, high2, high3, high4, high5) 


AIC(high1, high2, high3, high4, high5)

BIC(high1, high2, high3, high4, high5)
