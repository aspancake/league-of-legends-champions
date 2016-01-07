library(data.table)
library(ggplot2)
library(plyr)

# Import CSV into data table
setwd('...')
round_1 <- read.csv('master_2.csv')
round_1 <- as.data.table(round_1)

# Cleaning
round_1 <- subset(round_1, select = -c(X,Date))
names(round_1)[names(round_1)=='votes'] <- 'Votes'
round_1$Gender <- as.factor(round_1$Gender)

# ~ Exploratory Work ~ #

# How Important is Gender? (1=Male,2=Female,3=Other)
ggplot(round_1,aes(x=Votes,fill=Gender)) + geom_histogram()
summ_gender <- ddply(round_1,c("Gender"),summarise,mean=mean(Votes))

# How does play rate effect popularity?
ggplot(round_1,aes(x=Popularity,y=Votes)) + geom_smooth()

# Difference between roles?
ggplot(round_1,aes(x=Votes,fill=Primary)) + geom_histogram()
summ_roles <- ddply(round_1,c("Primary"),summarise,avg_votes=mean(Votes),avg_play=mean(Popularity))

# For a starting point, let us fit an OLS Regression
ols_full <- lm(Votes ~ . - Champion, data = round_1)
ols_null <- lm(Votes ~ 1, data = round_1)
step_vars <- step(ols_null, scope=list(upper=ols_full), direction="both")
summary(step_vars)

