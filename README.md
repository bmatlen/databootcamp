Data Bootcamp
============

Logit Mixed Models

#Read in File
data <- read.csv("~/Desktop/CogSketch_FINAL/all_data.csv")


##Read in Packages
library(car)
library(lme4)

# Convert Subject Variable to a Factor
data$subject=as.factor(data$subject)


##Interaction Model 
gc()
full_model <- lmer(Score ~ Group * Test + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

null_model <- lmer(Score ~ Group + Test + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

anova(null_model, full_model)

summary(full_model)


## Test Model 
full_model <- lmer(Score ~ Group + Test + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

null_model <- lmer(Score ~ Group        + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

anova(null_model, full_model)

summary(full_model)


## Group model 
full_model <- glmer(Score ~ Group + Test + (1|subject) + (1|topic), family=binomial, data=data, REML=F)

null_model <- glmer(Score ~         Test + (1|subject) + (1|topic), family=binomial, data=data, REML=F)

anova(null_model, full_model)

summary(full_model)


## Model with Multiple Random Variables
full_model <- lmer(Score ~ Group * Test + (1|subject) + (1|topic) + (1|spatial.reasoning.score) + (1|overall_pretest), family=binomial, data=data, REML=F)

null_model <- lmer(Score ~ Group + Test + (1|subject) + (1|topic) + (1|spatial.reasoning.score) + (1|overall_pretest), family=binomial, data=data, REML=F)

anova(null_model, full_model)

summary(full_model)
