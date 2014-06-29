databootcamp
============

Gentner lab data day - Logit Mixed Models

##Read in File
data <- read.csv("~/Desktop/CogSketch_FINAL/all_data.csv")


# Make Subject a factor
data$Subject=as.factor(data$Subject)


##LOGISTIC MODELING, interaction model 
gc()
full_model <- lmer(Score ~ Group * Test + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

null_model <- lmer(Score ~ Group + Test + (1|subject) + (1|topic), family=binomial, data=data,REML=F)

anova(null_model, full_model)

summary(full_model)


## Test Model 
full_model <- lmer(Score ~ Group + Test + (1|subject), family=binomial, data=data,REML=F)

null_model <- lmer(Score ~ Group        + (1|subject), family=binomial, data=data,REML=F)

anova(null_model, full_model)

summary(full_model)


## Group model 
full_model <- glmer(Score ~ Group + Test + (1|subject), family=binomial, data=data, REML=F)

null_model <- glmer(Score ~         Test + (1|subject), family=binomial, data=data, REML=F)

anova(null_model, full_model)

summary(full_model)


## Model with other random variables
full_model <- lmer(Score ~ Group * Test + (1|subject) + (1|spatial.reasoning.score) + (1|overall_pretest), family=binomial, data=data, REML=F)

null_model <- lmer(Score ~ Group + Test + (1|subject) + (1|spatial.reasoning.score) + (1|overall_pretest), family=binomial, data=data, REML=F)

anova(null_model, full_model)

summary(full_model)
