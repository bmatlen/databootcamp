Data Bootcamp
============

Logit Mixed Effects Models in R

    ##Read in File
    data <- read.csv("~/Desktop/CogSketch_FINAL/all_data.csv")


    ##Read in Packages
    library(car)
    library(lme4)


    ##Convert Subject Variable to a Factor
    data$subject=as.factor(data$subject)


    ##Interaction Model 
    full_model <- glmer(Score ~ Group * Test + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    null_model <- glmer(Score ~ Group + Test + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    anova(null_model, full_model)
    summary(full_model)


    ##Test Model 
    full_model <- glmer(Score ~ Group + Test + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    null_model <- glmer(Score ~ Group        + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    anova(null_model, full_model)
    summary(full_model)
    

    ##Group model 
    full_model <- glmer(Score ~ Group + Test + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    null_model <- glmer(Score ~         Test + (1 + Test|subject) + (1 + Test|topic), family=binomial, data=data)
    anova(null_model, full_model)
    summary(full_model)



