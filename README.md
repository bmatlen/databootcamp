Data Bootcamp
============

Logit Mixed Effects Models in R

    ##Read in File
    data <- read.csv("all_data.csv")


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


Mediation Analysis in R

    # MEDIATION Analysis


    binladen_data <- read.csv('binladen.csv')


    library(mediation)


    # straight mediation: binladen predicting mcivil mediated by stereo
    model.m <- lm(stereo ~ binladen + sex + age + ideo, data=binladen_data)
    model.y <- lm(mcivil ~ stereo + binladen + stereo + sex + age + ideo, data=binladen_data)

    model.total <- mediate(model.m, model.y, treat="binladen", mediator="stereo", boot=TRUE, sims=1000)
    summary(model.total)


    # total effect is the direct effect of X on Y completely ignoring the mediator
    model.ignore_mediator <- lm(mcivil ~ binladen + sex + age + ideo, data=binladen_data)
    summary(model.ignore_mediator)


    # moderated-mediation: biladen predicting mcivil mediated by stereo, moderated by age
    model.m <- lm(stereo ~ binladen*age + sex +ideo, data=binladen_data)
    model.y <- lm(mcivil ~ stereo*binladen*age + stereo + sex + ideo, data=binladen_data)


    # look at mediation conditional on age==3
    model.total <- mediate(model.m, model.y, treat="binladen", mediator="stereo", boot=TRUE, sims=1000, covariates = list(age = 3))
    summary(model.total)


    # look at mediation conditional on age==7
    model.total <- mediate(model.m, model.y, treat="binladen", mediator="stereo", boot=TRUE, sims=1000, covariates = list(age = 7))
    summary(model.total)

