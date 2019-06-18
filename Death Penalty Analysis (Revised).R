# Authors: Olivia Miske & Zachary Horne
# Title: Statistical vs Analogical arguments against the death penalty
# Date: July 7, 2018
# Exploratory or Confirmatory: Exploratory
# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


require(brms)
require(rstan)
require(Rcpp)
require(ggplot2)
require(scales)
require(loo)
require(gridExtra)
require(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


data1<- data%>%
  filter(Check == 3)%>%
  filter(Attention == "Yes")


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

# All models will be run with stronger and weaker priors
# testing the prediction that judgments for post-measure will be in accordance with pre-measure responses

model0 <- brm(Judgment ~ Pre_Test + (1|Question) + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(1,3)", class = "b", coef="Pre_Test"))) 


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


# testing the prediction that condition will have an effect on judgments
# more specifically: will the statistical or moral intervention change attitudes towards the death penalty

model1 <- brm(Judgment ~ Condition + Pre_Test + (1|Question) + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(0,1)", class = "b", coef = "ConditionStatistical"),
                        set_prior("normal(1,3)", class = "b", coef="Pre_Test"))) 


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


# testing the prediction that the condition effect may be stronger or weaker depending on pre-measure responses

model2 <- brm(Judgment ~ Condition*Pre_Test + (1|Question) + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(0,1)", class = "b"),
                        set_prior("normal(0,1)", class = "b", coef = "ConditionStatistical"),
                        set_prior("normal(1,3)", class = "b", coef="Pre_Test"))) 


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


# testing the prediction that condition may effect some post-measure items more than others

model3 <- brm(Judgment ~ Condition*Question + Pre_Test + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(0,10)", class = "b"),
                        set_prior("normal(0,1)", class = "b", coef = "Condition:Question"),
                        set_prior("normal(0,1)", class = "b", coef = "ConditionStatistical"),
                        set_prior("normal(1,3)", class = "b", coef="Pre_Test"))) 

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

# Compute bayes factors for all models above using bayes_factor and hypothesis functions in brms

# computing out of sample prediction of various models

l0 <- loo(model0)
l1 <- loo(model1)
l2 <- loo(model2)
l3 <- loo(model3)

loo_list <- list(l0,l1,l2,l3)

loo_model_weights(loo_list,method = "stacking")
loo_summary <- compare(l0,l1,l2,l3)


# Kfold best models according to looic

k0 <- kfold(model0, k=10)
k1 <- kfold(model1, k=10)
k2 <- kfold(model2, k=10)
k3 <- kfold(model3, k=10)

# then compare best models according to kfoldic