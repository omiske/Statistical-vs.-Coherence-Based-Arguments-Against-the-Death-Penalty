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

data<- capital_punishment%>%
  gather(Question, Response, c(G1:C2))

data1<- data%>%
  filter(AC1 == "Correct")%>%
  filter(AC2 == "Correct")%>%
  filter(AC3 == "Correct")%>%
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

model1 <- brm(Response ~ Condition + PreTest_Average + (1|Question) + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(0,1)", class = "b", coef = "ConditionStats"),
                        set_prior("normal(1,3)", class = "b", coef="PreTest_Average"))) 

me_plot_exp1 <- marginal_effects(model1, ordinal = TRUE, effects = "Condition")

Plot_exp1<- plot(me_plot_exp1)[[1]]+
  theme_bw(24) +
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        strip.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        strip.text = element_blank(),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        axis.line = element_line(colour = "black", size = 1.2),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(angle = 0, hjust = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
Plot_exp1 <- Plot_exp1 + labs(y = "Judgment\n(Higher = Pro Death Penalty)")

show(Plot_exp1)


model2 <- brm(Response ~ Condition*Question + PreTest_Average + (1|Subject),
              data=data1, family="cumulative", chains=4, 
              iter=2000, warmup=1000, control = list(adapt_delta = .90), sample_prior=TRUE,
              prior = c(set_prior("normal(0,1)", class = "b", coef = "ConditionStats"),
                        set_prior("normal(1,3)", class = "b", coef="PreTest_Average"))) 

me_plot_exp1_1 <- marginal_effects(model2, effects = "Question:Condition")

Plot_exp1_1<- plot(me_plot_exp1_1)[[1]]+
  geom_point(size = 6, position = position_dodge(width = .4))+
  geom_errorbar(size = 3, width = .5, position = position_dodge(width = .4))+
  scale_y_continuous(name="Judgment\n(Higher = Pro Death Penalty)")+
  theme_bw(24) +
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        strip.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        strip.text = element_blank(),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        axis.line = element_line(colour = "black", size = 1.2),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(angle = 0, hjust = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

show(Plot_exp1_1)
