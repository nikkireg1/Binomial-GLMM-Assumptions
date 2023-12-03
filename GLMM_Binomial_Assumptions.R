#Testing assumptions of mixed effects logistic regression
#Author: Nicole Regimbal - Just One Bird's Opinion
#Date: December 3, 2023

library(lme4)
library(ggplot2)
library(performance)
library(partR2)
library(see)

#Set working directory
getwd()
setwd("Your Path Here")
dir()

#Read in your data
df <- read.csv("glmm_assumptions.csv", header = TRUE)
summary(df)
head(df)

#Running your model

  #model <- glmer(Y ~ X + (1|Random), data = df, family = binomial(link = 'logit'))
disp_model <- glmer(Dispersed ~ Treatment + (1|Plot), data = df, family = binomial(link = 'logit'))
summary(disp_model)


#Visualizing our model
plot <- ggplot(df, aes(x = Treatment, y = Dispersed))+
  geom_point()+
  geom_smooth()+
  geom_jitter(width = 0.2, height = 0.02)+
  scale_x_continuous("Temperature (°C)", breaks = seq(26, 30, 2))+
  scale_y_continuous("Dispersal Probability")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
plot 


#Binary response variables
unique(df$Dispersed)


#Outliers
check_model(disp_model, check = 'outliers')
check_outliers(disp_model)

#Binned Residuals
check_model(disp_model, check = 'binned_residuals')
binned_residuals(disp_model)

  #Looking at an example with points outside of error bounds
  df1 <- read.csv("glmm_assumptions1.csv", header = TRUE)
  head(df1)
  head(df)
  
  disp_model1 <- glmer(Dispersed ~ Treatment + (1|Plot), data = df1, family = binomial(link = 'logit'))
  summary(disp_model1)
  
  check_model(disp_model1, check = 'binned_residuals')
  binned_residuals(disp_model1)
  
  


#Overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(disp_model)


