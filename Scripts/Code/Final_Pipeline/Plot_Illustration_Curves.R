# Can we make an illustrative bacterial growth curve?
setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts")

library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)
library(growthcurver)
library(gridBase)
library(gridExtra)

raw_growth <- tibble(
  time = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  OD = c(0.5,1,2,3,5,7.5,11,12.5,13,13,13,13,13,12.5,11.5,10,8.5,7,6,5,4)
)

# I stole this function from Justin Silverman
# http://www.statsathome.com/2017/06/07/fitting-non-linear-groth-curves-in-r/
fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}
# This too.
gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

raw_growth <- raw_growth %>%
  filter(time < 13)

growth_model_log <- SummarizeGrowth(raw_growth$time, raw_growth$OD)
growth_model_gom <- fit.gompertz(raw_growth$OD, raw_growth$time)
growth_model_gom_1 <- gompertz(raw_growth$time, 13, 3.5, 6)

temp_predict <- tibble(time = raw_growth$time, pred.wt = predict(growth_model_log$model, sigma = growth_model_log$model$sigma))

ggplot(data = raw_growth, aes(x = time, y = OD)) +
  geom_point() +
  geom_line(data = temp_predict, aes(x = time, y = pred.wt), colour = "red") +
  geom_line(data = growth_model_gom_1, aes(x = time, y = y), colour = "blue") 
