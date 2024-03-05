# Assume data is in a dataframe called 'data' with columns 'TRT' for treatment and 'RESP' for response
setwd("C:/Users/Leon/Desktop/Ass2")
# Load necessary library
library(multcomp)
library(vcdExtra)
library(tidyverse)
library(DescTools)
data=read.csv("DTA41.csv",header = T)
colnames(data)[1]="Obs"
data$TRT=factor(data$TRT)
# Fit logistic regression model
model <- glm(RESP ~ TRT, data = data, family = binomial)

# Summary of the model to check coefficients and overall fit
summary(model)

# Define contrasts for pairwise comparisons: each dose vs. placebo (TRT levels 1, 2, 3 vs. 0)
contrasts <- rbind("1 vs 0" = c(-1, 1, 0, 0),
                   "2 vs 0" = c(-1, 0, 1, 0),
                   "3 vs 0" = c(-1, 0, 0, 1))

# Apply Bonferroni correction and perform pairwise comparisons
summary(glht(model, linfct = mcp(TRT=contrasts), alternative = "two.sided"), test = adjusted("bonferroni"))

summary(glht(model, linfct = mcp(TRT = "Dunnett")))


dose_data=matrix(c(45,11,33,21,30,24,25,28),byrow = T,nrow = 4)
dimnames(dose_data) <- list("trt" = c("0", "1", "2", "3"),
                      "resp" = c("Yes", "No"))
dose_data
CochranArmitageTest(dose_data)
