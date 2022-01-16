library(car)
library(caret)
library(class)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(performance)

diabetes <- read.csv('diabetes.csv')
diabetes <- diabetes %>% 
  mutate(Outcome = factor(Outcome, levels = c(0,1), labels = c("Not Diabetes", "Diabetes")))
glimpse(diabetes)

colSums(is.na(diabetes))
x <- prop.table(table(diabetes$Outcome))
b <- barplot(x,col="lightBlue", main = "Target Class Proportion Diagram")
text(x=b, y= x, labels=round(x,2), pos = 1)



