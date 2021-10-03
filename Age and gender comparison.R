## Loading packages

data1 <- NIJ[ -c(5:8,10, 12:20,29:49,54) ]
convictions= Data[c(21:28)]

install.packages("ggplot2")
library(ggplot2)

install.packages("dbplyr")
library(dbplyr)

install.packages("tidyr")
library(tidyr)

install.packages("tidyverse")
library(tidyverse)

data2= na.omit(data1)

str(data1)




## Initial Comparison of Gender and Age
## 

attach(data1)
data2 <- table(Gender, Age_at_Release)
data2

attach(data1)
data2 <- table(Age_at_Release, Recidivism_Within_3years)
data2

## Comparison of Recidivism and Education

attach(data1)
data2 <- table(Recidivism_Within_3years, Education_Level)
data2

## Comparison of Gender and Recidivism_Within_3years

attach(data1)
data2 <- table(Recidivism_Within_3years, Gender)
data2

## Recidivism for females is 45%, males is 59%

## First plot attempt of Age_at_Release and Recidivism_Within_3Years

df <- data.frame(Age_at_Release = (data1$Age_at_Release), 
                 Recidivism_Within_3years = round(data1))

library(ggplot2)
ggplot(df, aes(x = Recidivism_Within_3years)) +
  geom_histogram() + facet_grid(.~Age_at_Release)

##  Plot needs some tweaking of the bin size. Latrice what do you think we can do to better show this plot?







