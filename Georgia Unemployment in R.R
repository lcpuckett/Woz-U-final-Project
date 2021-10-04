## View dataset for unemployment

View(GA_Unemployment)

## Install packages

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

## Change column names

names(Unemploy)[1] <- 'GA.Unemployment.rate.by.Month'
names(Unemploy)[names(Unemploy) == 'GA.Unemployment.rate.by.Month'] <- "Month"

names(Unemploy)[1] <- 'X'
names(Unemploy)[names(Unemploy) == 'X'] <- "Georgia"

names(Unemploy)[1] <- 'X.1'
names(Unemploy)[names(Unemploy) == 'X.1'] <- "US"

## Verify data changes

head(Unemploy)

