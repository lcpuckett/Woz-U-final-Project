## View dataset for unemployment

View(GA_Unemployment)

## Install packages

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

## Change column names

names(GA_Unemployment)[1] <- 'GA.Unemployment.rate.by.Month'
names(GA_Unemployment)[names(GA_Unemployment) == 'GA.Unemployment.rate.by.Month'] <- "Month"

names(GA_Unemployment)[1] <- 'X'
names(GA_Unemployment)[names(GA_Unemployment) == 'X'] <- "Georgia"

names(Unemploy)[1] <- 'X.1'
names(Unemploy)[names(Unemploy) == 'X.1'] <- "US"

## Verify data changes

head(GA_Unemployment)

