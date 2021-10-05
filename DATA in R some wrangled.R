Data <- read.csv("C:/Users/latri/Desktop/SCI SCHOOLING/FINAL PROJECT/First Choice Data.csv", na.strings = c(""), stringsAsFactors=TRUE)
#removed all unneeded columns
data1 <- Data[ -c(5:8,10, 12:20,29:49,54) ]
#made a table with just prior convictions if needed, used recoded data for any needed analysis
priors= dataR[c(7:14)]

#all the packages!
install.packages("ggplot2")
library(ggplot2)

install.packages("dbplyr")
library(dbplyr)

install.packages("tidyr")
library(tidyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("lattice")
library(lattice)

install.packages("gmodels")
library("gmodels")

library("rcompanion")
library("car")
library("fastR2")

#removing empty cells, looked like there were only 3277
data2= na.omit(data1)

str(data2)
#everything is a factor with multiple levels, should be able to do some analysis and significant testing, or can recode data into numbers.
<<<<<<< Updated upstream
#les is doing all other needed recodes. 
dataR= mutate(data2,Prior_Conviction_Episodes_Felony=recode(Prior_Conviction_Episodes_Felony,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Misd=recode(Prior_Conviction_Episodes_Misd,"0"=0, "1"=1, "2"=2, "3"= 3, "4 or more"= 4),
              Prior_Conviction_Episodes_Viol=recode(Prior_Conviction_Episodes_Viol, "true"=0, "false"=1), 
              Prior_Conviction_Episodes_Prop=recode(Prior_Conviction_Episodes_Prop,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Drug=recode(Prior_Conviction_Episodes_Drug,"0"=0, "1"=1, "2 or more"= 2),
              Prior_Conviction_Episodes_PPViolationCharges=recode(Prior_Conviction_Episodes_PPViolationCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_DomesticViolenceCharges=recode(Prior_Conviction_Episodes_DomesticViolenceCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_GunCharges=recode(Prior_Conviction_Episodes_GunCharges,"true"=0, "false"=1),
              Education_Level=recode(Education_Level,"At least some college"=3, "High School Diploma"=2, "Less than HS diploma"=1)
              )


#POST-WRANGLE INFO (checking that forms and types of data work for our analysis)
#to make histograms have to use recoded data.
ggplot(dataR, aes(x = Prior_Conviction_Episodes_GunCharges)) + geom_histogram(binwidth = 0.5)
ggplot(dataR, aes(x = Prior_Conviction_Episodes_Misd)) + geom_histogram(binwidth = 0.5)

#can make bar charts without recode!
ggplot(data2, aes(Age_at_Release))+ geom_bar()
ggplot(data2, aes(Prior_Conviction_Episodes_Misd))+ geom_bar()
barchart(data2$Age_at_Release)

#cant check normality with categorical data
ggplot(dataR, aes(sample = Education_Level)) + geom_qq()


#check correlation between some variables?


=======
#recoding prior convictions, education level and prison offense. les is doing all other needed recodes. 
dataR= mutate(data2, Prior_Conviction_Episodes_Felony=dplyr::recode(Prior_Conviction_Episodes_Felony,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Misd=dplyr::recode(Prior_Conviction_Episodes_Misd,"0"=0, "1"=1, "2"=2, "3"= 3, "4 or more"= 4),
              Prior_Conviction_Episodes_Viol=dplyr::recode(Prior_Conviction_Episodes_Viol, "true"=0, "false"=1), 
              Prior_Conviction_Episodes_Prop=dplyr::recode(Prior_Conviction_Episodes_Prop,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Drug=dplyr::recode(Prior_Conviction_Episodes_Drug,"0"=0, "1"=1, "2 or more"= 2),
              Prior_Conviction_Episodes_PPViolationCharges=dplyr::recode(Prior_Conviction_Episodes_PPViolationCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_DomesticViolenceCharges=dplyr::recode(Prior_Conviction_Episodes_DomesticViolenceCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_GunCharges=dplyr::recode(Prior_Conviction_Episodes_GunCharges,"true"=0, "false"=1),
              Education_Level=dplyr::recode(Education_Level,"At least some college"=3, "High School Diploma"=2, "Less than HS diploma"=1),
              Prison_Offense=dplyr::recode(Prison_Offense,"Drug"=0, "Other"=1, "Property"=2, "Violent/Non-Sex"= 3, "Violent/Sex"=4)
)


#POST-WRANGLE INFO (checking that wrangling of data works for our analysis)
#to make histograms have to use recoded data.
ggplot(dataR, aes(x = Prior_Conviction_Episodes_GunCharges)) + geom_histogram(binwidth = 0.5)
ggplot(dataR, aes(x = Prior_Conviction_Episodes_Misd)) + geom_histogram(binwidth = 0.5)

#can make bar charts without recode!
ggplot(data2, aes(Age_at_Release))+ geom_bar()
ggplot(data2, aes(Prior_Conviction_Episodes_Misd))+ geom_bar()
barchart(data2$Age_at_Release)

#cant check normality with categorical data
#ggplot(dataR, aes(sample = Education_Level)) + geom_qq()


#check correlation between some variables?


>>>>>>> Stashed changes
#DSO105 data time!
#we could do some goodness of fit Chi squares with random guesses about probabilities of going back? perhaps need more background info about returning to prison
#independent chi square to compare each! dont need to recode!use this to compare all the conviciton types to each other
##CrossTable(data2$Education_Level, data2$Prior_Conviction_Episodes_Viol, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
##CrossTable(data2$Education_Level, data2$Prison_Offense, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#for anything with only two fators
##CrossTable(data2$Gender, data2$Recidivism_Within_3years, chisq = TRUE,mcnemar = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")


#homogeneity of variance tests, one way anova
##plotNormalHistogram(dataR$Education_Level)
##fligner.test(Education_Level ~ Prior_Conviction_Episodes_Misd, data=dataR)
#perform post hocs find where significant
#repeated measure anovas?


#want to ask joseph if we need to recode T/F data for analysis, or if recoding this data will affect our analysis for variance, or the counts of things.
#can we analyze the T/F and the numbers data together?
<<<<<<< Updated upstream
#can use this data to compare with Recidivism within 3 years to ask "does having more prior convictions significantly affect the chances of being arrested again within three years?"
=======
#can use this data to compare with Recidivism within 3 years to ask "does having more prior convictions significantly affect the chances of being arrested again within three years?"
>>>>>>> Stashed changes
