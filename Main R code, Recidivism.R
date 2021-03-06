Data <- read.csv("C:/Users/latri/Desktop/SCI SCHOOLING/FINAL PROJECT/First Choice Data.csv", na.strings = c(""), stringsAsFactors=TRUE)
#removed all unneeded columns
data1 <- Data[ -c(5:8,10, 12:16,18:20,29:49,54) ]
#made a table with just prior convictions if needed, used recoded data for any needed analysis
priors= dataR[c(8:14)]

#second data set to look at unemployment by month/ year and how it effects crime, need to do some analysis still!
library(readxl)
nojob= read_excel("C:/Users/latri/Desktop/SCI SCHOOLING/FINAL PROJECT/Woz-U-final-Project/Unemployment and Crime.xlsx")

str(nojob)


#all the packages!
library(ggplot2)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(lattice)
library("gmodels")
library("rcompanion")
library("car")
library("fastR2")

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
install.packages("corrplot")
library("corrplot")


#removing empty cells, looked like there were only 3277
data2= na.omit(data1)

str(data2)
#everything is a factor with multiple levels, should be able to do some analysis and significant testing, or can recode data into numbers.
#recoding prior convictions, education level and prison offense. les is doing all other needed recodes. 
dataR= mutate(data2, Prior_Arrest_Episodes_Drug=dplyr::recode(Prior_Arrest_Episodes_Drug,"0"=0, "1"=1, "2"=2, "3"= 3, "4"=4, "5 or more"=5),
              Prior_Conviction_Episodes_Felony=dplyr::recode(Prior_Conviction_Episodes_Felony,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Misd=dplyr::recode(Prior_Conviction_Episodes_Misd,"0"=0, "1"=1, "2"=2, "3"= 3, "4 or more"= 4),
              Prior_Conviction_Episodes_Viol=dplyr::recode(Prior_Conviction_Episodes_Viol, "true"=0, "false"=1), 
              Prior_Conviction_Episodes_Prop=dplyr::recode(Prior_Conviction_Episodes_Prop,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Drug=dplyr::recode(Prior_Conviction_Episodes_Drug,"0"=0, "1"=1, "2 or more"= 2),
              Prior_Conviction_Episodes_PPViolationCharges=dplyr::recode(Prior_Conviction_Episodes_PPViolationCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_DomesticViolenceCharges=dplyr::recode(Prior_Conviction_Episodes_DomesticViolenceCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_GunCharges=dplyr::recode(Prior_Conviction_Episodes_GunCharges,"true"=0, "false"=1),
              Education_Level=dplyr::recode(Education_Level,"At least some college"=3, "High School Diploma"=2, "Less than HS diploma"=1),
              Prison_Offense=dplyr::recode(Prison_Offense,"Drug"=0, "Other"=1, "Property"=2, "Violent/Non-Sex"= 3, "Violent/Sex"=4),
              Recidivism_Within_3years=dplyr::recode(Recidivism_Within_3years,"true"=0, "false"=1),
              Recidivism_Arrest_Year1=dplyr::recode( Recidivism_Arrest_Year1,"true"=0, "false"=1),
              Recidivism_Arrest_Year2=dplyr::recode( Recidivism_Arrest_Year2,"true"=0, "false"=1),
              Recidivism_Arrest_Year3=dplyr::recode( Recidivism_Arrest_Year3,"true"=0, "false"=1)
)
install.packages("writexl")
library(writexl)
write_xlsx(dataR, "C:/Users/latri/Desktop/SCI SCHOOLING/FINAL PROJECT/DataR.xlsx")
#POST-WRANGLE INFO (checking that forms and types of data work for our analysis)
#to make histograms have to use recoded data.
ggplot(dataR, aes(x = Prior_Arrest_Episodes_Drug)) + geom_histogram(binwidth = 0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_Felony))+geom_histogram(binwidth=0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_Misd))+geom_histogram(binwidth=0.5)

ggplot(dataR, aes(x = Prior_Conviction_Episodes_Viol)) + geom_histogram(binwidth = 0.5)

ggplot(dataR, aes(x = Prior_Conviction_Episodes_Prop)) + geom_histogram(binwidth = 0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_Drug))+geom_histogram(binwidth=0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_PPViolationCharges))+geom_histogram(binwidth=0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_DomesticViolenceCharges))+geom_histogram(binwidth=0.5)

ggplot(dataR, aes(x= Prior_Conviction_Episodes_GunCharges))+geom_histogram(binwidth=0.5)

ggplot(Data, aes(x= DrugTests_Other_Positive))+geom_histogram(binwidth=.23)
#when looking at drug use (decimal number of drugs in system) it seems the majority of people had very little chemicals in their system. 
#From visuals alone I would say drug usage is not a significant factor to doing crimes

r

#can make bar charts without recode!
ggplot(data2, aes(Prison_Offense))+ geom_bar(fill="navyblue")
#lots of property offenses
ggplot(data2, aes(Age_at_Release))+ geom_bar(fill="navyblue")
#majority of people released are between 23- 32, then numbers drop! matches anecdotal info 25 is golden year for rearrest
ggplot(data2, aes(Education_Level))+ geom_bar(fill="navyblue")
#very few college educated getting arrested, mostly high school grads and then some with no diploma
ggplot(data2, aes(Prior_Arrest_Episodes_Drug))+geom_bar(fill="navyblue")
#once again looking at connection to drugs, the majority have 0 prior arrests, the least having four or "5 or more"
ggplot(data2, aes(Prior_Conviction_Episodes_Drug))+geom_bar(fill="navyblue")
#this again shows that majority of people dont have many drug convictions, like there are with property
ggplot(Data, aes(Prior_Arrest_Episodes_Violent))+geom_bar(fill="navyblue")

ggplot(data2, aes(Recidivism_Arrest_Year1))+ geom_bar(fill="navyblue")
ggplot(data2, aes(Recidivism_Arrest_Year2))+ geom_bar(fill="navyblue")
ggplot(data2, aes(Recidivism_Arrest_Year3))+ geom_bar(fill="navyblue")
ggplot(data2, aes(Recidivism_Within_3years))+ geom_bar(fill="navyblue")


#checking normality ( on noncategorical data)
ggplot(nojob, aes(sample = TotalCrime)) + geom_qq()
#pretty close to normal
ggplot(nojob, aes(sample = Unemploy_Georgia)) + geom_qq()
#normal

ggplot(nojob, aes(x = TotalCrime, y = Unemploy_Georgia))+geom_point()+geom_smooth(method=lm, color = "aquamarine")
#positive correlation between crime and unemployment? large margin of error


#check correlation between some variables?
cor.test(nojob$TotalCrime, nojob$Unemploy_Georgia, method="pearson", use = "complete.obs")
#correlation of 0.38, moderately positive, significant
cor.test(nojob$Burglary, nojob$Unemploy_Georgia, method="pearson", use = "complete.obs")
#very significant high correlation with unemployment
#only burglary was significantly correlated


nojob1=nojob[, c(3:11)]
chart.Correlation(nojob1, histogram=FALSE, method="pearson")
#playing around with the correlation of new data set. Assault and Rape very correlated. Total crime and burglary/ larceny highly correlated.
#unemployment and burglary are moderately correlated!

#looking at certain convictions with education level shows there is moderate correlation, nothing highly correlated
dataR1= dataR[, c(5,6,8,9,10,11,12,16:19)]
#looking at education, prison offense and ARRESTS for drugs
dataR2= dataR[, c(5,6,7,16:19)]
cor.test(dataR1$Recidivism_Within_3years, dataR1$Recidivism_Arrest_Year1, method="pearson", use = "complete.obs")
cor.test(dataR1$Recidivism_Within_3years, dataR1$Recidivism_Arrest_Year2, method="pearson", use = "complete.obs")
cor.test(dataR1$Recidivism_Within_3years, dataR1$Recidivism_Arrest_Year3, method="pearson", use = "complete.obs")
#Correlation between arrests within three years is highest within 1 year

#making nice looking plots
corr_matrix <- cor(dataR2)
corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.05, insig="blank")
#people with prior drug offenses were more LESS to have a certain type of prison offense

corr_mat= cor(nojob1)
corrplot(corr_mat, type="lower", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")
# vehicle theft and larceny are positively correlated to unemployment. while (thankfully) rape is slightly negatively correlated. 
#getting error, fix!



#DSO105 data time!
#we could do some goodness of fit Chi squares with random guesses about probabilities of going back? perhaps need more background info about returning to prison
CrossTable(data2$Prison_Offense, data2$Recidivism_Within_3years, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#very significant all around


#prior convictions/ arrests
CrossTable(data2$Age_at_Release, data2$Prior_Conviction_Episodes_Felony, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(data2$Prison_Offense, data2$Recidivism_Within_3years, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
# checked year one and within 3 for each conviction, copied down the p values to compare against each other, purely for finding which depth to look into. 

#age
CrossTable(data2$Age_at_Release, data2$Recidivism_Within_3years, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#youngings are not returning to jail way less than expected but also way more than expected? 
#only the rates of return for 33-37 are within non significant expectations. 

CrossTable(data2$Age_at_Release, data2$Prison_Offense, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#young people are doing more property and violent/ non-sex crime than expected. middle aged do more drug and other crimes than expected. while no longer doing property and violent as expected.
#older folks get more into drugs and back into property crimes, while also committing violent sex crimes, but not other.

CrossTable(data2$Age_at_Release, data2$Recidivism_Arrest_Year1, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

#education level
CrossTable(data2$Education_Level, data2$Recidivism_Within_3years, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#same here, expectations are way off!
CrossTable(data2$Education_Level, data2$Prison_Offense, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#more than college educated than expected were committing violent crimes. committing less property crime than expected. high school only were
#doing less violent/ non sex crime than expected, while no diploma people were doing less violent/ sex crime than expected. 
#So highly educated people were more violent than expected! is it b/c loan debt?? haha
CrossTable(data2$Education_Level, data2$Recidivism_Arrest_Year1, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

#gender
CrossTable(data2$Gender, data2$Recidivism_Within_3years, chisq = TRUE,mcnemar = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#race
CrossTable(data2$Race, data2$Recidivism_Within_3years, chisq = TRUE,mcnemar = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")


#unemployment
CrossTable(nojob$Year, nojob$Unemploy_Georgia, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

#drug use is combined with others but could do a tableau of test results




#remake in tableau
plotNormalHistogram(dataR$Education_Level)
#flattened
plotNormalHistogram(dataR$Prison_Offense)
#flattened
plotNormalHistogram(dataR$Prior_Conviction_Episodes_Drug)
#flattened
plotNormalHistogram(dataR$Prior_Conviction_Episodes_Felony)
#flattened

plotNormalHistogram(nojob$`Total Crime Per Month`)
#normally distributed
plotNormalHistogram(nojob$`Unemployment in Georgia`)
#normally distributed