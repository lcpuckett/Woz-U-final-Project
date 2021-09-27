Data <- read.csv("C:/Users/latri/Desktop/SCI SCHOOLING/FINAL PROJECT/First Choice Data.csv", stringsAsFactors=TRUE)
data1 <- Data[ -c(5:8,10, 12:20,29:49,54) ]
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
#after deleting all the columns we don't need, there are no missing values!
#will keep using data1 from now on. 

str(data1)
#everything is a factor with multiple levels, should be able to do some analysis and significant testing, or can recode data into numbers.
data2= mutate(data1,Prior_Conviction_Episodes_Felony=recode(Prior_Conviction_Episodes_Felony,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Misd=recode(Prior_Conviction_Episodes_Misd,"0"=0, "1"=1, "2"=2, "3"= 3, "4 or more"= 4),
              Prior_Conviction_Episodes_Viol=recode(Prior_Conviction_Episodes_Viol, "true"=0, "false"=1), 
              Prior_Conviction_Episodes_Prop=recode(Prior_Conviction_Episodes_Prop,"0"=0, "1"=1, "2"=2, "3 or more"= 3),
              Prior_Conviction_Episodes_Drug=recode(Prior_Conviction_Episodes_Drug,"0"=0, "1"=1, "2 or more"= 2),
              Prior_Conviction_Episodes_PPViolationCharges=recode(Prior_Conviction_Episodes_PPViolationCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_DomesticViolenceCharges=recode(Prior_Conviction_Episodes_DomesticViolenceCharges,"true"=0, "false"=1),
              Prior_Conviction_Episodes_GunCharges=recode(Prior_Conviction_Episodes_GunCharges,"true"=0, "false"=1)
              )


#wondering if I should do the arrest years, education and age groups as well
#want to ask joseph if we need to recode T/F data for analysis, or if recoding this data will affect our analysis for variance, or the counts of things.
#can we analyze the T/F and the numbers data together. 
#can use this data to compare with Recidivism within 3 years to ask "does having more prior convictions significantly affect the chances of being arrested again within three years?"

#boxplot
#ggplot(gapminder, aes(x = factor(year), y = pop)) + geom_boxplot()

#line chart with title
#ggplot(gm_Angola) + geom_line(aes(x = year, y = gdpPercap)) + ylab("Per Capita GDP") +  ggtitle("GDP in Angola")

#line chart with color-coded data and data-sized points on chart
#ggplot(gm_AfricaClean, aes(x = year, y = lifeExp, color = country)) + geom_line() + geom_point(aes(size = gdpPercap)) + ylab("Life Expectancy") + ggtitle("Life Expectancy and GDP in Four Countries")

#scatterplot
#ggplot(data1, aes(x = Age_at_Release, y = Prison_Offense, color =Recidivism_Within_3years )) + geom_point()

#ggplot(convictions, aes(x = convictions)) + geom_histogram()
