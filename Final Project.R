### Installing necessary packages and filtering data
install.packages("dplyr")
library(dplyr)
unique(Low_Birth_Weight_by_County$Value)
New_Mortality <- Infant_Mortality_by_County %>% ## filtering data so that all values are numeric
  filter(Value != "Suppressed") %>%
  filter(Value != "No Events")
New_Mortality$Deaths <- as.numeric(New_Mortality$Value)
hist(New_Mortality$Deaths) ### Creating a histogram to ensure that all the data is numeric

unique(Low_Birth_Weight_by_County$Value)## filtering data so that all values are numeric
New_Weight <- Low_Birth_Weight_by_County %>%
  filter(Value != "Suppressed") %>%
  filter(Value != "No Events")
New_Weight$Weight <- as.numeric(New_Weight$Value)
New_Weight$Weight <- as.numeric(gsub("%", "", New_Weight$Value))
hist(New_Weight$Weight)### Creating a histogram to ensure that all the data is numeric

unique(Prematurity_by_County$Value)## filtering data so that all values are numeric
New_Prematurity <- Prematurity_by_County %>%
  filter(Value != "Suppressed") %>%
  filter(Value != "No Events")
New_Prematurity$Premie <-as.numeric(gsub("%", "", New_Prematurity$Value))
hist(New_Prematurity$Premie)### Creating a histogram to ensure that all the data is numeric

unique(California_Arsenic$Value)## filtering data so that all values are numeric
New_Arsenic<- California_Arsenic %>%
  filter(Value != "Suppressed") %>%
  filter(Value != "No Events")
New_Arsenic$Served <-as.numeric(gsub("%", "", New_Arsenic$Value))
hist(New_Arsenic$Served)### Creating a histogram to ensure that all the data is numeric

New_Nitrates<- Californi %>%## filtering data so that all values are numeric
  filter(Value != "Suppressed") %>%
  filter(Value != "No Events")
New_Nitrates$Served <-as.numeric(gsub("%", "", New_Nitrates$Value))
hist(New_Nitrates$Served)### Creating a histogram to ensure that all the data is numeric


## Arsenic Join with birth outcomes data
Arsenic_Birth_Data <- inner_join(New_Arsenic, 
                    New_Prematurity,
                    by=c("CountyFIPS", "Year"="End Year")) 
plot(Arsenic_Birth_Data$Served, Arsenic_Birth_Data$Premie)
Arsenic_Birth_Data1 <- Arsenic_Birth_Data %>%
  filter(Concentration != "Concentration: (not-detected)") %>% ### filtering concentrations that I do not want to measure. I only want the max amount of arsenic (>10) served to the population.
  filter(Concentration != "Concentration: (0-<=10)") 
plot(Arsenic_Birth_Data$Served, Arsenic_Birth_Data$Premie)

Arsenic_Birth_Data1 <- inner_join(Arsenic_Birth_Data1,
                                  New_Mortality,
                                  by=c("CountyFIPS", "Year")) 
Arsenic_Birth_Data1 = Arsenic_Birth_Data1 %>% ## removing columns that are either repeated or not needed for analysis
  select(-"Data Comment.x", -"Data Comment.y", -"Data Comment",
         -"StateFIPS", -"StateFIPS.y", -"State", -"County", -"State.y", -"County.y")
Arsenic_Birth_Data1 = Arsenic_Birth_Data1[-12]
Arsenic_Birth_Data1 = Arsenic_Birth_Data1[-14]
Arsenic_Birth_Data1 <- inner_join(Nitrate_Birth_Data1, ## joining birth weight data with arsenic, prematurity, and infant mortality data
                                  New_Weight,
                                  by=c("CountyFIPS", "Year")) 
Arsenic_Birth_Data1 = Arsenic_Birth_Data1 %>% ## removing columns that are either repeated or not needed for analysis
  select(-"StateFIPS", -"State", -"County", -"Data Comment")
Arsenic_Birth_Data1 = Arsenic_Birth_Data1 %>%
  select(-"Value.y.y", -"Weight.x")
Arsenic_Birth_Data1 = Arsenic_Birth_Data1[-16]


### Plot of Arsenic Data vs. birth outcomes data

plot(Arsenic_Birth_Data1$Served, Arsenic_Birth_Data1$Weight.y,
     xlab="percentage of people who were served the maximum amount of arsenic recorded", 
     ylab = "Percentage of infants with a birthweight of less than 1500 grams", 
     main="The influence of arsenic in drinking water on birth weight", 
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)
plot(Arsenic_Birth_Data1$Served, Arsenic_Birth_Data1$Premie,
     xlab="percentage of people who were served the maximum amount of arsenic recorded", 
     ylab = "Percent of infants born prematurely", 
     main="The influence of arsenic in drinking water on premature birth rates", 
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)
plot(Arsenic_Birth_Data1$Served, Arsenic_Birth_Data1$Deaths,
     xlab="percentage of people who were served the maximum amount of arsenic recorded", 
     ylab = "Morality rate per 1000 births", 
     main="The influence of arsenic in drinking water on infant death rates",
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)


### Fresno County Arsenic Analysis
Fresno.A <- Arsenic_Birth_Data1 %>% ### Observing if birth defects in counties with high amounts of agriculture will be more influenced by nitrates levels 
  filter(County.x == "Fresno")
plot(Fresno.A$Served, Fresno.A$Premie)
plot(Fresno.A$Served, Fresno.A$Deaths)
plot(Fresno.A$Served, Fresno.A$Weight)

### Tulare County Arsenic analysis
Tulare.A <- Arsenic_Birth_Data1 %>%
  filter(County.x == "Tulare")
plot(Tulare.A$Served, Tulare.A$Premie)
plot(Tulare.A$Served, Tulare.A$Deaths)
plot(Tulare.A$Served, Tulare.A$Weight)


## Nitrate join with Birth Outcomes Data
Nitrate_Birth_Data <- inner_join(New_Nitrates, 
                                 New_Prematurity,
                                 by=c("CountyFIPS", "Year"="End Year")) 

Nitrate_Birth_Data1 <- Nitrate_Birth_Data %>%
  filter(Concentration != "Concentration: (not-detected)") %>% ### filtering concentrations that I do not want to measure. I only want the max amount of arsenic (>10) served to the population.
  filter(Concentration != "Concentration: (0-<=10)") 

Nitrate_Birth_Data1 <- inner_join(Nitrate_Birth_Data1,
                                  New_Mortality,
                                  by=c("CountyFIPS", "Year")) 
Nitrate_Birth_Data1 = Nitrate_Birth_Data1 %>% ## removing columns that are either repetitive or not needed for analysis
  select(-"Data Comment.x", -"Data Comment.y", -"Data Comment",
         -"StateFIPS", -"StateFIPS.y", -"State", -"County", -"State.y", -"County.y")
Nitrate_Birth_Data1 = Nitrate_Birth_Data1[-12]
Nitrate_Birth_Data1 = Nitrate_Birth_Data1[-14]
Nitrate_Birth_Data1 <- inner_join(Nitrate_Birth_Data1, ##joining birth weight data with nitrate, prematurity, and infant mortality data
                                  New_Weight,
                                  by=c("CountyFIPS", "Year")) 
Nitrate_Birth_Data1 = Nitrate_Birth_Data1 %>% ## removing columns that are either repetitive not needed for analysis
  select(-"StateFIPS", -"State", -"County", -"Data Comment")
Nitrate_Birth_Data1 = Nitrate_Birth_Data1[-16]


## Plotting percentage of people served with max nitrate vs. birth defects

plot(Nitrate_Birth_Data1$Served, Nitrate_Birth_Data1$Weight,
     xlab="percentage of people who were served the maximum amount of nitrates recorded", 
     ylab = "Percentage of infants with a birthweight of less than 1500 grams", 
     main="The influence of nitrates in drinking water on birth weight", 
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)
plot(Nitrate_Birth_Data1$Served, Nitrate_Birth_Data1$Premie,
     xlab="percentage of people who were served the maximum amount of nitrates recorded", 
     ylab = "Percent of infants born prematurely", 
     main="The influence of nitrates in drinking water on premature birth rates", 
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)
plot(Nitrate_Birth_Data1$Served, Nitrate_Birth_Data1$Deaths,
     xlab="percentage of people who were served the maximum amount of nitrates recorded", 
     ylab = "Mortality rate per 1000 births", 
     main="The influence of nitrates in drinking water on infant death rates",
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7)



### Fresno County Nitrate Analysis
Fresno <- Nitrate_Birth_Data1 %>% ### Observing if birth defects in counties with high amounts of agriculture will be more influenced by nitrates levels 
  filter(County.x == "Fresno")
plot(Fresno$Served, Fresno$Premie)
plot(Fresno$Served, Fresno$Deaths,
     xlab="percentage of people who were served the maximum amount of nitrates recorded", 
     ylab = "Mortality rate per 1000 births", 
     main="The influence of nitrates in drinking water on infant death rates in Fresno County",
     cex.lab=0.7, cex.axis=0.7, cex.main=0.7) ### I plotted this plot with the labels because I am using it as a representative for the general trend seen in the County plots
plot(Fresno$Served, Fresno$Weight)

### Tulare County Nitrate analysis
Tulare <- Nitrate_Birth_Data1 %>% ### Observing if birth defects in counties with high amounts of agriculture will be more influenced by nitrates levels 
  filter(County.x == "Tulare")
plot(Tulare$Served, Tulare$Premie)
plot(Tulare$Served, Tulare$Deaths)
plot(Tulare$Served, Tulare$Weight)
