#------------Libraries------
if(!require(corrplot))
  install.packages("corrplot")
if(!require(car))
  install.packages("car")
if(!require(MuMIn))
  install.packages("MuMIn")
if(!require(GGally))
  install.packages("GGally")
if(!require(effects))
  install.packages("effects")

library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(car)
library(MuMIn)
library(GGally)
library(effects)
library(ggpubr)

#------------Loading files---------
# load data into R and generate summary
babydata <- read.csv("babies23.data", sep = "")

summary(babydata)

# rename variable wt.1 to wt_mother
babydata <- babydata %>% rename(wt_mother = wt.1)
#------------Filtering non relevant Information------------
# check data for unknown values
babydata %>% filter(sex == 9)
babydata %>% filter(wt == 999)
babydata %>% filter(parity == 99)
babydata %>% filter(race == 99)
babydata %>% filter(age == 99)
babydata %>% filter(ed == 9)
babydata %>% filter(ht == 99)
babydata %>% filter(wt_mother == 999)
babydata %>% filter(drace == 99)
babydata %>% filter(dage == 99)
babydata %>% filter(ded == 9)
babydata %>% filter(dht == 99)
babydata %>% filter(dwt == 999)
babydata %>% filter(inc == 98 | inc == 99)
babydata %>% filter(smoke == 9)
babydata %>% filter(time == 9 | time == 98 | time == 99)
babydata %>% filter(number == 9 | number == 98 | number == 99)
babydata %>% filter(gestation == 999)

# update the data with NAs for unknown values
#babydata$sex[babydata$sex == 9] = NA
babydata$wt[babydata$wt == 999] = NA
babydata$parity[babydata$parity == 99] = NA
babydata$race[babydata$race == 99] = NA
babydata$age[babydata$age == 99] = NA
babydata$ed[babydata$ed == 9] = NA
babydata$ht[babydata$ht == 99] = NA
babydata$wt_mother[babydata$wt_mother == 999] = NA
babydata$drace[babydata$drace == 99] = NA
babydata$dage[babydata$dage == 99] = NA
babydata$ded[babydata$ded == 9] = NA
babydata$dht[babydata$dht == 99] = NA
babydata$dwt[babydata$dwt == 999] = NA
babydata$inc[babydata$inc == 98 | babydata$inc == 99] = NA
#babydata$smoke[babydata$smoke == 9] = NA
#babydata$time[babydata$time == 9 | babydata$time == 98 | babydata$time == 99] = NA
#babydata$number[babydata$number == 9 | babydata$number == 98 | babydata$number == 99] = NA
babydata$gestation[babydata$gestation == 999] = NA
babydata$marital[babydata$marital == 0] = NA

# changing the race to 5 for values 0 to 5
babydata$race[babydata$race %in% 0:5] = 5
babydata$drace[babydata$drace %in% 0:5] = 5

# changing the education value to 6 for values 6 & 7
babydata$ed[babydata$ed %in% c(6, 7)] = 6
babydata$ded[babydata$ded %in% c(6, 7)] = 6

# changing the date value to datatype date from integer, where 1096 is January1,1961
babydata$date <- as.Date(babydata$date, origin = "1958-01-01")


#------------Unit transformation------
#oz to kg (babies)
oz <- 0.02834
babydata$wt <- babydata$wt * oz
#lb to kg (mothers and fathers)
lb <- 0.4536
babydata$wt_mother <- babydata$wt_mother *lb
babydata$dwt <- babydata$dwt *lb
#endOfUnitTransformation

#------------Factorisation and conversion------------
# convert all the categorical variable to factors with given lables
babydata$smoke <- factor(babydata$smoke, 
                     levels = c(0, 1, 2, 3, 9), 
                     labels = c("Never", "Smokes Now", "Until current pregnancy", 
                                "Once did, not now", "unknown"))
babydata$time <- factor(babydata$time, 
                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99), 
                     labels = c("never smoked", "still smokes", "during current preg.", "within 1yr", 
                                "1-2 yrs ago", "2-3 yrs ago", "3-4 yrs ago", "5-9 yrs ago", 
                                "10+ yrs ago", "quit & dont know", "unknown", "not asked"))
babydata$inc <- factor(babydata$inc, 
                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                     labels = c("under 2500", "2500-4999", "5000-7499", "7500-9999", "10000-12499", 
                              "12500-14999", "15000-17499", "17500-19999", "20000-22499", "22500+"))
babydata$marital <- factor(babydata$marital, 
                     levels = c(1, 2, 3, 4, 5), 
                     labels = c("married", "legally separated", "divorced", "widowed", "never married"))
babydata$ded <- factor(babydata$ded, 
                     levels = c(0, 1, 2, 3, 4, 5, 6), 
                     labels = c("< 8th grade", "8-12th grade(not grad)", "HS graduate", "HS + trade", 
                                "HS + some college", "College graduate", "Trade school(unclear HS)"))
babydata$ed <- factor(babydata$ed, 
                     levels = c(0, 1, 2, 3, 4, 5, 6), 
                     labels = c("< 8th grade", "8-12th grade(not grad)", "HS graduate", "HS + trade", 
                                "HS + some college", "College graduate", "Trade school(unclear HS)"))
babydata$race <- factor(babydata$race, 
                     levels = c(5, 6, 7, 8, 9), 
                     labels = c("White", "Mex", "Black", "Asian", "mixed"))
babydata$drace <- factor(babydata$drace, 
                     levels = c(5, 6, 7, 8, 9), 
                     labels = c("White", "Mex", "Black", "Asian", "mixed"))
babydata$sex <- factor(babydata$sex, 
                     levels = c(1, 2, 9), 
                     labels = c("male", "female", "unknown"))
babydata$number <- factor(babydata$number, 
                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99), 
                     labels = c("never", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", 
                                "40-60", "60+", "smoke but dont know", "unknown", "not asked"))


# generate summary of the babydata after the changes
summary(babydata)


# create a subset of data with all numberic variables to be used to generate correlations
dataFiltered <- babydata %>% select(gestation, wt, parity, age,wt_mother, dage,ht, dht, dwt)
#------------Correlation between numeric variables-----

correlations <- cor(dataFiltered, use = "pairwise.complete.obs")
correlations <- round(correlations, 2)

corrplot(correlations, type="upper", order="hclust", sig.level = 0.01, insig = "blank", diag = FALSE)

#------------Plotting and looking for correlations------------
babydata %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = smoke)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

babydata %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = race)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

babydata %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = drace)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

#filtering smoker moms and check the quantity of cigs smoked vs weight of babies
dataSmoker <- babydata %>% 
  filter(smoke == "Smokes Now")
dataSmoker %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#filtering non smoker moms vs weight of babies coloured by income
dataNonSmoker <- babydata %>% 
  filter(smoke == "Never")
dataSmoker %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = inc)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#basically nothing changed there is no significant correlation between the variables, 
#the information is spread everywhere.
#filtering smoker moms and check the quantity of cigs smoked vs weight of babies
dataSmokerUCP <- babydata %>% 
  filter(smoke == "Until current pregnancy")
dataSmokerUCP %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#within once did not now 
dataSmokerA1Y <- babydata %>% 
  filter(smoke == "Once did, not now")
dataSmokerA1Y %>% 
  gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

#endOfPlotting

#------------Test of datasets---------------


testData_size <- floor(0.2 * nrow(babydata))

testData_index <- sample(1:nrow(babydata), testData_size)

testData <- babydata[testData_index,] %>% select(-sex, -id)
fittedData <- babydata[-testData_index,] %>% select(-sex, -id)

#------------Model 1---------

 # Creating Model 1
fullmodel <- lm(wt~ ., data = na.omit(fittedData), na.action = "na.fail")

summary(fullmodel)
Anova(fullmodel)

model_1 <- step(fullmodel)

  # Model Diagnostics
Anova(model_1) # Significance test
vif(model_1) # Collinearity test
shapiro.test(resid(model_1)) # Normality Test
ncvTest(model_1) # Error spread test
durbinWatsonTest(model_1) # Error independence
confint(model_1) # Looking at the confidence intervals

  # Doing some plotting to test for error distribution
plot(model_1, which =1:2)
qqnorm(resid(model_1))
hist(resid(model_1))
  
modelResid <- resid(model_1)
plot(fitted(model_1),modelResid, ylab = 'residuals', xlab = 'Fitted Values')

numericOnly <- dataFiltered %>% 
  select_if(is.numeric)

ggpairs(numericOnly)

  # Confidence Intervals
race_confInt <- plot(effect(term = "race", mod = model_1))
plot(effect(term = "gestation", mod = model_1))
plot(effect(term = "ht", mod = model_1))

corr_gestation <- ggscatter (fittedData, x = "gestation", y= "wt",
                                      add = "reg.line", conf.int = TRUE,
                                      cor.coef = TRUE, cor.method = "pearson")



#------------Model 2 -----------------------------------------------------------------
model_2 <- dredge(fullmodel)

Anova(model_2)
vif(model_2)
qqnorm(resid(model_2))
shapiro.test(resid(model_2))
