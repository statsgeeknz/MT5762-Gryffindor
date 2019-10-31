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
if(!require(MLmetrics))
  install.packages("MLmetrics")
if(!require(caret))
  install.packages("caret")

library(dplyr)
library(car)
library(corrplot)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(car)
library(MuMIn)
library(GGally)
library(effects)
library(ggpubr)
library(MLmetrics)
library(caret)

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

#glimpse(babydata_NONA)
# generate summary of the babydata after the changes
summary(babydata)


# create a subset of data with all numberic variables to be used to generate correlations
dataFiltered <- babydata %>% select(gestation, wt, age,wt_mother, dage,ht, dht, dwt)
#------------Correlation between numeric variables-----

correlations <- cor(dataFiltered, use = "pairwise.complete.obs")
correlations <- round(correlations, 2)

corrplot(correlations, type="upper", order="hclust", sig.level = 0.01, insig = "blank", diag = FALSE)

#------------Plotting and looking for correlations------------
babydata %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = smoke)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

babydata %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = race)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

babydata %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = drace)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

#filtering smoker moms and check the quantity of cigs smoked vs weight of babies
dataSmoker <- babydata %>% 
  filter(smoke == "Smokes Now")
dataSmoker %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#filtering non smoker moms vs weight of babies coloured by income
dataNonSmoker <- babydata %>% 
  filter(smoke == "Never")
dataSmoker %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = race)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#basically nothing changed there is no significant correlation between the variables, 
#the information is spread everywhere.

#filtering smoker moms and check the quantity of cigs smoked vs weight of babies
dataSmokerUCP <- babydata %>% 
  filter(smoke == "Until current pregnancy")
dataSmokerUCP %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()
#within once did not now 
dataSmokerA1Y <- babydata %>% 
  filter(smoke == "Once did, not now")
dataSmokerA1Y %>% 
  gather(gestation, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = race)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

#endOfPlotting

#------------Test of datasets---------------
set.seed(2)
CVData_size <- floor(0.2 * nrow(babydata))

CVData_index <- sample(1:nrow(babydata), CVData_size)

CVData <- babydata[CVData_index,] %>% select(-sex, -id)
fittedData <- babydata[-CVData_index,] %>% select(-sex, -id)

#------------Model 1---------

 # Creating Model 1
fittedData_NONA <- na.omit(fittedData)
fullmodel <- lm(wt~ ., data = fittedData_NONA, na.action = "na.fail")

model_1 <- step(fullmodel)

  # Model Diagnostics
Anova(model_1) # Significance test
vif(model_1) # Collinearity test
shapiro.test(resid(model_1)) # Normality Test
ncvTest(model_1) # Error spread test
durbinWatsonTest(model_1) # Error independence
confint(model_1) # Looking at the confidence intervals

  # Doing some plotting to test for error distribution
par(mfrow=c(3,2))
plot(model_1, which =1:2)
qqnorm(resid(model_1))
hist(resid(model_1))
modelResid <- resid(model_1)
plot(fitted(model_1),modelResid, ylab = 'residuals', xlab = 'Fitted Values')

  # Plotting the residual distribution per covariate
par(mfrow=c(3,2))
termplot(model_1, se=TRUE)

  # Plotting the partial residual distribution per covariate
par(mfrow=c(3,2))
termplot(model_1, se=TRUE, partial.resid = TRUE)


  # Correlation plots
numericOnly <- dataFiltered %>% 
  select_if(is.numeric)

ggpairs(numericOnly)

  # Confidence Intervals
plot(effect(term = "number", mod = model_1))
plot(effect(term = "race", mod = model_1))

corr_gestation <- ggscatter (fittedData_NONA, x = "gestation", y= "wt",
                                      add = "reg.line", conf.int = TRUE,
                                      cor.coef = TRUE, cor.method = "pearson")
corr_gestation

#------------Model 2 -----------------------------------------------------------------
 # Creating model 2
model2_initial <- lm(wt ~ 1, data= fittedData_NONA)
model_2Updated <- update(model2_initial, .~. +race +wt_mother +dwt +smoke + gestation + parity +ht)
model_2 <- model_2Updated

# The results, effectively model 2 includes the weights of father and mother
## also adds smoke factor. removes dad's height and number of cigaretts. 

# Model Diagnostics
Anova(model_2) # Significance test
vif(model_2) # Collinearity test
shapiro.test(resid(model_2)) # Normality Test
ncvTest(model_2) # Error spread test
durbinWatsonTest(model_2) # Error independence
confint(model_2) # Looking at the confidence intervals

# Doing some plotting to test for error distribution
modelResid <- resid(model_2)

par(mfrow=c(2,2))
plot(model_2, which =1:2)
qqnorm(resid(model_2))
hist(resid(model_2))

# Plotting the residual distribution per covariate
par(mfrow=c(3,3))
termplot(model_2, se=TRUE)

# Plotting the partial residual distribution per covariate
par(mfrow=c(3,3))
termplot(model_2, se=TRUE, partial.resid = TRUE)

  # Deciding between models
AIC(model_1)
AIC(model_2)

# validation ----

validationData_m1 <- CVData %>% select(all.vars(formula(model_1)))

validationData_m1_noNA <- na.omit(validationData_m1)
predictions_m1 <- model_1 %>% predict(validationData_m1_noNA)

mse_m1 <- MSE(predictions_m1, validationData_m1_noNA$wt)


validationData_m2 <- CVData %>% select(all.vars(formula(model_2)))

validationData_m2_noNA <- na.omit(validationData_m2)
predictions_m2 <- model_2 %>% predict(validationData_m2_noNA)

mse_m2 <- MSE(predictions_m2, validationData_m2_noNA$wt)


# 5 fold cross validation
babydata_NONA <- na.omit(babydata)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
CVmodel_1 <- train(formula(model_1), data = babydata_NONA, method = "lm",
               trControl = train.control)
# Summarize the results
print(CVmodel_1)
coef(model_1)

set.seed(234) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
CVmodel_2 <- train(formula(model_2), data = babydata_NONA, method = "lm",
                   trControl = train.control)
# Summarize the results
print(CVmodel_2)
coef(model_2)




# Bootstrap ---------------------------------------------------------------

#first we need to resample the data 1000 times with replacement, and then use the 
#new data to fit the chosen model. use quantile bootstrap to find the empirical
#confidence intervals for the parameters

nr <- dim(fittedData_NONA)[1]
names <- names(coef(lm(model_2, data = fittedData_NONA)))
newcoe <- matrix(nr = 1000, nc = 14)
set.seed(0702)
for (i in 1:1000) {
  new_data <- fittedData_NONA[sample(1:nr, nr, replace = T), ]
  newlm <- lm(model_2, data = new_data)
  newcoe[i, ] <- coef(newlm)
}

coninter <- matrix(nr = 14, nc = 2)
for (i in 1:14)
{
  coninter[i, ] <- quantile(newcoe[, i], c(0.025, 0.975), na.rm = T)
}
rownames(coninter) <- names
colnames(coninter) <- c("2.5%", "97.5%")
coninter

cooks.distance(model_2)
plot(modelResid)
