if(!require(corrplot))
  install.packages("corrplot")

library(dplyr)
library(corrplot)

# load data into R and generate summary
data <- read.csv("babies23.data", sep = "")

summary(data)

# rename variable wt.1 to wt_mother
data <- data %>% rename(wt_mother = wt.1)

# check data for unknown values
data %>% filter(sex == 9)
data %>% filter(wt == 999)
data %>% filter(parity == 99)
data %>% filter(race == 99)
data %>% filter(age == 99)
data %>% filter(ed == 9)
data %>% filter(ht == 99)
data %>% filter(wt_mother == 999)
data %>% filter(drace == 99)
data %>% filter(dage == 99)
data %>% filter(ded == 9)
data %>% filter(dht == 99)
data %>% filter(dwt == 999)
data %>% filter(inc == 98 | inc == 99)
data %>% filter(smoke == 9)
data %>% filter(time == 9 | time == 98 | time == 99)
data %>% filter(number == 9 | number == 98 | number == 99)
data %>% filter(gestation == 999)

# update the data with NAs for unknown values
#data$sex[data$sex == 9] = NA
data$wt[data$wt == 999] = NA
data$parity[data$parity == 99] = NA
data$race[data$race == 99] = NA
data$age[data$age == 99] = NA
data$ed[data$ed == 9] = NA
data$ht[data$ht == 99] = NA
data$wt_mother[data$wt_mother == 999] = NA
data$drace[data$drace == 99] = NA
data$dage[data$dage == 99] = NA
data$ded[data$ded == 9] = NA
data$dht[data$dht == 99] = NA
data$dwt[data$dwt == 999] = NA
data$inc[data$inc == 98 | data$inc == 99] = NA
#data$smoke[data$smoke == 9] = NA
#data$time[data$time == 9 | data$time == 98 | data$time == 99] = NA
#data$number[data$number == 9 | data$number == 98 | data$number == 99] = NA
data$gestation[data$gestation == 999] = NA
data$marital[data$marital == 0] = NA

# changing the race to 5 for values 0 to 5
data$race[data$race %in% 0:5] = 5
data$drace[data$drace %in% 0:5] = 5

# changing the education value to 6 for values 6 & 7
data$ed[data$ed %in% c(6, 7)] = 6
data$ded[data$ded %in% c(6, 7)] = 6

# changing the date value to datatype date from integer, where 1096 is January1,1961
data$date <- as.Date(data$date, origin = "1958-01-01")


# convert all the categorical variable to factors with given lables
data$smoke <- factor(data$smoke, 
                     levels = c(0, 1, 2, 3, 9), 
                     labels = c("Never", "Smokes Now", "Until current pregnancy", "Once did, not now", "unknown"))
data$time <- factor(data$time, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99), 
                    labels = c("never smoked", "still smokes", "during current preg.", "within 1yr", "1-2 yrs ago", "2-3 yrs ago", "3-4 yrs ago",
                               "5-9 yrs ago", "10+ yrs ago", "quit & dont know", "unknown", "not asked"))
data$inc <- factor(data$inc, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                   labels = c("under 2500", "2500-4999", "5000-7499", "7500-9999", "10000-12499", 
                              "12500-14999", "15000-17499", "17500-19999", "20000-22499", "22500+"))
data$marital <- factor(data$marital, levels = c(1, 2, 3, 4, 5), labels = c("married", "legally separated", "divorced", "widowed", "never married"))
data$ded <- factor(data$ded, levels = c(0, 1, 2, 3, 4, 5, 6), 
                   labels = c("< 8th grade", "8-12th grade(not grad)", "HS graduate", "HS + trade", "HS + some college", 
                              "College graduate", "Trade school(unclear HS)"))
data$ed <- factor(data$ed, levels = c(0, 1, 2, 3, 4, 5, 6), 
                   labels = c("< 8th grade", "8-12th grade(not grad)", "HS graduate", "HS + trade", "HS + some college", 
                              "College graduate", "Trade school(unclear HS)"))
data$race <- factor(data$race, levels = c(5, 6, 7, 8, 9), labels = c("White", "Mex", "Black", "Asian", "mixed"))
data$drace <- factor(data$drace, levels = c(5, 6, 7, 8, 9), labels = c("White", "Mex", "Black", "Asian", "mixed"))
data$sex <- factor(data$sex, levels = c(1, 2, 9), labels = c("male", "female", "unknown"))
data$number <- factor(data$number, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99), 
                      labels = c("never", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-60", "60+", "smoke but dont know", "unknown", "not asked"))


# generate summary of the data after the changes
summary(data)


# create a subset of data with all numberic variables to be used to generate correlations
dataFiltered <- data %>% select(gestation, wt, parity, age,wt_mother, dage,ht, dht, dwt)
#----correlation between numeric variables-----

correlations <- cor(dataFiltered, use = "pairwise.complete.obs")
correlations <- round(correlations, 2)

corrplot(correlations, type="upper", order="hclust", sig.level = 0.01, insig = "blank", diag = FALSE)



data %>% gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = smoke)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

data %>% gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = race)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

data %>% gather(gestation, parity, age, wt_mother, ht, dage, dwt, dht, key = "param", value = "value") %>%
  ggplot(aes(x = value, y = wt, colour = drace)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()

testData_size <- floor(0.2 * nrow(data))

testData_index <- sample(1:nrow(data), testData_size)

testData <- data[testData_index,]
fittedData <- data[-testData_index,]




