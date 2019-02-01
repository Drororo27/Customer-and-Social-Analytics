#Mobile Analytics Assignment
#Shen YANG

rm(list=ls())
setwd("~/Documents/UCI/Customer and Social Analytics/Mobile Analytics Individual Assignment")

######################################### Data Processing #########################################
library(dplyr)
# read data
geo_data = read.csv("Geo-Fence Analytics.csv")

geo_data$didclick = as.factor(geo_data$didclick)

# create imp_large
geo_data = mutate(geo_data, imp_large = ifelse(imp_size == "728x90",1,0))
geo_data$imp_large = as.factor(geo_data$imp_large)

# create cat_entertainment
geo_data = mutate(geo_data, cat_entertainment = ifelse(app_topcat %in% c("IAB1","IAB1-6"),1,0))
geo_data$cat_entertainment = as.factor(geo_data$cat_entertainment)

# create cat_social
geo_data = mutate(geo_data, cat_social = ifelse(app_topcat == "IAB14",1,0))
geo_data$cat_social = as.factor(geo_data$cat_social)

# create cat_tech
geo_data = mutate(geo_data, cat_tech = ifelse(app_topcat == "IAB19-6",1,0))
geo_data$cat_tech = as.factor(geo_data$cat_tech)

# create os_ios
geo_data = mutate(geo_data, os_ios = ifelse(device_os == "iOS",1,0))
geo_data$os_ios = as.factor(geo_data$os_ios)

# create distance
library(aspace)
geo_data = mutate(geo_data, distance = 6371 * acos(cos(as_radians(device_lat)) * 
                    cos(as_radians(geofence_lat)) * cos(as_radians(device_lon) - as_radians(geofence_lon)) 
                    + sin(as_radians(device_lat)) * sin(as_radians(geofence_lat))) )

# create distance_squared
geo_data = mutate(geo_data, distance_squared = distance^2)

# create ln_app_review_vol
geo_data = mutate(geo_data, ln_app_review_vol = log(app_review_vol))

head(geo_data)

###################################### Descriptive Statistics ######################################
# summary statistics
library(pastecs)
col_name = c("didclick","distance","imp_large","cat_entertainment", "cat_social", "cat_tech", 
             "os_ios", "ln_app_review_vol" , "app_review_val")
for(i in 1:length(col_name)){
  
  if(col_name[i] %in% c("imp_large","cat_entertainment", "cat_social", "cat_tech", 
                        "os_ios")){
    print(col_name[i])
    print(summary(geo_data[,which(colnames(geo_data)==col_name[i])]))
  }else{
    print(col_name[i])
    print(stat.desc(geo_data[,which(colnames(geo_data)==col_name[i])]))
  }
  
}

# the correlations among the above variables
library(corrplot)
data.matrix(geo_data[,which(colnames(geo_data) %in% col_name)]) %>%
  cor() %>%
  corrplot.mixed(lower.col = "black", number.cex = .85, tl.cex = 0.65)

# the relationship of distance (x-axis) and click-through-rate (y-axis), 
# and any other pairs of variables of interest.
library(ggplot2)
# create distance_group
geo_data$distance_group[geo_data$distance > 10] = 7
geo_data$distance_group[geo_data$distance <= 10] = 6
geo_data$distance_group[geo_data$distance <= 7] = 5
geo_data$distance_group[geo_data$distance <= 4] = 4
geo_data$distance_group[geo_data$distance <= 2] = 3
geo_data$distance_group[geo_data$distance <= 1] = 2
geo_data$distance_group[geo_data$distance <= 0.5] = 1
geo_data$distance_group = as.factor(geo_data$distance_group)

# calculate the click_through_rate
click_through_rate = c()
for (i in 1:7) {
  click_through_rate[i] = sum(geo_data$didclick[which(geo_data$distance_group==i)])/
    length(which(geo_data$distance_group==i))
}
distance_click = data.frame(distance_group = c(1:7), click = click_through_rate)

# plot click_through_rate ~ distance_group
ggplot(data = distance_click, aes(distance_group, click)) +
  geom_point() +
  geom_smooth(alpha=0.3, se=T) + 
  xlab("distance_group") 

# explore click_through_rate ~ device
click_through_rate2 = c()
click_through_rate2[1] = sum(geo_data$didclick[which(geo_data$device_os=="Android")])/
  length(which(geo_data$device_os=="Android"))
click_through_rate2[2] = sum(geo_data$didclick[which(geo_data$device_os=="iOS")])/
  length(which(geo_data$device_os=="iOS"))

device_click = data.frame(device = c("Android","iOS"), click = click_through_rate2)

ggplot(data = device_click, aes(device, click)) +
  geom_col() + 
  ylab("click_through_rate") 


###################################### Logistics Regression ######################################
#put all varibles into the regression
fit1 = glm(didclick ~ distance + distance_squared + imp_large + cat_entertainment + cat_social + 
           cat_tech + os_ios + ln_app_review_vol + app_review_val, data = geo_data, family = "binomial")
summary(fit1)

# simplify the model

#Variable Importance
#To assess the relative importance of individual predictors in the model, we can also look at 
#the absolute value of the t-statistic for each model parameter. 
library(caret)
varImp(fit1)

fit2 = glm(didclick ~ distance + distance_squared + imp_large + 
             cat_tech + os_ios, data = geo_data, family = "binomial")
summary(fit2)

anova(fit1, fit2, test="Chisq")

par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

length(which(abs(resid(fit2))>3))

summary(geo_data$didclick[which(abs(resid(fit2))>3)])


#generate a new data set
#777,99802018，9981,442
geo_data_choice_based = filter(geo_data, didclick == 1)
a = filter(geo_data, didclick == 0)
set.seed(442)
geo_data_choice_based = rbind(geo_data_choice_based, a[sample(dim(a)[1],828),])
rm(a)

#regression
fit1_choice_based = glm(didclick ~ distance + distance_squared + imp_large + cat_entertainment + cat_social + 
                          cat_tech + os_ios + ln_app_review_vol + app_review_val, data = geo_data_choice_based, 
                        family = "binomial")
summary(fit1_choice_based)

par(mfrow=c(2,2))
plot(fit1_choice_based)
par(mfrow=c(1,1))

#Multicollinearity
library(car)
vif(fit1_choice_based)
varImp(fit1_choice_based)

fit2_choice_based = glm(didclick ~ distance + imp_large + cat_entertainment + cat_tech,
                        data = geo_data_choice_based, 
                        family = "binomial")
summary(fit2_choice_based)

fit3_choice_based = glm(didclick ~ distance + imp_large + cat_tech,
                        data = geo_data_choice_based, 
                        family = "binomial")
summary(fit3_choice_based)

anova(fit3_choice_based, fit1_choice_based, test="Chisq")
anova(fit3_choice_based, fit2_choice_based, test="Chisq")



#liner assumption
testdata = transmute(geo_data_choice_based, distance, distance_squared, ln_app_review_vol, app_review_val)
predictors = colnames(testdata)

library(tidyr)
prob = predict(fit1_choice_based,type = "response")
testdata = testdata %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Create the scatter plots:
ggplot(testdata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~predictors, scales = "free_y")

library(pscl)
pR2(fit1_choice_based)  
pR2(fit3_choice_based)  


populateRate = length(which(geo_data$didclick==1))/length(geo_data$didclick)
clickRate = 0.5

geo_data_choice_based$offsetvalue = log(((1 - populateRate)/populateRate)/((1 - clickRate)/clickRate))

fit4_choice_based = glm(didclick ~ distance + imp_large + cat_tech,
                        data = geo_data_choice_based, 
                        family = "binomial", offset = offsetvalue)
summary(fit4_choice_based)


pR2(fit4_choice_based)  


#plot ROC curve
library(pROC)
g = roc(geo_data_choice_based$didclick ~ predict(fit4_choice_based,type = "response"))
plot(g,print.thres = "best",print.auc=TRUE)
#find the best threshold
best_thres = coords(g, "best", ret=c("threshold", "specificity", "sensitivity"))

#confusion matrix
as.numeric(predict(fit4_choice_based, newdata=geo_data_choice_based, type = "response") > best_thres[1]) %>%
  confusionMatrix(reference = geo_data_choice_based$didclick)











#it is essential to check that the assumed model is actually a valid model


# look for 'McFadden'
#Unlike linear regression with ordinary least squares estimation, there is no R2 statistic which 
#explains the proportion of variance in the dependent variable that is explained by the predictors. 
#However, there are a number of pseudo R2 metrics that could be of value. Most notable is McFadden’s R2, 
#which is defined as 1−[ln(LM)/ln(L0)] where ln(LM) is the log likelihood value for the fitted model 
#and ln(L0) is the log likelihood for the null model with only an intercept as a predictor. 
#The measure ranges from 0 to just under 1, with values closer to zero indicating that the model has 
#no predictive power.
library(pscl)
pR2(fit4)  




