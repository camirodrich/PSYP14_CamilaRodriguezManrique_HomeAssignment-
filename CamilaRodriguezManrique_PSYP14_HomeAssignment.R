library(psych)
library(dplyr)
library(car)
library(r2glmm)
library(lme4) 
library(lmerTest)
library(MuMIn)
library(lmtest)
library(lmboot)
library(sandwich)
library(boot)
library(lm.beta)
library(tidyverse)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

###Assignment Part 1###

home_sample_1 <- read.csv("http://tinyurl.com/yxm5rd89")

View(home_sample_1)
summary(home_sample_1)
#In the summary, one participant (patient 88) indicated on the pain scale from 0 to 10 that their pain level was "55", which is an invalid option.
#By looking at his painCat variable which lies close to the mean and median of all the participants, I can assume that the patient meant to type "5" instead of "55".
#The first step, would therefore be to correct this in the data set.
home_sample_1$pain <- as.numeric(replace(home_sample_1$pain, 88, "5"))
#Another participant (case 34) also had an invalid response. They reported an STAI_trait of "4.2" even though the scale only starts from 20. 
#Given that their pain variable is "5", we can assume that "4.2" was meant to be a "42". Therefore, I will correct this.
home_sample_1$STAI_trait <- as.numeric(replace(home_sample_1$STAI_trait, 34, "42"))
View(home_sample_1) #Checking whether these worked and they did!

#Model Diagnostics 
initialmodel2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data= home_sample_1)

#Checking for outliers with Cook's distance
initialmodel2 %>% 
  plot(which = 4)
# -> No significant outliers were found (Cook's distance < 1)

#Check for the assumption of normality
#Q-QPlot
initialmodel2 %>% 
  plot (which = 2)
#Histogram 
residuals_model2 = enframe(residuals(initialmodel2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()
#Skew and Kurtosis
describe(residuals(initialmodel2))
# -> The assumption of normality is not violated.

#Checking for the assumption of linearity 
initialmodel2 %>%
  residualPlots()
# -> Since none of the tests were significant (p < 0.05), the assumption of linearity is not violated. 

#Checking for the assumption of homoscedasticity
initialmodel2 %>% 
  plot(which=3)
# -> The shape on the plot is not funnel shape, therefore the assumption of homoscedasticity is not violated.

#Checking for the assumption of no multicollinearity
initialmodel2 %>% 
  vif()
#This shows that variables "cortisol_saliva" and "cortisol_serum" have a VIF higher then 3, meaning that a problem regarding multicollinearity is present here. 
#Looking at the assignment, it makes sense that these two variables would be problematic as they are both based on levels of cortisol.
#Additionally, the assignment says that "cortisol_serum" is more reliable than "cortisol_saliva", thus I will remove the variable "cortisol_saliva" from the model.

newmodel2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data= home_sample_1)

#Rechecking the previously done assumption checks. 
#Outliers
newmodel2 %>% 
  plot(which = 4)
# -> no outliers found

#Normality
newmodel2 %>% 
  plot (which = 2)
residuals_newmodel2 = enframe(residuals(newmodel2))
residuals_newmodel2 %>%
  ggplot() + aes(x = value) + geom_histogram()
describe(residuals(newmodel2))
# -> The assumption of normality is not violated.

#Linearity 
newmodel2 %>%
  residualPlots()
# -> The assumption of linearity is not violated. 

#Homoscedasticity
newmodel2 %>% 
  plot(which=3)
# -> The assumption of homoscedasticity is not violated.

#No multicollinearity
newmodel2 %>% 
  vif()
# -> After removing "cortisol_saliva", none of the remaining variables have a VIF > 3 so there is no problem of multicollinearity. 

#Hierarchical Regression:
model1 <- lm(pain ~ sex + age, data = home_sample_1)
model2 <- newmodel2

summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
#The adjusted R squared indicates how much variance the model explains. 
#Model2 clearly explains more variance than model1 as the adjusted R squared is higher.

AIC(model1)
AIC(model2)
#The difference between the AIC of model1 and model2 is larger than 2, meaning that there is a significant difference between both models.
#The AIC of model2 is smaller, therefore model2 has less errors and a better model fit for the predictive data.

anova(model1, model2)
#The F-test is significant, therefore the two models are significantly different in terms of their residual errors.

summary(model1) #B = estimate std. #p-value 
summary(model2)

confint(model1) #confidence interval
confint(model2)

lm.beta(model1) #beta 
lm.beta(model2)

# -> conclusion: The two models are significantly different and model2 is a better fit to the data!


###Assignment Part 2###

#Making the proposed research model
researchmodel <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = home_sample_1)

#Model Diagnostics
#Outliers
researchmodel %>% 
  plot(which = 4)
# -> There are no apparent outliers that need to excluded.

#Normality
researchmodel %>% 
  plot (which = 2)
residuals_initialresearchmodel = enframe(residuals(researchmodel))
residuals_initialresearchmodel %>%
  ggplot() + aes(x = value) + geom_histogram()
describe(residuals(researchmodel))
# -> The assumption of normality is not violated.

#Linearity 
researchmodel %>%
  residualPlots()
# -> The assumption of linearity is not violated. 

#Homoscedasticity
researchmodel %>% 
  plot(which=3)
# -> The assumption of homoscedasticity is not violated.

#No multicollinearity
researchmodel %>% 
  vif()
# -> The assumption of no multicollinearity is not violated.

#Backward Regression 
backwardresearchmodel <- step(researchmodel, direction = "backward")
#This analysis shows that the most accurate model for predicting pain includes the following predictors: age, pain_cat, mindfulness and cortisol_serum.
backwardmodel <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = home_sample_1)

theorybasedmodel <- model2

#Comparing the initial research model, the backward model and the theory-based model
summary(researchmodel)$adj.r.squared
summary(backwardmodel)$adj.r.squared
summary(theorybasedmodel)$adj.r.squared
#researchmodel ADJR2 < theorybasedmodel ADJR2 < backwardmodel ADJR2
#The backwardmodel explains the most variance of the data out of the three models.

AIC(researchmodel)
AIC(backwardmodel)
AIC(theorybasedmodel)
#All the three models are significantly different from each other as their AIC difference are larger than 2. 
#The AIC of the backwardmodel is the smallest, therefore it fits the best on the data.

anova(researchmodel,backwardmodel)
anova(backwardmodel,theorybasedmodel)
#The three models are not significantly different from each other. 
#However, AIC is more reliable.

summary(researchmodel) #B = estimate std. #p-value 
summary(backwardmodel)
summary(theorybasedmodel)

confint(researchmodel) #confidence interval
confint(backwardmodel)
confint(theorybasedmodel)

lm.beta(researchmodel) #beta
lm.beta(backwardmodel)
lm.beta(theorybasedmodel)
# -> Conclusion: the backward model is a better fit to the data when it comes to predicting pain in comparison to the research model and the theory-based model.

#New Data Set
home_sample_2 <- read.csv("http://tinyurl.com/87v6emky")

View(home_sample_2)
summary(home_sample_2)
# -> All data seems to valid with no suprising data.

#Testing the models ability to predict on the new data set
pred_test_back <- predict(backwardmodel, home_sample_2)
pred_test_theory <- predict(theorybasedmodel, home_sample_2)
pred_test_back
pred_test_theory

RSS_test_back = sum((home_sample_2[, "pain"] - pred_test_back)^2)
RSS_test_theory = sum((home_sample_2[, "pain"] - pred_test_theory)^2)
RSS_test_back
RSS_test_theory 
#This test reveals that the backward model has more error than the theory-based model when it comes to predicting pain data. 
# -> Conclusion: The theory-based model is better at predicting pain data in general in comparison to the backward model. 

###Assignment Part 3###

#New data set (3)
home_sample_3 <- read.csv("http://tinyurl.com/b385chpu")
View(home_sample_3)

summary(home_sample_3) 
#A case (patient 2) shows a negative number in household_income variable which is seems invalid. However, household_income is a variable we will not be using in this part of the assignment so I will not remove this case. 
#Another case (patient 25) indicated that their sex was "woman" instead of using the binary options of "male" and "female".
#Therefore, I will change their sex item from "woman" to "female".
home_sample_3corrected <- home_sample_3 %>% 
  mutate(sex = replace(sex, sex== "woman", "female"))
view(home_sample_3corrected)

#New data set (4)
home_sample_4 <- read.csv("http://tinyurl.com/4f8thztv")
View(home_sample_4)
summary(home_sample_4) 
#All the data seems valid with no surprising data.

#Exploring the data through graphs
home_sample_3corrected %>% 
  mutate(class= factor(hospital))

home_sample_3corrected = home_sample_3corrected %>%
  mutate(hospital = factor(hospital, levels = c(
    "hospital_1",
    "hospital_2",
    "hospital_3",
    "hospital_4",
    "hospital_5",
    "hospital_6",
    "hospital_7",
    "hospital_8",
    "hospital_9", 
    "hospital_10")))

#predictor = age 
home_sample_3corrected %>% 
  ggplot() + aes(y = pain, x = age) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)
home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = age, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
#predictor = STAI_trait
home_sample_3corrected %>% 
  ggplot()+aes(y = pain, x = STAI_trait) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)
home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = STAI_trait, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
#predictor = pain_cat 
home_sample_3corrected %>% 
  ggplot()+aes(y = pain, x = pain_cat) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)
home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = pain_cat, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
#predictor = mindfulness
home_sample_3corrected %>% 
  ggplot()+aes(y = pain, x = mindfulness) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)
home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = mindfulness, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
#predictor = cortisol_serum 
home_sample_3corrected %>% 
  ggplot()+aes(y = pain, x = cortisol_serum) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)
home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)

#Building the random intercept model and the fixed model
model_randomintercept <- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data= home_sample_3corrected)

#Comparing the theory-based model with the random intercept model 

summary(model_randomintercept) #B = estimate std. #p-value 

confint(model_randomintercept) #confidence intervals

stdCoef.merMod(model_randomintercept) #beta

r.squaredGLMM(model_randomintercept) #R2marginal #R2conditional

#Looking for the most influential predictor 
r2beta(model_randomintercept, method = "nsj", data = home_sample_3corrected)
# -> the most influential predictor is cortisol_serum as it explains the most variance (0.123) and is a significant predictor (CL: 0.44)

#Prediction on data set 4
pred_modelri <- predict(model_randomintercept, home_sample_4,allow.new.levels = TRUE)
pred_modelri
summary(pred_modelri)

#Calculating the RSS
pred_modelri_RSS <- sum((home_sample_4$pain - pred_modelri)^2)
pred_modelri_RSS

#Calculating the TSS
model_randomintercept_mean <- lm(pain ~ 1, data = home_sample_4)
pred_modelri_TSS <- sum((home_sample_4$pain - predict(model_randomintercept_mean))^2)
pred_modelri_TSS

#Calculating the R2 ( = 1-RSS/TSS)
pred_modelri_R2 <- 1-(pred_modelri_RSS/pred_modelri_TSS)
pred_modelri_R2
# -> when compared to the marginal and condition R2, it is closer to the marginal r2

#Building a new model with the most influential variable 
model_influential <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = home_sample_3corrected)

#Plot
home_sample_3corrected = home_sample_3corrected %>%
  mutate(pred_slope = predict(model_influential))

finalplot <- home_sample_3corrected %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method= "lm", aes(y = pred_slope, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 2)

finalplot