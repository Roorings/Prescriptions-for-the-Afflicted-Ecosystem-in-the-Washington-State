library(car) 
library(DT)
library(flextable)
library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(lubridate)
library(tidytext)
library("RColorBrewer")
library(randomForest)
library(tictoc)
library(e1071)
library(ggpubr)
library("caret")
library(dbplyr)
library(dplyr)
library(plyr)

getwd()
setwd('D:/SCHOOL/INCLASS/20-21spring/OTHERS/2021America/C')

data = read.csv("data_merged7.csv")
data_pn= data[data$Lab.Status == "Positive ID" | data$Lab.Status == "Negative ID",c("Lab.Status","suitability","time_normalization","Location")]
data_n = data[data$Lab.Status == "Negative ID",c("Lab.Status","suitability","time_normalization","Location")]
data_p = data[data$Lab.Status == "Positive ID",c("Lab.Status","suitability","time_normalization","Location")]
data_p$Lab.Status = 1

set.seed(53845)
m = sample(1:3237,17)
data_n_sample = data.frame()
for (i in (0:17)){data_n_sample = rbind(data_n_sample,data_n[m[i],])}
data_n_sample$Lab.Status = 0
ourdata = rbind(data_n_sample,data_p)

#Logistic Regression(full model)
fit_full = glm(Lab.Status~., family = binomial(link = "logit"), data = ourdata, control=list(maxit=1000))
as_flextable(fit_full)

# test vif
vif_full = as.data.frame(vif(fit_full))
vif_full

#stepwise to get reduced model
fit_AIC = step(fit_full, k = 2)
as_flextable(fit_AIC)

# anova to test differences
anova = anova(fit_full, fit_AIC,test = "Chisq")
flextable(data = anova)

#coefficient analysis
coef = as.data.frame(exp(coef(fit_AIC)))
names(coef) = "Coefficient"
coef

# Confidence Interval of coefficients
coef = exp(confint(fit_AIC))
as.data.frame(exp(confint(fit_AIC)))

# confussion matrix
true = as.factor(ourdata$Lab.Status)
pred = as.numeric(predict(fit_AIC,type = "response", ourdata)>0.5)
pred = as.factor(pred)
confusion_matrix = confusionMatrix(data = pred, reference = true)
confusion_matrix

# test overdispersion
fit.od = glm(Lab.Status~., family = quasibinomial(link = "logit"), data = ourdata, control=list(maxit=100))
pchisq(summary(fit.od)$dispersion * fit_AIC$df.residual,fit_AIC$df.residual, lower = F)
#H_0 is phi = 1 so we can not reject the null hypothesis.






data_pn <- data_pn %>% 
  mutate(Lab.Status = if_else(Lab.Status == "Positive ID", 1, 0)) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(Lab.Status, suitability, time_normalization,Location, everything())

cor_pn <- cor(data_pn[,1:3])
cor_pn
ggcorrplot(cor_pn,lab = T)
