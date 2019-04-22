rm(list=ls())
setwd("C:/Users/singh yash/Desktop/LTFS")

train <- read.csv(file = "train.csv", header = T, sep = ",")
test <- read.csv(file = "test.csv", header = T, sep = ",")

# combining train and test data from data processing
test$loan_default <- NA

data.complete <- rbind(train, test)
View(data.complete)
str(data.complete)

# data conversions
data.complete$branch_id <- as.factor(data.complete$branch_id)
data.complete$supplier_id <- as.factor(data.complete$supplier_id)
data.complete$manufacturer_id <- as.factor(data.complete$manufacturer_id)
data.complete$Current_pincode_ID <- as.factor(data.complete$Current_pincode_ID)
data.complete$State_ID <- as.factor(data.complete$State_ID)
data.complete$Employee_code_ID <- as.factor(data.complete$Employee_code_ID)
data.complete$MobileNo_Avl_Flag <- as.factor(data.complete$MobileNo_Avl_Flag)
data.complete$Aadhar_flag <- as.factor(data.complete$Aadhar_flag)
data.complete$PAN_flag <- as.factor(data.complete$PAN_flag)
data.complete$VoterID_flag <- as.factor(data.complete$VoterID_flag)
data.complete$Driving_flag <- as.factor(data.complete$Driving_flag)
data.complete$Passport_flag <- as.factor(data.complete$Passport_flag)
#data.complete$NEW.ACCTS.IN.LAST.SIX.MONTHS <- as.factor(data.complete$NEW.ACCTS.IN.LAST.SIX.MONTHS)
#data.complete$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS <- as.factor(data.complete$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
#data.complete$NO.OF_INQUIRIES <- as.factor(data.complete$NO.OF_INQUIRIES)
data.complete$loan_default <- as.factor(data.complete$loan_default)

#install.packages("stringr")
library(stringr)

str(data.complete)

# processing the date variables - average account age and credit history length
data.complete$AVERAGE.ACCT.AGE_year1 <- as.numeric(str_extract(data.complete$AVERAGE.ACCT.AGE,"[0-9]*"))*12
data.complete$AVERAGE.ACCT.AGE_month1 <- as.numeric(str_extract(data.complete$AVERAGE.ACCT.AGE,"[0-9]*[ ]+[0-9]*"))
data.complete$AVERAGE.ACCT.AGE <- data.complete$AVERAGE.ACCT.AGE_year1 + data.complete$AVERAGE.ACCT.AGE_month1

data.complete$CREDIT.HISTORY.LENGTH_year1 <- as.numeric(str_extract(data.complete$CREDIT.HISTORY.LENGTH,"[0-9]*"))*12
data.complete$CREDIT.HISTORY.LENGTH_month1 <- as.numeric(str_extract(data.complete$CREDIT.HISTORY.LENGTH,"[0-9]*[ ]+[0-9]*"))
data.complete$CREDIT.HISTORY.LENGTH <- data.complete$CREDIT.HISTORY.LENGTH_year1 + data.complete$CREDIT.HISTORY.LENGTH_month1

data.complete <- data.complete[,1:41]
data.complete$DisbursalDate <- as.Date(data.complete$DisbursalDate)
data.complete$DisbursalDate <- Sys.Date() - data.complete$DisbursalDate
data.complete$DisbursalDate <- as.numeric(data.complete$DisbursalDate)

# data processing for missing value treatment and outlier treatment and checking the summary stats of the variables

apply(is.na(data.complete),2,sum)
colSums(is.na(data.complete))

var_summary <- function(x){
  if(class(x)=="numeric") {
    var_type = class(x)
    n = length(x)
    nmiss = sum(is.na(x))
    mean = mean(x, na.rm = T)
    median = median(x, na.rm = T)
    var = var(x, na.rm = T)
    sd = sqrt(var)
    LC1 = mean(x, na.rm = T) - 3*sqrt(var(x, na.rm = T))
    UC1 = mean(x, na.rm = T) + 3*sqrt(var(x, na.rm = T))
    pctl = quantile(x,p=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), na.rm = T)
    return(c(var_type=var_type, n=n, nmiss=nmiss, mean=mean, median=median, var=var, sd=sd, LC1=LC1, UC1=UC1, pctl=pctl))
  }
  else {
    var_type  = class(x)
    n = length(x)
    nmiss = sum(is.na(x))
    freq = table(x)
    prop = prop.table(table(x))
    return(c(var_type=var_type, n=n, nmiss=nmiss, freq=freq, prop=prop))
  }
}

num_vars <- sapply(data.complete, is.numeric)
other_vars <- !sapply(data.complete, is.numeric)

data.complete[num_vars]
my_num_vars <- t(data.frame(apply(data.complete[num_vars],2,FUN = var_summary)))
my_other_vars <- apply(data.complete[other_vars],2,FUN = var_summary)
View(my_num_vars)
View(my_other_vars)
write.csv(my_num_vars, file = "summary_stats.csv")

# outlier treatment for numeric variables
out_treat <- function(x) {
  quantiles <- quantile(x,0.01, na.rm=T)
  x[x < quantiles] = quantiles
  return(x)
}
data.complete[num_vars] <- apply(data.complete[num_vars],2,FUN = out_treat)

out_treat1 <- function(x) {
  quantiles <- quantile(x,0.99, na.rm = T)
  x[x > quantiles] = quantiles
  return(x)
}

data.complete[c(2:4,11,20,22:30,36:40)] <- apply(data.complete[c(2:4,11,20,22:30,36:40)],2, FUN = out_treat1)

out_treat2 <- function(x) {
  UC1 <- mean(x, na.rm = T) + 3*sqrt(var(x, na.rm = T))
    x[x > UC1] = UC1
    return(x)
}

data.complete[c(31:35)] <- apply(data.complete[c(31:35)],2, FUN = out_treat2)

my_num_vars <- t(data.frame(apply(data.complete[num_vars],2,FUN = var_summary)))
View(my_num_vars)

# outlier treatment for categorical variables
View(my_other_vars)

data.complete[data.complete$PERFORM_CNS.SCORE.DESCRIPTION=='Not Scored: More than 50 active Accounts found', "PERFORM_CNS.SCORE.DESCRIPTION"] <- "No Bureau History Available"
table(data.complete$PERFORM_CNS.SCORE.DESCRIPTION)

table(data.complete$manufacturer_id)
data.complete[data.complete$manufacturer_id=='155', "manufacturer_id"] <- '152'
data.complete[data.complete$manufacturer_id=='156', "manufacturer_id"] <- '152'

table(data.complete$Employment.Type)
data.complete[data.complete$Employment.Type=='', "Employment.Type"] <- 'Salaried'

my_other_vars <- apply(data.complete[other_vars],2,FUN = var_summary)

# creating dummy variables

data.complete$PERFORM_CNS.SCORE.DESCRIPTION <- factor(data.complete$PERFORM_CNS.SCORE.DESCRIPTION,levels = c('A-Very Low Risk','B-Very Low Risk','C-Very Low Risk',
          'D-Very Low Risk','E-Low Risk','F-Low Risk','G-Low Risk','H-Medium Risk','I-Medium Risk','J-High Risk','K-High Risk',
          'L-Very High Risk','M-Very High Risk','No Bureau History Available','Not Scored: No Activity seen on the customer (Inactive)',
          'Not Scored: No Updates available in last 36 months','Not Scored: Not Enough Info available on the customer',
          'Not Scored: Only a Guarantor','Not Scored: Sufficient History Not Available'),labels = c(1:19))

data.complete$Employment.Type <- factor(data.complete$Employment.Type,levels = c('Salaried','Self employed'),labels = c(1,2))

# rearranging columns
#3,4,5,7,10,11,16,17,19,20,21,23,24,25,26,34,38,39,

data.complete1 <- data.complete[,c(3,4,5,7,10,11,16,17,19,20,21,23,24,25,26,34,38,39,1,2,6,8,9,12,13,14,15,18,22,27,28,29,30,31,32,33,
                                            35,36,37,40,41)]

# assumptions check for normality and linearity
hist(log(data.complete1$disbursed_amount))
hist(log(data.complete1$asset_cost)) 

# splititng the data back into training and testing

train.treat <- data.complete1[!is.na(data.complete1$loan_default),] 
test.treat <- data.complete1[is.na(data.complete1$loan_default),]
test.treat <- test.treat[-41]

old_num <-  sapply(train.treat, is.numeric)
old_others <- !sapply(train.treat, is.numeric)
loan_default <- train.treat$loan_default
train.treat.num <- train.treat[old_num]
train.treat.num <- cbind(train.treat.num, loan_default)

# variable reduction
# Anova to check the significance of numerical variables - tranform loan_default to numeric to perform anova
train.treat.num$loan_default <- as.numeric(train.treat.num$loan_default)
fit <- aov(loan_default ~., data = train.treat.num)
summary(fit)

# numeric varibable to drop - PRI.DISBURSED.AMOUNT, SEC.ACTIVE.ACCTS, SEC.SANCTIONED.AMOUNT, SEC.DISBURSED.AMOUNT, PRIMARY.INSTAL.AMT, SEC.INSTAL.AMT
# NEW.ACCTS.IN.LAST.SIX.MONTHS, SEC.OVERDUE.ACCTS

# cor_metrics to check auto and cross correlation between numeric variables
cor_metrics <- cor(train.treat.num[c(-15,-26)])
View(cor_metrics)
write.csv(cor_metrics, file = "cor_metrics.csv")

# factor analysis to check for highly correlated variables

require(psych)
require(GPArotation)

scree(cor_metrics, factors = T, pc = T, main = "ScreePlot", hline = NULL, add = F)
eigen(cor_metrics)$values
require(dplyr)

eigen_values <- mutate(data.frame(eigen(cor_metrics)$values),
                       cum_sum_eigen=cumsum(eigen.cor_metrics..values),
                       pct_var=eigen.cor_metrics..values/sum(eigen.cor_metrics..values),
                       cum_pct_var=cum_sum_eigen/sum(eigen.cor_metrics..values))

View(eigen_values)

FA <- fa(r=cor_metrics, nfactors = 12, rotate = "varimax", fm="ml")
print(FA)
FA_sort <- fa.sort(FA)
ls(FA_sort)
FA_sort$loadings
Loadings <- data.frame(FA_sort$loadings[1:ncol(train.treat.num[c(-15,-26)]),])
View(Loadings)
write.csv(Loadings, file = "factLoadings.csv")

# significant numerical variables after factor analysis - SEC.DISBURSED.AMOUNT, SEC.SANCTIONED.AMOUNT, PRI.DISBURSED.AMOUNT, PRI.SANCTIONED.AMOUNT,
# AVERAGE.ACCT.AGE, CREDIT.HISTORY.LENGTH, NEW.ACCTS.IN.LAST.SIX.MONTHS, PRI.ACTIVE.ACCTS, asset_cost, ltv, PRI.OVERDUE.ACCTS, SEC.NO.OF.ACCTS,
# PRIMARY.INSTAL.AMT

# checking significance of categorical variables
# drop categorical variables due to high number of levels - supplier_id, current pincode id, date of birth, employee code id

train.treat.others <- train.treat[old_others]
chisq.test(train.treat.others$loan_default, train.treat.others$branch_id)
chisq.test(train.treat.others$loan_default, train.treat.others$manufacturer_id)
chisq.test(train.treat.others$loan_default, train.treat.others$Employment.Type)
chisq.test(train.treat.others$loan_default, train.treat.others$State_ID)
chisq.test(train.treat.others$loan_default, train.treat.others$MobileNo_Avl_Flag)
chisq.test(train.treat.others$loan_default, train.treat.others$Aadhar_flag)
chisq.test(train.treat.others$loan_default, train.treat.others$PAN_flag)
chisq.test(train.treat.others$loan_default, train.treat.others$VoterID_flag)
chisq.test(train.treat.others$loan_default, train.treat.others$Driving_flag)
chisq.test(train.treat.others$loan_default, train.treat.others$Passport_flag)
chisq.test(train.treat.others$loan_default, train.treat.others$PERFORM_CNS.SCORE.DESCRIPTION)

# significant cat variables - branch_id, manufacturer_id*, Employment.Type, State_ID, Aadhar_flag, VoterID_flag, PERFORM_CNS.SCORE.DESCRIPTION
# can be considered - Driving_flag, Passport_flag PAN_flag,

# splitting the training data into training and validation
require(caret)

set.seed(123)
indexes <- createDataPartition(train.treat$loan_default, times = 1, p = 0.7, list = FALSE)
train.final <- train.treat[indexes,]
valid.final <- train.treat[-indexes,]

# Model building using logsitic regression

fit <- glm(loan_default ~ asset_cost + ltv + branch_id + manufacturer_id + Employment.Type + PAN_flag + VoterID_flag + 
             PERFORM_CNS.SCORE.DESCRIPTION + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + PRI.SANCTIONED.AMOUNT + 
             PRI.CURRENT.BALANCE + PRIMARY.INSTAL.AMT + AVERAGE.ACCT.AGE + CREDIT.HISTORY.LENGTH + Passport_flag + PERFORM_CNS.SCORE + DisbursalDate 
, data = train.final, family = binomial("logit"))
summary(fit)
ls(fit)

library(neuralnet)

fit <- neuralnet(loan_default ~ asset_cost + ltv + branch_id + manufacturer_id + Employment.Type + PAN_flag + VoterID_flag + 
                   PERFORM_CNS.SCORE.DESCRIPTION + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + PRI.SANCTIONED.AMOUNT + 
                   PRI.CURRENT.BALANCE + PRIMARY.INSTAL.AMT + AVERAGE.ACCT.AGE + CREDIT.HISTORY.LENGTH + Passport_flag + PERFORM_CNS.SCORE + DisbursalDate,
                   data = train.final, hidden = 4, err.fct = "sse", linear.output = FALSE, learningrate = 0.01, threshold = 0.01)


# drop variables with high p values - State_ID PRI.DISBURSED.AMOUNT  SEC.NO.OF.ACCTS  SEC.DISBURSED.AMOUNT NEW.ACCTS.IN.LAST.SIX.MONTHS Aadhar_flag
# added PRI.CURRENT.BALANCE Passport_flag PERFORM_CNS.SCORE DisbursalDate

# evaluating logisitc regression output
#install.packages("InformationValue")
require(InformationValue)

train.final1 <- cbind(train.final, pred_loan_default = predict(fit, type = "response"))
Concordance(train.final1$loan_default, train.final1$pred_loan_default)

cut1 <- optimalCutoff(train.final1$loan_default, train.final1$pred_loan_default, optimiseFor = "Both", returnDiagnostics = T)
confusionMatrix(train.final1$loan_default, train.final1$pred_loan_default, threshold = 0.2063)

plotROC(train.final1$loan_default, train.final1$pred_loan_default, Show.labels = F)

# validation with testing data
valid.final1 <- cbind(valid.final, pred_loan_default = predict(fit, newdata = valid.final, type = "response"))
cut1 <- optimalCutoff(valid.final1$loan_default, valid.final1$pred_loan_default, optimiseFor = "Both", returnDiagnostics = T)
confusionMatrix(valid.final1$loan_default, valid.final1$pred_loan_default, threshold = 0.2063)

plotROC(valid.final1$loan_default, valid.final1$pred_loan_default, Show.labels = F)

# predicitng values for testing dataset

test.final1 <- cbind(test.treat, loan_default = predict(fit, newdata = test.treat, type = "response"))
output <- test.final1[c(1,41)]

write.csv(output, file = "output.csv", row.names = F)

# using randomforest

install.packages("adabag")
require(adabag)
require(doSNOW)

cl <- makeCluster(5, type = "SOCK")
registerDoSNOW(cl)

fit <- boosting(loan_default ~ asset_cost + ltv + branch_id + manufacturer_id + Employment.Type + PAN_flag + VoterID_flag + 
             PERFORM_CNS.SCORE.DESCRIPTION + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + PRI.SANCTIONED.AMOUNT + PRI.CURRENT.BALANCE + 
             PRIMARY.INSTAL.AMT + AVERAGE.ACCT.AGE + CREDIT.HISTORY.LENGTH + Passport_flag + PERFORM_CNS.SCORE + DisbursalDate,
             data = train.final, boos = T, mfinal = 50, coeflearn = 'Breiman' )

summary(fit)

stopCluster(cl)

# using H20 package

#install.packages("h2o")
require(h2o)

cl <- makeCluster(5, type = "SOCK")
h2o.init(
  nthreads = -1,
  max_mem_size = "16G")
h2o.removeAll()

write.csv(train.final, file = "train.final.csv")
write.csv(valid.final, file = "valid.final.csv")
write.csv(test.treat, file = "test.treat.csv")

train1 <- h2o.importFile(path = normalizePath("train.final.csv"))
valid1 <- h2o.importFile(path = normalizePath("valid.final.csv"))
test1 <- h2o.importFile(path = normalizePath("test.treat.csv"))

train1 <- h2o.assign(train1, "train.hex")
valid1 <- h2o.assign(valid1, "valid.hex")
test1 <- h2o.assign(test1, "test.hex")

rf1 <- h2o.randomForest(         
  training_frame = train1,        
  validation_frame = valid1,  
  x=1:18, 
  y=41,                           
  model_id = "rf_covType_v1",   
  ntrees = 350,                 
  max_depth = 25,               
  stopping_rounds = 20,          
  stopping_tolerance = 0.00001,    
  score_each_iteration = T,     
  seed=7777777)  

stopCluster(cl)

summary(rf1)

# performance evaluation
rf1@model$validation_metrics

loan_default <- h2o.predict(
  object = rf1,
  newdata = test1)

output <- as.data.frame(loan_default)
output <- cbind(test.treat,output)
names(output)[41] <- "loan_default"
result_output <- output[c(19,41)]


write.csv(result_output, file = "output.csv", row.names = F)

library(neuralnet)

fit <- neuralnet(is_chat ~ ef3 + ef4 + ef5 + ef6 + ef8 + ef10  + ef11 + ef13, 
                 data = training_data, hidden = 4, err.fct = "sse", linear.output = FALSE, learningrate = 0.01, threshold = 0.01)



