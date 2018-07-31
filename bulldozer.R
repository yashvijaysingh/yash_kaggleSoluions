################################### start ##############################################
rm(list = ls())
setwd("C:/Users/yash.singh/Desktop/Kaggle/Bull")

# extracting datasets
train <- read.csv(file = "Train.csv", header = T)
valid <- read.csv(file = "Valid.csv", header = T)
test <- read.csv(file = "Test.csv", header = T)

str(train)
# rearranging and merging train and valid data in one for data cleaning
valid$SalePrice <- NA

# rearranging columns in valid dataset
valid <- valid[c(1,53,2:52)]
data.complete <- rbind(train,valid)

##################################### summary of the data ####################################
var_summ <- function(x) {
  if(class(x)=="numeric" || class(x)=="integer") {
    var_type = class(x)
    n = length(x)
    nmiss = sum(is.na(x))
    nmiss_pct = (sum(is.na(x))/length(x))*100
    mean = mean(x, na.rm = T)
    median = median(x, na.rm = T)
    var = var(x, na.rm = T)
    sd = sqrt(var(x, na.rm = T))
    UC1 = mean(x, na.rm = T) + 3*sqrt(var(x, na.rm = T))
    LC1 = mean(x, na.rm = T) - 3*sqrt(var(x, na.rm = T))
    pctl = quantile(x,p=c(0,0.01,0.05,0.95,0.99,1), na.rm = T)
    return(c(var_type=var_type, n=n, nmiss=nmiss, nmiss_pct=nmiss_pct, mean=mean, median=median, var=var, sd=sd, UC1=UC1,
             LC1=LC1, pctl=pctl))
  }
  else{
    var_type = class(x)
    n = length(x)
    nmiss=sum(is.na(x))
    nmiss_pct = (sum(is.na(x))/length(x))*100
    nlevels = nlevels(x)
    freq =table(x)
    prop = prop.table(table(x))
    return(c(var_type=var_type, n=n, nmiss=nmiss, nmiss_pct=nmiss_pct, nlevels=nlevels, frequency=freq, proportion=prop))
  }
}

# converting year and sale date in date format 

data.complete$saledate = as.Date(as.character(data.complete$saledate), '%m/%d/%Y')
#data.complete$saledate <- format(as.Date(data.complete$saledate, format="%Y/%d/%m"),"%Y")

data.complete$YearMade = as.Date(as.character(data.complete$YearMade), '%Y')

data.complete$duration <- as.numeric(data.complete$saledate - data.complete$YearMade)

data.complete$auctioneerID <- as.factor(data.complete$auctioneerID)

data.complete$datasource <- as.factor(data.complete$datasource)

data.complete$ModelID <- as.factor(data.complete$ModelID)

data.complete$MachineID <- as.factor(data.complete$MachineID)

############# creating duration column in the test dataset

test$saledate = as.Date(as.character(test$saledate), '%m/%d/%Y')
#data.complete$saledate <- format(as.Date(data.complete$saledate, format="%Y/%d/%m"),"%Y")

test$YearMade = as.Date(as.character(test$YearMade), '%Y')

test$duration <- as.numeric(test$saledate - test$YearMade)

test$auctioneerID <- as.factor(test$auctioneerID)

test$datasource <- as.factor(test$datasource)

data.complete$ModelID <- as.factor(data.complete$ModelID)

data.complete$MachineID <- as.factor(data.complete$MachineID)

test[test$Enclosure =='', "Enclosure"] <- "OROPS"

write.csv(test, file = "test_clean.csv")

# remove year date and sale data as converted into duration
data.complete <- data.complete[c(-7,-10)]

num_vars <-  sapply(data.complete, is.numeric)
other_vars <- !sapply(data.complete, is.numeric)

my_num_data <- t(data.frame(apply(data.complete[num_vars],2,FUN = var_summ)))
my_cat_data <- apply(data.complete[other_vars],2,FUN = var_summ)

View(my_num_data)
View(my_cat_data)

# missings in cat vars
cat_miss <- colSums(is.na(data.complete[other_vars])| data.complete[other_vars]=='')/nrow(data.complete)*100
View(cat_miss)

###################### Missing value treatment for cat vars

data.complete[is.na(data.complete$auctioneerID), "auctioneerID"] <- 1
data.complete[data.complete$Enclosure =='', "Enclosure"] <- "OROPS"
data.complete[data.complete$Enclosure =='NO ROPS', "Enclosure"] <- "OROPS"
data.complete[data.complete$Enclosure =='None or Unspecified', "Enclosure"] <- "OROPS"
data.complete[is.na(data.complete$Hydraulics),"Hydraulics"] <- "2 Valve"
data.complete[data.complete$datasource == 173, "datasource"] <- 172
# data.complete[is.na(data.complete$Enclosure), "Enclosure"] <- which.max(prop.table(table(data.complete$Enclosure)))

table(data.complete$Enclosure)
#data.complete$ProductGroup <- factor(data.complete$ProductGroup, levels=c("BL","MG","SSL","TEX","TTT","WL"), labels=c(1:6))
# dropping varibales with more than 30 percent missings
# num_vars = SalesID, MachineHoursCurrentMeter
# cat_vars = MachineID UsageBand , fiSecondaryDesc, fiModelSeries, fiModelDescriptor, ProductSize, Drive_System, Forks, Pad_Type, Ride_Control,
# Stick, Transmission, Turbocharged, Blade_Extension, Blade_Width, Enclosure_Type, Engine_Horsepower, Pushblock, Ripper, Scarifier, Tip_Control,
# Tire_Size, Coupler, Coupler_System, Grouser_Tracks, Hydraulics_Flow, Track_Type, Undercarriage_Pad_Width, Stick_Length, Thumb, Pattern_Changer,
# Grouser_Type, Backhoe_Mounting, Blade_Type, Travel_Controls, Differential_Type, Steering_Controls
# date variables - YearMade, saledate

# varibles to be considered
# num_vars - SalePrice, duration
# cat_vars - ModelID, datasource, auctioneerID, fiModelDesc, fiBaseModel, fiProductClassDesc, state, ProductGroup, ProductGroupDesc, 
# Enclosure,Hydraulics 

colnames(data.complete)
data.cleaned <- data.complete[c(2,4:6,9:10,15:18,20,31,52)]
str(data.cleaned)
data.cleaned$ModelID <- as.numeric(data.cleaned$ModelID)

################## checking the lineraity of data #########################

hist(data.cleaned$SalePrice)
data.cleaned$ln_SalePrice <- log(data.cleaned$SalePrice)
hist(data.cleaned$ln_SalePrice)

# creating dummy varibles for ProductGroup

#install.packages('caret')
#install.packages("caret", repos="http://cran.rstudio.com/", dependencies=TRUE)
#require(caret)
#ls("package:caret")
#dv1 <- dummyVars(~ProductGroup, data = data.cleaned)
#dummy_ProductGroup <- data.frame(predict(dv1, data.cleaned))
#data.cleaned1 <- cbind(data.cleaned, Group_Bl= dummy_ProductGroup$ProductGroup.BL, Group_MG = dummy_ProductGroup$ProductGroup.MG,
#                       Group_SSL = dummy_ProductGroup$ProductGroup.SSL, Group_TEX = dummy_ProductGroup$ProductGroup.TEX,
#                       Group_TTT = dummy_ProductGroup$ProductGroup.TTT, Group_WL = dummy_ProductGroup$ProductGroup.WL)
#colnames(data.cleaned1)

################################## splitting data back into training set and validation set
set.seed(123)
train_clean <- data.cleaned[!is.na(data.cleaned$SalePrice),]
valid_clean <- data.cleaned[is.na(data.cleaned$SalePrice),]

write.csv(train_clean, file = "train_clean.csv")
write.csv(valid_clean, file = "valid_clean.csv")

####################################### Anova to check the significance of factor variables

Anova <- aov(ln_SalePrice ~ datasource + auctioneerID + fiProductClassDesc + state
             + ProductGroup + ProductGroupDesc + Enclosure + Hydraulics, data = train_clean)

summary(Anova)

# fiModelDesc + fiBaseModel - not checked as number of levels are too high
# model id - sqrt

# checking the significance of numeric vars
cor_metrics <- cor(train_clean[c(2,13,14)])
View(cor_metrics)

########################################### fitting a regression model

#fit <- lm(ln_SalePrice ~ModelID + datasource + auctioneerID + fiProductClassDesc +
#            state + Group_Bl + Group_MG + Group_SSL + Group_TEX + Group_TTT + Group_WL + ProductGroupDesc + Enclosure + Hydraulics + duration, data = train_clean)

fit <- lm(ln_SalePrice ~ ModelID + datasource + auctioneerID + fiProductClassDesc +
            state + Enclosure + Hydraulics + duration, data = train_clean)

#step1 <- step(fit)
summary(fit)

train_final <- cbind(train_clean, pred_ln_sales = predict(fit, newdata = train_clean),
                      pred_sales = exp(predict(fit, newdata = train_clean)))

valid_final <- cbind(valid_clean, pred_ln_sales = predict(fit, newdata = valid_clean),
                     pred_sales = exp(predict(fit, newdata = valid_clean)))

test_final <- cbind(test, pred_ln_sales = predict(fit, newdata = test),
                     pred_sales = exp(predict(fit, newdata = test)))

write.csv(test_final, file = "test_final.csv")
########################### Model verification by creating deciles

decLocations <- quantile(train_final$pred_ln_sales, probs = seq(0.1,0.9, by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
train_final$decile <- findInterval(train_final$pred_ln_sales,c(-Inf,decLocations, Inf))
View(train_final)

summary(train_final$decile)
xtabs(~decile,train_final)

# decile analysis report
require(sqldf)

train_final.DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pred_sales,
                        avg(SalePrice) as avg_actual_sales
                        from train_final
                        group by decile
                        order by decile desc")
View(train_final.DA)

# calculating RMSE for training set
sqrt(mean((train_final$ln_SalePrice - train_final$pred_ln_sales)**2))
# rmse = 0.44

##################################### using decision trees - raprt #####################
install.packages("rpart")
require(rpart)

fit <- rpart(ln_SalePrice ~ ModelID + datasource + auctioneerID + fiProductClassDesc + ProductGroup + ProductGroupDesc +
               state + Enclosure + Hydraulics + duration, method = "anova", data = train_clean)

summary(fit)

train_final <- cbind(train_clean, pred_ln_sales = predict(fit, newdata = train_clean),
                     pred_sales = exp(predict(fit, newdata = train_clean)))

test_final <- cbind(test, pred_ln_sales = predict(fit, newdata = test),
                    pred_sales = exp(predict(fit, newdata = test)))

write.csv(test_final, file = "test_final.csv")

sqrt(mean((train_final$ln_SalePrice - train_final$pred_ln_sales)**2))
# rmse  = 0.41

################################### Applying randomforest using H2o package #############################

## Create an H2O cloud 
library(h2o)
h2o.init(
  nthreads=-1,           
  max_mem_size = "2G")    
h2o.removeAll()           

## Load a file from disk
train_clean <- h2o.importFile(path = normalizePath("train_clean.csv"))
valid_clean <- h2o.importFile(path = normalizePath("valid_clean.csv"))
test_clean <- h2o.importFile(path = normalizePath("test_clean.csv"))

#Assignment within H2o
train_clean <- h2o.assign(train_clean, "train_clean.hex") 
valid_clean <- h2o.assign(valid_clean, "valid_clean.hex")     
test_clean <- h2o.assign(test_clean, "test_clean.hex") 

rf2 <- h2o.randomForest(        
  training_frame = train_clean,       
  validation_frame = valid_clean,     
  x= c(3,4,5,7,8,9,10,11,12,13,14),                       
  y=15,                         
  model_id = "rf_bull1",      
  ntrees = 500,                 
  max_depth = 15,               
  stopping_rounds = 15,          
  stopping_tolerance = 0.0001,    
  score_each_iteration = T,     
  seed=5555555)                 

#Performance Evaluation
summary(rf2)    
rf2@model$training_metrics

# RMSE =  0.34

pred_ln_sales<-h2o.predict(
  object = rf2,
  newdata = test_clean)

pred_ln_sales <- as.matrix(pred_ln_sales)
pred_sales <- exp(pred_ln_sales)
test_clean <- as.data.frame(test_clean)
test_clean$pred_ln_Sales <- pred_ln_sales
test_clean$pred_Sales <- pred_sales
test_final <- as.data.frame(valid_clean)
write.csv(test_clean, file = "test_final.csv")

h2o.shutdown(prompt=FALSE)

######################################## Thank-you ##################################