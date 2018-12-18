
rm(list=ls())
setwd("D:/Users/703188311/Desktop/DataScience")

# extract datasets
train <- read.csv(file = "train.csv", header = T)
test <- read.csv(file = "test.csv", header = T)
str(train)
test$num_orders <- NA

# Merging train and test data for data cleaning and data processing
data.complete <- rbind(train,test)
data.complete$emailer_for_promotion <- as.factor(data.complete$emailer_for_promotion)
data.complete$homepage_featured <- as.factor(data.complete$homepage_featured)
data.complete$center_id <- as.factor(data.complete$center_id)
data.complete$meal_id <- as.factor(data.complete$meal_id)
data.complete$checkout_price <- sqrt(data.complete$checkout_price)
data.complete$base_price <- sqrt(data.complete$base_price)

# Calculating summary stats of entire table
var_summ <- function(x) {
  if(class(x) == "numeric" || class(x) == "integer") {
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
    pctl = quantile(x, p=c(0,0.01,0.05,0.5,0.95,0.99,1), na.rm = T)
    return(c(var_type = var_type, n=n, nmiss=nmiss, nmiss_pct=nmiss_pct, mean=mean, median=median, var=var,
           sd=sd, UC1=UC1, LC1=LC1, pctl=pctl))
  }
  else{
    var_type = class(x)
    n = length(x)
    nmiss = sum(is.na(x))
    nmiss_pct = (sum(is.na(x))/length(x))*100
    nlevels = nlevels(x)
    freq = table(x)
    prop = prop.table(table(x))
    return(c(var_type=var_type, n=n, nmiss=nmiss, nmiss_pct=nmiss_pct, nlevels=nlevels, frequency = freq,
             proportion=prop))
  }
}

sapply(data.complete, FUN = is.numeric)
numeric_vars = names(data.complete)[sapply(data.complete, FUN = is.numeric)]
summary_stats = t(apply(data.complete[numeric_vars], 2, FUN = var_summ))
View(summary_stats)

# Treating outliers for checkout price and base price

out_treat <- function(x) {
  quantiles = quantile(x,0.01, na.rm = T)
  x[x < quantiles[1]] <- quantiles[1]
  x
}

data.complete$checkout_price <- out_treat(data.complete$checkout_price)
data.complete$base_price <- out_treat(data.complete$base_price)

out_treat1 <- function(x) {
  UC1 = mean(x, na.rm = T) + 3*sqrt(var(x, na.rm = T))
  x=ifelse(x>UC1, UC1, x)
  return(x)
}

out_treat1 <- function(x) {
  quantiles = quantile(x,0.99, na.rm = T)
  x[x > quantiles[1]] <- quantiles[1]
  x
}

data.complete$checkout_price <- out_treat1(data.complete$checkout_price)
data.complete$base_price <- out_treat1(data.complete$base_price)

# Checking summary stats after outlier treatment
summary_stats = t(apply(data.complete[numeric_vars], 2, FUN = var_summ))
View(summary_stats)

# Checking linerity

hist(log(data.complete$num_orders))

# Splitting data back to training and testing

train_clean <- data.complete[!is.na(data.complete$num_orders),]
valid_clean <- data.complete[is.na(data.complete$num_orders),]
write.csv(valid_clean, file = "valid_clean.csv", row.names = F)
train_clean$ln_num_orders <- (log(train_clean$num_orders))
hist(train_clean$ln_num_orders)
plot(train_clean$num_orders, train_clean$base_price)

cor_mat <- data.frame(cor(train_clean[c(2,5,6,9,10)]))
View(cor_mat)


# split the training data into train and valid
set.seed(123)
samp <- sample(1:nrow(train_clean), floor(nrow(train_clean)*0.7))
dev <- train_clean[samp,]
val <- train_clean[-samp,]

write.csv(dev, file = "dev.csv", row.names = F)
write.csv(val, file = "val.csv", row.names = F)

# develop the model
fit <- lm(ln_num_orders ~ center_id + meal_id + checkout_price  + emailer_for_promotion +
            homepage_featured, data = dev)
# dropping base-price due to multicollinearity

#fit <- lm(ln_num_orders ~ center_id + meal_id + checkout_price +
 #           homepage_featured, data = dev)

summary(fit)
pred_ln_num_orders = predict(fit, newdata = dev)
pred_num_orders = exp((pred_ln_num_orders))
View(pred_num_orders)
dev1 <- data.frame(cbind(dev,pred_num_orders))

val1 <- data.frame(cbind(val,pred_num_orders = exp((predict(fit, newdata = val)))))

#RMSE
sqrt(mean(dev1$num_orders - dev1$pred_num_orders)**2)
sqrt(mean(val1$num_orders - val1$pred_num_orders)**2)
valid_clean$num_orders <- exp(predict(fit, newdata = valid_clean))

output <- valid_clean[c(1,9)]
View(output)
write.csv(output,"output.csv", row.names = F)

# Decision tree usig rpart package

install.packages("rpart")
require(rpart)

fit <- rpart(ln_num_orders ~ center_id + meal_id + checkout_price + emailer_for_promotion +
            homepage_featured, data = dev)
summary(fit)
plot(fit)

pred_ln_num_orders = predict(fit, newdata = dev)
pred_num_orders = exp(pred_ln_num_orders)
View(pred_num_orders)
dev1 <- data.frame(cbind(dev,pred_num_orders))

val1 <- data.frame(cbind(val,pred_num_orders = exp(predict(fit, newdata = val))))

#RMSE
sqrt(mean(dev1$num_orders - dev1$pred_num_orders)**2)
sqrt(mean(val1$num_orders - val1$pred_num_orders)**2)
valid_clean$num_orders <- exp(predict(fit, newdata = valid_clean))

# Random Forest using h2o package

## Create an H2O cloud 
install.packages("h2o")
library(h2o)
h2o.init(
  nthreads=-1,           
  max_mem_size = "2G")    
h2o.removeAll()           

## Load a file from disk
dev <- h2o.importFile(path = normalizePath("dev.csv"))
val <- h2o.importFile(path = normalizePath("val.csv"))
valid_clean <- h2o.importFile(path = normalizePath("valid_clean.csv"))

#Assignment within H2o
dev <- h2o.assign(dev, "dev.hex") 
val <- h2o.assign(val, "val.hex")     
valid_clean <- h2o.assign(valid_clean, "valid_clean.hex") 

rf2 <- h2o.randomForest(        
  training_frame = dev,       
  validation_frame = val,     
  x= c(3,4,5,6,7,8),                       
  y=10,                         
  model_id = "rf_gen",      
  ntrees = 250,                 
  max_depth = 20,               
  stopping_rounds = 15,          
  stopping_tolerance = 0.0001,    
  score_each_iteration = T,     
  seed=25000000)                 

#Performance Evaluation
summary(rf2)    
rf2@model$training_metrics

ln_num_orders<-h2o.predict(
  object = rf2,
  newdata = valid_clean)

num_orders <- exp(ln_num_orders)
num_orders <- as.matrix(num_orders)
valid_clean <- as.data.frame(valid_clean)
valid_clean$num_orders <- num_orders
valid_clean <- as.data.frame(valid_clean)
write.csv(valid_clean[c(1,9)], file = "output.csv", row.names = F)

h2o.shutdown(prompt=FALSE)

# GBM using h2o package

library(h2o)
h2o.init(
  nthreads=-1,           
  max_mem_size = "3G")    
h2o.removeAll()           

## Load a file from disk
dev <- h2o.importFile(path = normalizePath("dev.csv"))
val <- h2o.importFile(path = normalizePath("val.csv"))
valid_clean <- h2o.importFile(path = normalizePath("valid_clean.csv"))

#Assignment within H2o
dev <- h2o.assign(dev, "dev.hex") 
val <- h2o.assign(val, "val.hex")     
valid_clean <- h2o.assign(valid_clean, "valid_clean.hex") 

gbm2 <- h2o.gbm(        
  training_frame = dev,       
  validation_frame = val,     
  x= c(3,4,5,6,7,8),                       
  y=10,                         
  model_id = "gbm_gen",      
  ntrees = 250,
  learn_rate = 0.75,
  max_depth = 20,
  sample_rate = 0.5,
  col_sample_rate = 0.85,
  stopping_rounds = 25,          
  stopping_tolerance = 0.0005,    
  score_each_iteration = T,     
  seed=25000000)                 

#Performance Evaluation
summary(gbm2)    
gbm2@model$training_metrics

ln_num_orders<-h2o.predict(
  object = gbm2,
  newdata = valid_clean)

num_orders <- exp(ln_num_orders)
num_orders <- as.matrix(num_orders)
valid_clean <- as.data.frame(valid_clean)
valid_clean$num_orders <- num_orders
valid_clean <- as.data.frame(valid_clean)
write.csv(valid_clean[c(1,9)], file = "output.csv", row.names = F)

h2o.shutdown(prompt=FALSE)

