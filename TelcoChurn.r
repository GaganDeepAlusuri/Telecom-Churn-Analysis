rm(list=ls())
library(rio)
library(dplyr)
library(ggplot2)
library(lattice)
library(MASS)
library(boot)
library(e1071)
d = import("TelcoChurn.xlsx", sheet= "Data")
colnames(d)=tolower(make.names(colnames(d)))   
str(d)

#feature engineering
#Converting gender to 1/0
d$gender[d$gender == "Male"] <- 1 
d$gender[d$gender == "Female"] <- 0 
#Converting Partner to 1/0
d$partner[d$partner == "Yes"] <- 1 
d$partner[d$partner == "No"] <- 0
#Converting dependents to 1/0
d$dependents[d$dependents == "Yes"] <- 1 
d$dependents[d$dependents == "No"] <- 0
#Converting phoneservice to 1/0
d$phoneservice[d$phoneservice == "Yes"] <- 1 
d$phoneservice[d$phoneservice == "No"] <- 0
#Converting paperlessbilling to 1/0
d$paperlessbilling[d$paperlessbilling == "Yes"] <- 1 
d$paperlessbilling[d$paperlessbilling == "No"] <- 0 
#Converting churn to 1/0
d$churn[d$churn == "Yes"] <- 1      
d$churn[d$churn == "No"] <- 0 

#Converting to "Yes"/"No"
d$multiplelines[d$multiplelines == "No phone service"] <- "No"
d$onlinesecurity [d$onlinesecurity == "No internet service"] <- "No"
d$onlinebackup [d$onlinebackup == "No internet service"] <- "No"
d$deviceprotection[d$deviceprotection == "No internet service"] <- "No"
d$techsupport[d$techsupport == "No internet service"] <- "No"
d$streamingtv[d$streamingtv == "No internet service"] <- "No"
d$streamingmovies[d$streamingmovies == "No internet service"] <- "No"

#Converting to 1/0
d$multiplelines <- ifelse(d$multiplelines=='Yes', 1,0)
d$onlinesecurity<- ifelse(d$onlinesecurity=='Yes', 1,0)
d$onlinebackup<- ifelse(d$onlinebackup=='Yes', 1,0)
d$deviceprotection<- ifelse(d$deviceprotection=='Yes', 1,0)
d$techsupport<- ifelse(d$techsupport=='Yes', 1,0)
d$streamingtv<- ifelse(d$streamingtv=='Yes', 1,0)
d$streamingmovies<- ifelse(d$streamingmovies=='Yes', 1,0)

#Converting necessary variables to factors
d$internetservice <- as.factor(d$internetservice)
#d$contract <- as.factor(d$contract)
d$paymentmethod <- as.factor(d$paymentmethod)
d$gender <- as.factor(d$gender)
#d$seniorcitizen <- as.factor(d$seniorcitizen)
#d$partner <- as.factor(d$partner)
#d$dependents <- as.factor(d$dependents)
#d$techsupport <- as.factor(d$techsupport)
#d$multiplelines <- as.factor(d$multiplelines)
d$streamingtv <- as.factor(d$streamingtv)
#d$streamingmovies <- as.factor(d$streamingmovies)
#d$paperlessbilling <- as.factor(d$paperlessbilling)
d$phoneservice <- as.factor(d$phoneservice)
colSums(is.na(d))  # 11 nulls in total charges.                              

#Treating nulls in totalcharges
df <- d %>% mutate(totalcharges = ifelse(is.na(d$totalcharges), d$monthlycharges*d$tenure, totalcharges) )
colSums(is.na(df))  

#EDA
table(df$churn)
table(df$churn,df$phoneservice)
table(df$churn,df$internetservice)
bwplot(tenure ~ factor(churn), data=df,main="Tenure of churn Vs Non-Churn")
bwplot(monthlycharges ~ factor(churn), data=df, main="Monthly charges of churn Vs Non-Churn")
d_churn <- d %>% count(churn)
ggplot(d_churn, aes(x = churn, y = n, fill = churn)) +
  geom_bar(stat = "identity") +
  labs(x = "Churn", y = "Count", title = "Churn distribution")


# Corr test
library("PerformanceAnalytics")
df_temp <- df[, c(6,19,20)]
chart.Correlation(df_temp)  #High correlation between tenure and totalcharges.                 

df$churn <- as.numeric(df$churn)
telephone_customers <- df[df$phoneservice == 1 & df$internetservice == "No", ]
internet_customers <- df[df$phoneservice == 0 & df$internetservice %in% c("DSL", "Fiber optic"), ]
internet_customers$internetservice <- ifelse(internet_customers$internetservice=='DSL', 1,0)
both_customers <- df[df$phoneservice == 1 & df$internetservice %in% c("DSL", "Fiber optic"), ]

#Building the models
linear <- lm(churn ~ seniorcitizen + partner + dependents + multiplelines +
               techsupport + contract + streamingmovies + paperlessbilling +
               monthlycharges, data=df)
summary(linear)                              
plot(linear)  # Fails linearity, normality and homoskedasticity

#Logit models without train/test split
logit1 <- glm(churn ~ seniorcitizen + partner + dependents +
                tenure + paperlessbilling + multiplelines +
                monthlycharges
                ,family = binomial(link = "logit"), data = telephone_customers)
summary(logit1)

logit2 <- glm(churn ~ seniorcitizen + partner + dependents  + tenure + internetservice + techsupport +
                contract + monthlycharges + onlinesecurity + onlinebackup + deviceprotection 
                , family = binomial(link = "logit"), data = internet_customers)
summary(logit2)

logit3 <- glm(churn ~ seniorcitizen + partner + dependents + tenure +
                techsupport + internetservice+ multiplelines + 
              + contract + paperlessbilling + monthlycharges+ onlinesecurity + onlinebackup
              + deviceprotection + streamingmovies + streamingtv, family = binomial(link = "logit"), data = both_customers) 
summary(logit3)

library(stargazer)
stargazer(df, title="Descriptive Statistics", type="text")
stargazer(logit1, logit2, logit3, title="Churn Logit Models", type="text", single.row=TRUE)

AIC(logit1,logit2,logit3)
BIC(logit1, logit2, logit3)

#comparing models
logit1$coef
exp(logit1$coef) 

logit2$coef
exp(logit2$coef) 

logit3$coef
exp(logit3$coef) 

library(lmtest) 
lrtest(logit1) #The test has found that Model 1 (i.e., logit1) fits significantly better than Model 2
lrtest(logit2) #Model 1 fits significantly better than Model 2
lrtest(logit3) #Model 1 fits significantly better than Model 2

####### Creating Train and Test Sets ################
# Set the random seed
set.seed(1024)

# Split the data into training and test sets
telephone_train <- telephone_customers[sample(nrow(telephone_customers), nrow(telephone_customers)*0.75), ]
telephone_test <- telephone_customers[setdiff(1:nrow(telephone_customers), rownames(telephone_train)), ]

internet_train <- internet_customers[sample(nrow(internet_customers), nrow(internet_customers)*0.75), ]
internet_test <- internet_customers[setdiff(1:nrow(internet_customers), rownames(internet_train)), ]

both_train <- both_customers[sample(nrow(both_customers), nrow(both_customers)*0.75), ]
both_test <- both_customers[setdiff(1:nrow(both_customers), rownames(both_train)), ]

# Train the logit models using the training data
logit1_train <- glm(churn ~ factor(seniorcitizen) + partner + dependents +
                      tenure + paperlessbilling + multiplelines +
                      monthlycharges
                    ,family = binomial(link = "logit"), data = telephone_train)

logit2_train <- glm(churn ~ seniorcitizen + partner + dependents  + tenure + internetservice + techsupport +
                      contract + monthlycharges + onlinesecurity + onlinebackup + deviceprotection 
                    , family = binomial(link = "logit"), data = internet_train)

logit3_train <- glm(churn ~ seniorcitizen + partner + dependents + tenure +
                      techsupport + internetservice+ multiplelines + 
                      + contract + paperlessbilling + monthlycharges+ onlinesecurity + onlinebackup
                    + deviceprotection + streamingmovies + streamingtv, family = binomial(link = "logit"), data = both_train) 

#Combing the output of all three trained models
stargazer(logit1_train, logit2_train, logit3_train, title="Trained Churn Logit Models", type="text", single.row=TRUE)

# Testing on the test data

# Predict on the test data and evaluate the models
logit1_pred <- predict(logit1_train, newdata = telephone_test, type = "response")
logit1_pred_class <- ifelse(logit1_pred > 0.4, 1, 0)

logit2_pred <- predict(logit2_train, newdata = internet_test, type = "response")
logit2_pred_class <- ifelse(logit2_pred > 0.5, 1, 0)

logit3_pred <- predict(logit3_train, newdata = both_test, type = "response")
logit3_pred_class <- ifelse(logit3_pred > 0.5, 1, 0)

# Confusion matrices
table(logit1_pred_class, telephone_test$churn)
table(logit2_pred_class, internet_test$churn)
table(logit3_pred_class, both_test$churn)

# Classification error
mean(logit1_pred_class != telephone_test$churn)
mean(logit2_pred_class != internet_test$churn)
mean(logit3_pred_class != both_test$churn)

# Accuracy rate
1 - mean(logit1_pred_class != telephone_test$churn)
1 - mean(logit2_pred_class != internet_test$churn)
1 - mean(logit3_pred_class != both_test$churn)


# Function to calculate recall
recall <- function(actual, predicted){
  tp <- sum(actual == 1 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)
  recall_score <- tp / (tp + fn)
  return(recall_score)
}

# Function to calculate precision
precision <- function(actual, predicted){
  tp <- sum(actual == 1 & predicted == 1)
  fp <- sum(actual == 0 & predicted == 1)
  precision_score <- tp / (tp + fp)
  return(precision_score)
}

# Function to calculate F1-score
f1_score <- function(actual, predicted){
  recall_value <- recall(actual, predicted)
  precision_value <- precision(actual, predicted)
  f1_score_value <- 2 * ((precision_value * recall_value) / (precision_value + recall_value))
  return(f1_score_value)
}

# Function to calculate AUC
auc_score <- function(actual, predicted){
  library(pROC)
  roc_curve <- roc(actual, predicted)
  auc_score_value <- auc(roc_curve)
  return(auc_score_value)
}

# Calculate evaluation metrics for each model
recall1 <- recall(telephone_test$churn, logit1_pred_class)
precision1 <- precision(telephone_test$churn, logit1_pred_class)
f1_score1 <- f1_score(telephone_test$churn, logit1_pred_class)
auc_score1 <- auc_score(telephone_test$churn, logit1_pred)

recall2 <- recall(internet_test$churn, logit2_pred_class)
precision2 <- precision(internet_test$churn, logit2_pred_class)
f1_score2 <- f1_score(internet_test$churn, logit2_pred_class)
auc_score2 <- auc_score(internet_test$churn, logit2_pred)

recall3 <- recall(both_test$churn, logit3_pred_class)
precision3 <- precision(both_test$churn, logit3_pred_class)
f1_score3 <- f1_score(both_test$churn, logit3_pred_class)
auc_score3 <- auc_score(both_test$churn, logit3_pred)

# Create a table with the evaluation metrics
metrics_table <- data.frame(Model = c("Model 1", "Model 2", "Model 3"),
                            Recall = c(recall1, recall2, recall3),
                            Precision = c(precision1, precision2, precision3),
                            F1_Score = c(f1_score1, f1_score2, f1_score3),
                            AUC = c(auc_score1, auc_score2, auc_score3))
accuracy <- c(0.9297297, 0.811617, 0.7726687) #From the above calculations
metrics_table$Accuracy <- accuracy
print(metrics_table)
