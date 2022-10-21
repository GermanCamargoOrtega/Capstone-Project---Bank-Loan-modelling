#t itle: "Bank Loan modelling"
# author: "Germán Camargo Ortega"
# date: '2022-12-21'


#############################################################
## 1. Load libraries and data set, and check the structure ##
#############################################################


### 1.1 Libraries for data analysis and visualization

# install packeges if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org") 
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org") 
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org") 
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org") 
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org") 
if(!require(performanceEstimation)) install.packages("performanceEstimation", repos = "http://cran.us.r-project.org") 
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org") 
if(!require(smotefamily)) install.packages("smotefamily", repos = "http://cran.us.r-project.org") 
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") 
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org") 


# load libraries
library(tidyverse)
library(caret)
library(data.table)
library(janitor)
library(knitr)
library(DataExplorer)
library(ggpubr)
library(GGally)
library(performanceEstimation)
library(ROSE)
library(smotefamily)
library(readxl)
library(rpart.plot)


### 1.2 Load the data set

# load the data
Loan_Modelling  <- read.csv(
  "https://raw.githubusercontent.com/GermanCamargoOrtega/edxProject/main/BankLoan.csv"
)



# create a copy to leave original unchanged
df <- Loan_Modelling

# clean column names
df <- clean_names(df)

set.seed(1, sample.kind="Rounding")



### 1.3 Check the structure of the data.

# check data characteristics
head(df, 10) 

# further check data characteristics
glimpse(df) 
t(introduce(df))
plot_intro(df) 

# check the number of unique values fr each variable
map_df(df, n_distinct) %>% pivot_longer(everything(), 
                                        names_to = "variables", 
                                        values_to = "unique values") %>% kable()

# calculate the number of missing values (another way of chicking this)
map_df(df, function(x) sum(length(which(is.na(x))))) %>% 
  pivot_longer(everything(), 
               names_to = "variables", 
               values_to = "missing values") %>% kable()

# statistical summary
t(as.data.frame(apply(df, 2, summary)))

# check which is the smallest ZIP code and identify the row number
df$zip_code %>% min()
df$zip_code[385]
slice_min(df, zip_code, n=2) %>% select("zip_code")

# drop column ID
df <- select(df, -id)

# adjust the zip code in row 385
df$zip_code[385] <- 93007



# Transform columns "personal_loan", "securities_account", "cd_account", "online", "credit_card" (i.e. columns 10 to 14) into factors.
df[,9:13] <- map(df[,9:13], as_factor)

#############################################################
## 2. EDA of the data set                                  ##
#############################################################

# barplots for bimodal variables
plot_bar(df)

# histograms for multimodal variables
plot_histogram(df)

# plot a correlation matrix (Pearson)
plot_correlation(na.omit(df), geom_text_arg = c(size = 2))


# Violin plots
# a) Personal_Loan vs. CCAvg vs. CD_Account vs. Education
ggplot(df, aes(x = personal_loan, y = cc_avg)) + 
  geom_violin(aes(fill = cd_account), trim = FALSE, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = cd_account), width = 0.15,position = position_dodge(0.9)) +
  facet_wrap(~education)

# b) Personal_Loan vs. Income vs. Securities_Account vs. Education
ggplot(df, aes(x = personal_loan, y = income)) + 
  geom_violin(aes(fill = securities_account), trim = FALSE, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = securities_account), width = 0.15,position = position_dodge(0.9)) +
  facet_wrap(~education)


# c) Personal_Loan vs. Age vs. CreditCard vs. Family
ggplot(df, aes(x = personal_loan, y = age)) + 
  geom_violin(aes(fill = credit_card), trim = FALSE, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = credit_card), width = 0.15,position = position_dodge(0.9)) +
  facet_wrap(~family)


# d) Personal_Loan vs. ZIPCode
ggplot(df, aes(x = personal_loan, y = zip_code)) + 
  geom_violin(aes(fill = personal_loan), trim = T, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = personal_loan), width = 0.15,position = position_dodge(0.9)) 


# e) Personal_Loan vs. Experience
ggplot(df, aes(x = personal_loan, y = income)) + 
  geom_violin(aes(fill = factor(education)), trim = FALSE, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = factor(education)), width = 0.15,position = position_dodge(0.9)) 



#############################################################
## 3. Data pre-processing                                  ##
#############################################################

# QQ plot for skewed data
qq_data <- df[, c("cc_avg", "income", "mortgage")]
plot_qq(qq_data)

# QQ plot for log-transformed  data
log_qq_data <- update_columns(qq_data, 1:3, function(x) log(x + 1))
plot_qq(log_qq_data)

# Histograms of log-transformed  data
plot_histogram(log_qq_data)

# Drop columns ID, Age, Experience, ZIPCode
df <- select(df, - c(age, experience, zip_code))

# Log transform CCAvg, Income, Mortgage
df <- update_columns(df, c(1, 3, 5), function(x) log(x + 1))
head(df)



#############################################################
## 4. Model development                                    ##
#############################################################

### 4.1 Split data into training and test sets

# Split data into 30% test/ 70% training
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = df$credit_card, times = 1, p = 0.3, list = FALSE)

# Subset test data by test_index (all raws in test_index, and all columns)
test_data <- df[test_index, ]

# Subset training data by test_index (rest of raws, and all columns)
train_data <- df[-test_index, ]

# Remove test_index
rm(test_index)

# check the data distribution in training and test set
ggplot(train_data, aes(x=credit_card)) + geom_bar(aes(y = ..count.., group = 1))
ggplot(test_data, aes(x=credit_card)) + geom_bar(aes(y = ..count.., group = 1))


### 4.2 Treat data imbalance

# compare to the original data
table(train_data$credit_card) 

# down-sampling 
set.seed(1, sample.kind="Rounding")
down_train <- downSample(x = train_data[, -ncol(train_data)], y = factor(train_data$credit_card))
down_train <- rename(down_train, credit_card = Class)
table(down_train$credit_card)   

# up-sampling 
set.seed(1, sample.kind="Rounding")
up_train <- upSample(x = train_data[, -ncol(train_data)], y = factor(train_data$credit_card))      
up_train <- rename(up_train, credit_card = Class)
table(up_train$credit_card) 

# SMOTE 
set.seed(1, sample.kind="Rounding")
smote_train <- performanceEstimation::smote(credit_card ~ ., data  = train_data, 
                                            perc.over = 0.5, perc.under = 2)
#smote_train$credit_card <- as.numeric(smote_train$credit_card)
table(smote_train$credit_card) 

# ROSE 
set.seed(1, sample.kind="Rounding")
rose_train <- ROSE(credit_card ~ ., data  = train_data)$data                  
table(rose_train$credit_card) 


### 4.3 Build the logistic regression models

# Model with unbalanced data
lr_model <- glm(credit_card ~ ., data = train_data, family = "binomial")
residuals_lr_model <- summary(residuals(lr_model))

# Model with down-sampling balancing
lr_model_downSampling <- glm(credit_card ~ ., data = down_train, family = "binomial")
residuals_lr_model_downSampling <- summary(residuals(lr_model_downSampling))

# Model with up-sampling balancing
lr_model_upSampling <- glm(credit_card ~ ., data = up_train, family = "binomial")
residuals_lr_model_upSampling <- summary(residuals(lr_model_upSampling))

# Model with SMOTE balancing
lr_model_SMOTE <- glm(credit_card ~ ., data = smote_train, family = "binomial")
residuals_lr_model_SMOTE <- summary(residuals(lr_model_SMOTE))

# Model with ROSE balancing
lr_model_ROSE <- glm(credit_card ~ ., data = rose_train, family = "binomial")
residuals_lr_model_ROSE <- summary(residuals(lr_model_ROSE))

# Compile and compare the residuals of each model
residual_models <- rbind(residuals_lr_model, residuals_lr_model_downSampling, 
                         residuals_lr_model_upSampling, residuals_lr_model_SMOTE, 
                         residuals_lr_model_ROSE)
knitr::kable(residual_models)
 

# Now let's check the performance as measured by recall in test data.
# Model with unbalanced data
fitted.results.lr_model <- ifelse(predict(lr_model, 
                                          newdata = test_data, 
                                          type = "response") > 0.5, 1, 0) %>% as_factor()

lr_model_recall <- recall(data = fitted.results.lr_model, 
                          reference = test_data$credit_card)

# Model with down-sampling balancing
fitted.results.lr_model_downSampling <- ifelse(predict(lr_model_downSampling, 
                                                       newdata = test_data, 
                                                       type = "response") > 0.5, 1, 0) %>% as_factor()

lr_model_downSampling_recall <- recall(data = fitted.results.lr_model_downSampling, 
                                       reference = test_data$credit_card)

# Model with up-sampling balancing
fitted.results.lr_model_upSampling <- ifelse(predict(lr_model_upSampling, 
                                                     newdata = test_data, 
                                                     type = "response") > 0.5, 1, 0) %>% as_factor()

lr_model_upSampling_recall <- recall(data = fitted.results.lr_model_upSampling, 
                                     reference = test_data$credit_card)

# Model with SMOTE balancing
fitted.results.lr_model_SMOTE <- ifelse(predict(lr_model_SMOTE, 
                                                newdata = test_data, 
                                                type = "response") > 0.5, 1, 0) %>% as_factor()

lr_model_SMOTE_recall <- recall(data = fitted.results.lr_model_SMOTE, 
                                reference = test_data$credit_card)

# Model with ROSE balancing
fitted.results.lr_model_ROSE <- ifelse(predict(lr_model_ROSE, 
                                               newdata = test_data, 
                                               type = "response") > 0.5, 1, 0) %>% as_factor()

lr_model_ROSE_recall <- recall(data = fitted.results.lr_model_ROSE, 
                               reference = test_data$credit_card)


recall_models <- rbind(lr_model_recall, lr_model_downSampling_recall, 
                       lr_model_upSampling_recall, lr_model_SMOTE_recall, 
                       lr_model_ROSE_recall)

knitr::kable(recall_models)


# Let's check the coefficients using imbalanced data.
# display the summary with coefficients
summary(lr_model)

# Generate the function to nicely draw the matrix.
draw_confusion_matrix <- function(cm) {

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 



# Let's generate and plot the confusion matrix using imbalanced data.
# generate the CM (lr_cm)
lr_cm <- confusionMatrix(
  fitted.results.lr_model,
  test_data$credit_card,
  positive = NULL,
  dnn = c("Prediction", "Reference"),
  prevalence = NULL,
  mode = "everything")

draw_confusion_matrix(lr_cm)


# Let's check the coefficients using balanced data via up-sampling.
# display the summary with coefficients
summary(lr_model_upSampling)


# Let's check the confusion matrix using balanced data via up-sampling.
# generate the CM (lr_cm)
lr_cm_upSampling <- confusionMatrix(
  fitted.results.lr_model_upSampling,
  test_data$credit_card,
  positive = NULL,
  dnn = c("Prediction", "Reference"),
  prevalence = NULL,
  mode = "everything")

draw_confusion_matrix(lr_cm_upSampling)


# Next I will compare to a decision tree classifier.

### 4.4 Build the decision tree classifier
# Check the classifier using default parameters with imbalanced data.
# 1. Set the seed
set.seed(1, sample.kind="Rounding")

# 2. Generate and fit the model with imbalanced data and default parameters
rt_model <- train(credit_card ~ ., data = train_data, method = "rpart")
fitted.results.rt_model <- predict(rt_model,test_data)

# 3. Generate and plot the confusion matrix
rt_model_cm <- confusionMatrix(
  fitted.results.rt_model,
  test_data$credit_card,
  positive = NULL,
  dnn = c("Prediction", "Reference"),
  prevalence = NULL,
  mode = "everything")

draw_confusion_matrix(rt_model_cm)

# 4. Plot the decision tree
rpart.plot(rt_model$finalModel, fallen.leaves = FALSE)



# Check a classifier with up-sampling-based balanced data, using default parameters.
# 1. Set the seed
set.seed(1, sample.kind="Rounding")

# 2. Generate and fit the model with imbalanced data and default parameters
rt_model_upSampling <- train(credit_card ~ ., data = up_train, method = "rpart")
fitted.results.rt_model_upSampling <- predict(rt_model_upSampling,test_data)

# 3. Generate and plot the confusion matrix
rt_model_cm_upSampling <- confusionMatrix(
  fitted.results.rt_model_upSampling,
  test_data$credit_card,
  positive = NULL,
  dnn = c("Prediction", "Reference"),
  prevalence = NULL,
  mode = "everything")

draw_confusion_matrix(rt_model_cm_upSampling)

# 4. Plot the decision tree
rpart.plot(rt_model_upSampling$finalModel, fallen.leaves = FALSE)



# Check a classifier with up-sampling-based balanced data, using tunned parameters.
# 1. Set the seed
set.seed(1, sample.kind="Rounding")

# 2. Generate and fit the model with imbalanced data and default parameters
rt_model_upSampling_tunned <- train(credit_card ~ ., 
                                    data = up_train, 
                                    method = "rpart",
                                    trControl = trainControl(method  = "cv", number  = 5),
                                    metric="Accuracy", 
                                    maximize = T,
                                    tuneGrid = data.frame(cp = 0.05),
                                    tuneLength = 30)


fitted.results.rt_model_upSampling_tunned <- predict(rt_model_upSampling_tunned,test_data)

# 3. Generate and plot the confusion matrix
rt_model_cm_upSampling_tunned <- confusionMatrix(
  fitted.results.rt_model_upSampling_tunned,
  test_data$credit_card,
  positive = NULL,
  dnn = c("Prediction", "Reference"),
  prevalence = NULL,
  mode = "everything")

draw_confusion_matrix(rt_model_cm_upSampling_tunned)

# 4. Plot the decision tree
rpart.plot(rt_model_upSampling_tunned$finalModel, fallen.leaves = FALSE)
- It make sense of course that individuals with higher income and bigger family will more likely to borrow more from the bank (to sustain the family) as they have higher chances of paying back.