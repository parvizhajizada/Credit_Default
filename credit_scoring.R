# Load necessary packages
library(readr)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(corrplot)
library(gridExtra)
library(GGally)
library(imputeMissings)
library(caret)
library(tibble)
library(corrplot)
library(AER)
library(lmtest)
library(nnet)
library(MASS)
library(verification)
library(janitor)
library(class)
library(kernlab)
library(verification)
library(tidyverse)
library(gmodels)
library(vcd)


# Load data
app <- read.csv("application.csv")
glimpse(app)
dim(app)

# Data Partitioning
set.seed(1)
which_train <- createDataPartition(app$TARGET, 
                                   p = 0.8, 
                                   list = FALSE) 

# Split data into training and test test 
train <- app[which_train,]
test <- app[-which_train,]

# Comparison of the distribution of the dependent variable in both samples
tabyl(train$TARGET) # looks fine
tabyl(test$TARGET) # looks fine

# EDA
table(app$TARGET) # Imbalanced class problem
ggplot(app, aes(x = TARGET)) +
  geom_histogram() 

# Missing values
data.frame(colSums(is.na(train)), colMeans(is.na(train))) %>% 
  filter(colSums.is.na.train.. != 0)

# Factorize categorical variables
facts <- c('SK_ID_CURR', 'CNT_CHILDREN', 'FLAG_MOBIL', 'FLAG_EMP_PHONE',
            'FLAG_WORK_PHONE', 'FLAG_CONT_MOBILE', 'FLAG_PHONE', 'FLAG_EMAIL',
            'CNT_FAM_MEMBERS', 'REGION_RATING_CLIENT', 'REGION_RATING_CLIENT_W_CITY',
            'HOUR_APPR_PROCESS_START', 'REG_REGION_NOT_LIVE_REGION', 'REG_REGION_NOT_WORK_REGION',
            'LIVE_REGION_NOT_WORK_REGION', 'REG_CITY_NOT_LIVE_CITY', 'REG_CITY_NOT_WORK_CITY',
            'LIVE_CITY_NOT_WORK_CITY', 'OBS_30_CNT_SOCIAL_CIRCLE', 'DEF_30_CNT_SOCIAL_CIRCLE',
            'OBS_60_CNT_SOCIAL_CIRCLE', 'DEF_60_CNT_SOCIAL_CIRCLE', 'FLAG_DOCUMENT_2',
            'FLAG_DOCUMENT_3','FLAG_DOCUMENT_4','FLAG_DOCUMENT_5','FLAG_DOCUMENT_6','FLAG_DOCUMENT_7',
            'FLAG_DOCUMENT_8','FLAG_DOCUMENT_9','FLAG_DOCUMENT_10','FLAG_DOCUMENT_11','FLAG_DOCUMENT_12',
            'FLAG_DOCUMENT_13','FLAG_DOCUMENT_14','FLAG_DOCUMENT_15','FLAG_DOCUMENT_16','FLAG_DOCUMENT_17',
            'FLAG_DOCUMENT_18','FLAG_DOCUMENT_19','FLAG_DOCUMENT_20','FLAG_DOCUMENT_21',
            'AMT_REQ_CREDIT_BUREAU_HOUR', 'AMT_REQ_CREDIT_BUREAU_DAY', 'AMT_REQ_CREDIT_BUREAU_WEEK',
            'AMT_REQ_CREDIT_BUREAU_MON', 'AMT_REQ_CREDIT_BUREAU_QRT', 'AMT_REQ_CREDIT_BUREAU_YEAR')

train[facts] <- lapply(train[facts], factor)
test[facts] <- lapply(test[facts], factor)


# Checking for any anomolies
summary(train['DAYS_BIRTH'] / -365)
summary(train['DAYS_EMPLOYED'])

ggplot(train, aes(x = DAYS_EMPLOYED)) +
  geom_histogram() +
  ggtitle("Days Employment Histogram") +
  xlab("Days Employment")

anom <- train[train['DAYS_EMPLOYED'] == 365243,]
non_anom <- train[train['DAYS_EMPLOYED'] != 365243,]

non_anom$TARGET %>% mean()
anom$TARGET %>% mean()
nrow(anom)

# Create an anomalous flag column
train['DAYS_EMPLOYED_ANOM'] <- train["DAYS_EMPLOYED"] == 365243
test['DAYS_EMPLOYED_ANOM'] <- test["DAYS_EMPLOYED"] == 365243

# Replace the anomalous values with nan
train$DAYS_EMPLOYED[train$DAYS_EMPLOYED == 365243] <- NaN
test$DAYS_EMPLOYED[test$DAYS_EMPLOYED == 365243] <- NaN

ggplot(train, aes(x = DAYS_EMPLOYED)) +
  geom_histogram() +
  ggtitle("Days Employment Histogram") +
  xlab("Days Employment")


# Find correlations with the target and sort
facts <- sapply(train, is.factor)
train.n <- train[,!facts]
correlations <- cor(train$TARGET, train.n, use="pairwise.complete.obs") %>% 
  data.frame(row.names = 'Correlation') %>% 
  sort() 

correlations[,1:15] %>% t()
correlations[,44:58] %>% t()


train['DAYS_BIRTH'] <- train['DAYS_BIRTH'] %>% abs()
test['DAYS_BIRTH'] <- test['DAYS_BIRTH'] %>% abs()
cor(train['DAYS_BIRTH'], train['TARGET'])

ggplot(train, aes(x = DAYS_BIRTH / -365)) +
  geom_histogram() +
  ggtitle("Age of Client") +
  xlab("Age (years)")

ggplot(train, aes(abs(DAYS_BIRTH / -365), colour = as.factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of Ages") +
  xlab("Age (years)")


# Create YEAR_BIRTH column which is age basically
train['YEARS_BIRTH'] <- train['DAYS_BIRTH'] / 365
test['YEARS_BIRTH'] <- test['DAYS_BIRTH'] / 365


# Bin the age data
train['YEARS_BINNED'] <- cut(train$YEARS_BIRTH, seq(0, 70, by = 5))
test['YEARS_BINNED'] <- cut(test$YEARS_BIRTH, seq(0, 70, by = 5))

# Group by the bin and calculate averages
age_groups <- train %>% 
  group_by(YEARS_BINNED) %>%
  summarize(Failure_to_Repay = mean(TARGET, na.rm = TRUE))
age_groups

# Graph the age bins and the average of the target as a bar plot
ggplot(age_groups, aes(x = YEARS_BINNED, y = Failure_to_Repay)) +
  geom_bar(stat = 'identity') +
  ggtitle("Failure to Repay by Age Group") +
  xlab("Age Group (years)") +
  ylab("Failure to Repay (%)")

# Extract the EXT_SOURCE variables and show correlations
ext_data <- train[c('TARGET', 'EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH')]
ext_data_corrs <- cor(ext_data, use = "pairwise.complete.obs")
ext_data_corrs

# Heatmap of correlations
library(RColorBrewer)
corrplot(ext_data_corrs, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))

p1 <- ggplot(train, aes(EXT_SOURCE_1, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of EXT_SOURCE_1 by Target Value") +
  xlab("EXT_SOURCE_1") 

p2 <- ggplot(train, aes(EXT_SOURCE_2, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of EXT_SOURCE_2 by Target Value") +
  xlab("EXT_SOURCE_2") 

p3 <- ggplot(train, aes(EXT_SOURCE_3, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of EXT_SOURCE_3 by Target Value") +
  xlab("EXT_SOURCE_3") 


grid.arrange(p1, p2, p3, ncol = 1)


# Interaction terms 
# Make a new dataframe for interaction terms
inter_features <- c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH')

# Need to impute missing values
train[inter_features] <- train[inter_features] %>% imputeMissings::impute(method = "median/mode")
test[inter_features] <- test[inter_features] %>% imputeMissings::impute(method = "median/mode")

# Check for correlation between combinations of interactions of features with target 
correlations2 <- 
data.frame(cor(train$EXT_SOURCE_1*train$EXT_SOURCE_2, train$TARGET), 
           cor(train$EXT_SOURCE_1*train$EXT_SOURCE_3, train$TARGET),
           cor(train$EXT_SOURCE_1*train$DAYS_BIRTH, train$TARGET), 
           cor(train$EXT_SOURCE_2*train$EXT_SOURCE_3, train$TARGET),
           cor(train$EXT_SOURCE_2*train$DAYS_BIRTH, train$TARGET),
           cor(train$EXT_SOURCE_3*train$DAYS_BIRTH, train$TARGET),
           cor(train$EXT_SOURCE_1*train$EXT_SOURCE_2*train$EXT_SOURCE_3, 
               train$TARGET),
           cor(train$EXT_SOURCE_1*train$EXT_SOURCE_2*train$DAYS_BIRTH, train$TARGET),
           cor(train$EXT_SOURCE_2*train$EXT_SOURCE_3*train$DAYS_BIRTH, train$TARGET)) %>% t()
correlations2 <- correlations2[order(correlations2),] %>% 
  as.data.frame() 
names(correlations2) <- "Correlation"


# Not to add too many features, let's just use the ones with +0.18 corr coef.
train <- train %>% mutate(EXT_SOURCE_2_EXT_SOURCE_3 = EXT_SOURCE_2*EXT_SOURCE_3,
                          EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3 = EXT_SOURCE_1*EXT_SOURCE_2*EXT_SOURCE_3,
                          EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3_DAYS_BIRTH = EXT_SOURCE_2*EXT_SOURCE_3*DAYS_BIRTH)

test <- test %>% mutate(EXT_SOURCE_2_EXT_SOURCE_3 = EXT_SOURCE_2*EXT_SOURCE_3,
                        EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3 = EXT_SOURCE_1*EXT_SOURCE_2*EXT_SOURCE_3,
                        EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3_DAYS_BIRTH = EXT_SOURCE_2*EXT_SOURCE_3*DAYS_BIRTH)

# Domain knowledge features
* CREDIT_INCOME_PERCENT # the percentage of the credit amount relative to a client's income
* ANNUITY_INCOME_PERCENT # the percentage of the loan annuity relative to a client's income
* CREDIT_TERM # the length of the payment in months (since the annuity is the monthly amount due)
* DAYS_EMPLOYED_PERCENT # the percentage of the days employed relative to the client's age

# Creation of features
train['CREDIT_INCOME_PERCENT'] <- train['AMT_CREDIT'] / train['AMT_INCOME_TOTAL']
train['ANNUITY_INCOME_PERCENT'] <- train['AMT_ANNUITY'] / train['AMT_INCOME_TOTAL']
train['CREDIT_TERM'] <- train['AMT_ANNUITY'] / train['AMT_CREDIT']
train['DAYS_EMPLOYED_PERCENT'] <- train['DAYS_EMPLOYED'] / train['DAYS_BIRTH']
test['CREDIT_INCOME_PERCENT'] <- test['AMT_CREDIT'] / test['AMT_INCOME_TOTAL']
test['ANNUITY_INCOME_PERCENT'] <- test['AMT_ANNUITY'] / test['AMT_INCOME_TOTAL']
test['CREDIT_TERM'] <- test['AMT_ANNUITY'] / test['AMT_CREDIT']
test['DAYS_EMPLOYED_PERCENT'] <- test['DAYS_EMPLOYED'] / test['DAYS_BIRTH']

# Visualization of these new features
p4 <- ggplot(train, aes(CREDIT_INCOME_PERCENT, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of CREDIT_INCOME_PERCENT by Target Value") +
  xlab("CREDIT_INCOME_PERCENT") 

p5 <- ggplot(train, aes(ANNUITY_INCOME_PERCENT, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of ANNUITY_INCOME_PERCENT by Target Value") +
  xlab("ANNUITY_INCOME_PERCENT")

p6 <- ggplot(train, aes(CREDIT_TERM, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of CREDIT_TERM by Target Value") +
  xlab("CREDIT_TERM")

p7 <- ggplot(train, aes(DAYS_EMPLOYED_PERCENT, colour = factor(TARGET))) +
  geom_density(size = 1.2) +
  ggtitle("Distribution of DAYS_EMPLOYED_PERCENT by Target Value") +
  xlab("DAYS_EMPLOYED_PERCENT")


grid.arrange(p4, p5, p6, p7, ncol = 1)

# Median imputation of missing values
train <- imputeMissings::impute(train, method = 'median/mode')
test <- imputeMissings::impute(test, method = 'median/mode')

# Scale numeric features
facts <- sapply(train, is.factor)
train <- train[,!facts] %>% 
  dplyr::select(-"TARGET") %>%
  scale() %>%
  cbind(train[facts], train['TARGET'])

  
test <- test[,!facts] %>% 
  dplyr::select(-"TARGET") %>%
  scale() %>%
  cbind(test[facts], test['TARGET'])


# Second look to correlation after feature engineering, imputing and scaling
# Find correlations with the target and sort
facts <- sapply(train, is.factor)
train.n <- train[,!facts]
correlations <- cor(train$TARGET, train.n, use="pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  sort()
correlations[,1:15]
correlations[,53:67]

train$TARGET <- train$TARGET %>% factor(levels = c(0, 1), labels = c('ND', 'D')) 
test$TARGET <- test$TARGET %>% factor(levels = c(0, 1), labels = c('ND', 'D')) 


# Modeling
formula <- TARGET ~ EXT_SOURCE_2_EXT_SOURCE_3 + EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3 +
  EXT_SOURCE_1_EXT_SOURCE_2_EXT_SOURCE_3_DAYS_BIRTH + EXT_SOURCE_2 + EXT_SOURCE_3 +
  EXT_SOURCE_1 + YEARS_BIRTH + FLOORSMAX_AVG + FLOORSMAX_MEDI +
  AMT_GOODS_PRICE + FLOORSMAX_MODE + REGION_POPULATION_RELATIVE + ELEVATORS_AVG +
  YEARS_BEGINEXPLUATATION_AVG + NONLIVINGAPARTMENTS_AVG + NONLIVINGAPARTMENTS_MEDI +
  YEARS_BEGINEXPLUATATION_MODE + NONLIVINGAPARTMENTS_MODE + AMT_INCOME_TOTAL + CREDIT_TERM +
  ANNUITY_INCOME_PERCENT 

# Trying a classic method (GLM)
glmModel <- glm(formula, data=train, family=binomial)
pred.glmModel <- predict(glmModel, newdata=test, type="response")
library(pROC)
roc.glmModel <- pROC::roc(test$TARGET, pred.glmModel)

# GLMBOOST
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 2,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
set.seed(1)
glmBoostModel <- train(formula, 
                       data=train, 
                       method = "glmboost", 
                       metric="ROC", 
                       trControl = fitControl, 
                       tuneLength=5, 
                       center=TRUE)

pred.glmBoostModel <- as.vector(predict(glmBoostModel, newdata=test, type="prob")[,"D"])
roc.glmBoostModel <- pROC::roc(test$TARGET, pred.glmBoostModel)
auc.glmBoostModel <- pROC::auc(roc.glmBoostModel)


# CART
set.seed(1)
cartModel <- train(formula, 
                   data=train, 
                   method = "rpart", 
                   metric="ROC", 
                   trControl = fitControl, 
                   tuneLength=5)

pred.cartModel <- as.vector(predict(cartModel, newdata=test, type="prob")[,"D"])
roc.cartModel <- pROC::roc(test$TARGET, pred.cartModel)
auc.cartModel <- pROC::auc(roc.cartModel)


# Conditional Inference Tree
set.seed(1)
partyModel <- train(formula, 
                    data=train, 
                    method = "ctree", 
                    metric="ROC", 
                    trControl = fitControl, 
                    tuneLength=5)

pred.partyModel <- as.vector(predict(partyModel, newdata=test, type="prob")[,"D"])
roc.partyModel <- pROC::roc(test$TARGET, pred.partyModel)
auc.partyModel <- pROC::auc(roc.partyModel)


# Elastic Net
set.seed(1)
eNetModel <- train(formula, 
                   data=train, 
                   method = "glmnet", 
                   metric="ROC", 
                   trControl = fitControl, 
                   family="binomial", 
                   tuneLength=5)

pred.eNetModel <- as.vector(predict(eNetModel, newdata=test, type="prob")[,"D"])
roc.eNetModel <- pROC::roc(test$TARGET, pred.eNetModel)
auc.eNetModel <- pROC::auc(roc.eNetModel)


# Earth
set.seed(1)
earthModel <- train(formula,
                    data=train,
                    method = "earth",
                    glm=list(family=binomial),
                    metric="ROC",
                    trControl = fitControl,
                    tuneLength=5)

pred.earthModel <- as.vector(predict(earthModel, newdata=test, type="prob")[,"D"])
roc.earthModel <- pROC::roc(test$TARGET, pred.earthModel)
auc.earthModel <- pROC::auc(roc.earthModel)


# Boosted Trees
set.seed(1)
gbmModel <- train(formula,
                  data=train,
                  method = "gbm",
                  metric="ROC", 
                  trControl = fitControl,
                  verbose=FALSE,
                  tuneLength=5)

pred.gbmModel <- as.vector(predict(gbmModel, newdata=test, type="prob")[,"D"])
roc.gbmModel <- pROC::roc(test$TARGET, pred.gbmModel)
auc.gbmModel <- pROC::auc(roc.gbmModel)


# Random Forest
set.seed(1)
rfModel <- train(formula,
                 data=train, 
                 method = "rf",
                 metric="ROC",
                 trControl = fitControl,
                 verbose=FALSE, 
                 tuneLength=5)

pred.rfModel <- as.vector(predict(rfModel, newdata=test, type="prob")[,"D"])
roc.rfModel <- pROC::roc(test$TARGET, pred.rfModel)
auc.rfModel <- pROC::auc(roc.rfModel)


# Choose the best model
test.auc <- data.frame(model=c("glm","glmboost","gbm","glmnet","earth","cart","ctree", "rForest"),
                       auc=c(auc.glmModel, auc.glmBoostModel, auc.gbmModel, auc.eNetModel, auc.earthModel, auc.cartModel, auc.partyModel, auc.rfModel))
test.auc <- test.auc[order(test.auc$auc, decreasing=TRUE),]
test.auc$model <- factor(test.auc$model, levels=test.auc$model)
test.auc

# Plot AUC
library(ggplot2)
theme_set(theme_gray(base_size = 18))
qplot(x=model, y=auc, data=test.auc, geom="bar", stat="identity", position = "dodge")+ geom_bar(fill = "light blue", stat="identity")
