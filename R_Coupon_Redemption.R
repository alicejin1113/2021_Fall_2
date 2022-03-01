###title: "CRM Group Project - Coupon Redemption"
##authors: "Marketing Group 29"


##Importing libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyr)
library(GGally)
library(faraway)
library(Hmisc)
library(caret)
library(knitr)
library(dplyr)
library(rpart.plot)
library(MASS)


### Inital Data Compilation, Data joining, Data Sampling (Size Reduction) and Writing to new CSV
## Reading in five  initial datasets 
transaction<-read.csv("customer_transaction_data.csv")
train<-read.csv("train.csv")
campaign<-read.csv("campaign_data.csv")
item_data_coupon_mapping<-read.csv("item_data_coupon_mapping.csv")
customer_demo<-read.csv("customer_demographics_CLEAN.csv")

### Setting up date variables as date types
transaction$date<-as.Date(transaction$date, format = "%Y-%m-%d")
campaign$start_date<-as.Date(campaign$start_date, format = "%Y-%m-%d")
campaign$end_date<-as.Date(campaign$end_date, format = "%Y-%m-%d")
transaction$date<-as.Date(transaction$date, format = "%Y-%m-%d")


### Performing the required joins in our data. 
train_with_campaign2 <- inner_join(train,campaign,by="campaign_id")
train_with_campaign_with_demo2 <- inner_join(train_with_campaign2,customer_demo,by="customer_id")
train_camp_demo_coup_item <- inner_join(train_with_campaign_with_demo2,item_data_coupon_mapping,by="coupon_id")
final <- inner_join(train_camp_demo_coup_item,transaction,by=c("customer_id","item_id"))

### Filtering data (making various logical assumptions)
#1. Drop columns where redemption_status = 0 but coupon_discount exists (1.7%)
final3<- filter(final,!(redemption_status==0 & coupon_discount<0))
#2. If redemption_status = 1 and coupon_discount = 0 ,. other_discount=0 and quantity =1, then drop (3.9%)
final4<- filter(final3,!(redemption_status==1 & coupon_discount==0 & other_discount==0 & quantity==1))

###Exporting Data to CSV
write.csv(final4, file = "Coupon_Redemption_Data")


####################
### Exploratory Data Analysis
### Understanding the variables
## Identifying the number of features or columns
couponData<-read.csv("Coupon_Redemption_Data.csv")
describe(couponData)
str(couponData, give.attr = FALSE)
num_entry <- dim(couponData)[1]
num_col <- dim(couponData)[2]
num_missing <- sum(complete.cases(couponData)) - nrow(couponData)
print(paste("Our final table has",num_entry,"entries and", num_col, "of columns with", num_missing, "missing values"))

## Identifying the data types of features
numeric_col <- select_if(couponData, is.numeric) %>% colnames()
numeric_col
num_numeric_col <- select_if(couponData, is.numeric) %>% ncol()
num_numeric_col
char_col <- select_if(couponData, is.character) %>% colnames()
char_col
num_char_col <- select_if(couponData, is.character) %>% ncol()
num_char_col
View(couponData)

### Analyzing relationships between variables

## Correlation
names <- c('marital_status' ,'brand_type', 'campaign_type','category', 'age_range')
couponData[,names] <- lapply(couponData[,names] , factor)
str(couponData, give.attr = FALSE)
couponData[, -1] %>% select_if(is.numeric) %>% cor() %>% corrplot()
couponData[, c(4, 6, 10, 14, 16, 17, 27)] %>% cor() %>% corrplot(, method = "number")


## Multicollinearity (problematic if greater than 5)
multicol <- couponData[, -1] %>% select_if(is.numeric) %>% faraway::vif()
multicol
multicol_var <- multicol[multicol > 5]
multicol_var
# We can see that marital_status_binary, family_size and no_of_children are problematic.
# Analyzed together with the corrplot, we can see that family_size is highly correlated with no_of_children
# The correaltion is 
cor(couponData$no_of_children, couponData$family_size)
# Also, analyzed together with the corrplot, we can see that marital_status_binary is correlated with family_size
# The correaltion is 
cor(couponData$marital_status_binary, couponData$family_size)
cor(couponData$marital_status_binary, couponData$no_of_children)
# Further see the collinearity
plot(couponData$no_of_children, couponData$family_size) # parallel
plot(couponData$marital_status_binary, couponData$family_size) # parallel

drop_marital_status_binary <- c("marital_status_binary") 
couponData_ind <- couponData[, !(names(couponData) %in% drop_marital_status_binary)]
multicol_ind_1 <- couponData_ind[, -1] %>% select_if(is.numeric) %>% faraway::vif()
multicol_ind_1

drop_family_size <- c("family_size") 
couponData_ind <- couponData[, !(names(couponData) %in% drop_family_size)]
multicol_ind_2 <- couponData_ind[, -1] %>% select_if(is.numeric) %>% faraway::vif()
multicol_ind_2

drop_no_of_children <- c("no_of_children") 
couponData_ind <- couponData[, !(names(couponData) %in% drop_no_of_children)]
multicol_ind_3 <- couponData_ind[, -1] %>% select_if(is.numeric) %>% faraway::vif()
multicol_ind_3
# As long as deleting family_size or no_of_children, we are free of collinearity problem.

### Presenting visualization of variables

#Replicating data entries by the quantity to create graphs that are weighted by the correct quantity column
library(tidyr)
couponData_new <- couponData %>% 
  uncount(couponData$quantity)

#Group by date and customer_id (transaction), creating aggregate quantities
library(dplyr)
trans <- couponData %>% group_by(date, customer_id) %>% summarise(quantity = sum(quantity))
hist(trans$quantity)
#The quantity of items in a transaction is heavily skewed to the right, with most of the transaction item quantities being below 5.

hist(couponData_new$selling_price)
#The selling price of items is heavily skewed to the right, with most items having selling prices below $200.

hist(-couponData_new$other_discount)
#The other_discount applied on items is heavily skewed to the right, with most items having other_discount applied to them below $50.

hist(-couponData_new$coupon_discount)
#The couponData_discount applied on items is heavily skewed to the right, with most items having couponData_discount applied to them below $50.

hist(couponData_new$duration)
#Almost all of the campaigns have durations of 47 days or 48 days.

###############################################
#hist(couponData_new$redemption_status)
#The number of items that redeem the couponDatas are only 1/4 of the number of items that do not redeem the couponDatas.
#lookup <- c("X" = 0, "Y" = 1)
#couponData_new$new_campaign_type <- lookup[couponData_new$campaign_type]
#hist(couponData_new$new_campaign_type)
#The number of Campaign Y are only 1/4 of the number of Campaign Y.
#print(paste('Campaign Duration-  ',"X:",proportions2[1],"Y:",proportions2[2]))
#hist(couponData_new$duration)


#hist(couponData_new$age_group_encoded)
#The customer age groups are normally distributed.

#hist(couponData_new$rented)
#Almost all of the customers live in their own home rather than a rented place.

#hist(couponData_new$income_bracket)
#The customer income brackets are normally distributed.
# lookup2 <- c("Local" = 0, "Established" = 1)
# couponData_new$new_brand_type <- lookup2[couponData_new$brand_type]
# hist(couponData_new$new_brand_type)
# #The number of local brands is only 1/4 of the number of established brands.
# 
# lookup3 <- c("Bakery" = 1, "Dairy, Juices & Snacks" = 2, "Flowers & Plants" = 3, "Garden" = 4, "Grocery" = 5, "Meat" = 6, "Miscellaneous" = 7, "Natural Products" = 8, "Packaged Meat" = 9, "Pharmaceutical" = 10, "Prepared Food" = 11, "Salads" = 12, "Seafood" = 13, "Skin & Hair Care" = 14, "Travel" = 15, "Vegetables (cut)" = 16)
# couponData_new$new_category <- lookup3[couponData_new$category]
# hist(couponData_new$new_category)
#The category of "Meat" contains the most number of items.

#########################################################


ct<-couponData_new %>% count(redemption_status)
proportions <- table(couponData_new$redemption_status)/length(couponData_new$redemption_status)
colnames(proportions) <- c('Not Redeemed','Redeemed')
percentages <- proportions*100
barplot(percentages, main="Redemption Distribution", 
        xlab="Redemption Status", ylab="Percentage",
        ylim=c(0,100), names=c("Not Redeemed", "Redeemed"),
        col = "lightblue")
print(paste('Only',round(percentages[2],2),"% of the items in coupons are redeemed"))

ct1<-couponData_new %>% count(campaign_type)
proportions1 <- table(couponData_new$campaign_type)/length(couponData_new$campaign_type)
percentages1 <- proportions1*100
barplot(percentages1, main="Campaigh Types", 
        xlab="Type", ylab="Percentage",
        ylim=c(0,100), names=c("X","Y"),
        col = "lightblue")
print(paste('Campaign Types-  ',"X:",percentages1[1],"Y:",percentages1[2]))


ct3<-couponData_new %>% count(age_group_encoded)
proportions3 <- table(couponData_new$age_group_encoded)/length(couponData_new$age_group_encoded)
percentages3<- proportions3*100
barplot(percentages3, main="Distribution of Customers' age groups", 
        xlab="Age groups", ylab="Percentage",
        ylim=c(0,100),
        col = "lightblue")
#The customer age groups are normally distributed.

ct4<-couponData_new %>% count(rented)
proportions4 <- table(couponData_new$rented)/length(couponData_new$rented)
percentages4 <- proportions4*100
barplot(percentages4, main="Customers' Home OWnership", 
        xlab="Home Ownership Status", ylab="Percentage",
        ylim=c(0,100),names=c("Owned","Rented"),
        col = "lightblue")
#Almost all of the customers live in their own home rather than a rented place.

ct5<-couponData_new %>% count(income_bracket)
proportions5 <- table(couponData_new$income_bracket)/length(couponData_new$income_bracket)
percentages5 <- proportions5*100
barplot(percentages5, main="Income Buckets", 
        xlab="income buckets", ylab="Percentage",
        ylim=c(0,100),
        col = "lightblue")
#The customer income brackets are normally distributed.

ct7<-couponData_new %>% count(category)
proportions7 <- table(couponData_new$category)/length(couponData_new$category)
percentages7 <- proportions7*100
barplot(percentages7, main="Item Categories", 
        xlab="Categories", ylab="Percentage",
        ylim=c(0,100),
        col = "lightblue")
#The category of "Meat" contains the most number of items.

ct6<-couponData_new %>% count(brand_type)
proportions6 <- table(couponData_new$brand_type)/length(couponData_new$brand_type)
percentages6 <- proportions6*100
barplot(percentages6, main=" Brand Types", 
        xlab="Brand Types", ylab="Percentage",
        ylim=c(0,100), name=c("Established","Local"),
        col = "lightblue")
#The number of local brands is only 1/4 of the number of established brands.


###___________________________________###
### MODEL SELCTION


### drop columns not adding to analysis and set required variables up as factors
couponData<-read.csv("Coupon_Redemption_Data.csv")
str(couponData)
drop <- c('X','id', 'date', 'campaign_id', 'customer_id', 'coupon_id', 'campaign_type', 'start_date', 'end_date', 'age_group_encoded','marital_status_binary','item_id','brand','no_of_children')
couponData1 = couponData[,!(names(couponData) %in% drop)]
str(couponData1)
names <- c('brand_type', 'category', 'age_range', 'income_bracket', 'rented', 'redemption_status','marital_status')
couponData1[,names] <- lapply(couponData1[,names] , factor)
head(couponData1)
str(couponData1)
summary(couponData1)

### Subset Data into training (80%) and test (20%) data sets
dt = sort(sample(nrow(couponData1), nrow(couponData1)*.8))

CouponTrainingData<-couponData1[dt,]
CouponTestData<-couponData1[-dt,]

###Model selection 
### Model 1: Generalized Linear Model
### Using stepAIC to determine the most sigificant variables for this model

stepAICmodel <- glm(redemption_status~.,
                data = CouponTrainingData,
                family="binomial")
            
a <- stepAIC(stepAICmodel, direction = "both")
a$anova

### Note that these variables in the a$anova output ("Final Model") will be used in all models moving forward
trctrl = trainControl(method = "repeatedcv", number = 5,repeats = 3)
mylogit = train(redemption_status ~ duration + age_range + marital_status + rented + 
                  family_size + income_bracket + brand_type + category + quantity + 
                  selling_price + other_discount + coupon_discount,
                data = CouponTrainingData,
                method = "glm",
                family="binomial",
                trControl=trctrl)
mylogit
summary(mylogit)

### Model 2:Lasso regression Model

trctrl = trainControl(method = "cv", number = 5)
reg_lasso<-train(redemption_status ~ duration + age_range + marital_status + rented + 
                   family_size + income_bracket + brand_type + category + quantity + 
                   selling_price + other_discount + coupon_discount,
                 data=CouponTrainingData,
                 method="glmnet",
                 family="binomial",
                 trControl=trctrl,
                 tuneGrid = expand.grid(alpha = 1,  lambda = seq(0.001,0.1,by = 0.001)))

reg_lasso$bestTune

trctrl = trainControl(method = "repeatedcv", number = 5,repeats = 3)
reg_lasso<-train(redemption_status ~ duration + age_range + marital_status + rented + 
                   family_size + income_bracket + brand_type + category + quantity + 
                   selling_price + other_discount + coupon_discount,
                 data=CouponTrainingData,
                 method="glmnet",
                 family="binomial",
                 trControl=trctrl,
                 tuneGrid = expand.grid(alpha = 1,  lambda = reg_lasso$bestTune[,2]))

reg_lasso
summary(reg_lasso$finalModel)

### Model 3: Decision Tree Model
trctrl = trainControl(method = "repeatedcv", number = 5,repeats = 3)
decisiontree = train(redemption_status ~ duration + age_range + marital_status + rented + 
                       family_size + income_bracket + brand_type + category + quantity + 
                       selling_price + other_discount + coupon_discount,
                   data=CouponTrainingData,
                   method = "rpart",
                   parms = list(split = "gini"), 
                   trControl=trctrl,
                   tuneLength = 10)

prp(decisiontree$finalModel)
decisiontree

### Predict 3 Models on test dataset

glm_prediction <- predict(mylogit, CouponTestData)
reg_lasso_prediction <- predict(reg_lasso, CouponTestData)
decisiontree_prediction <- predict(decisiontree, CouponTestData)

summary(glm_prediction)
summary(reg_lasso_prediction)
summary(decisiontree_prediction)

#glm OOS fit
glmtable <- table(CouponTestData$redemption_status, glm_prediction)
rownames(glmtable) <- c("Obs.no redeem","Obs.redeem")
colnames(glmtable) <- c("Pred.no redeem","Pred.redeeem")
glmtable
glm_accuracy <- sum(diag(glmtable))/sum(glmtable) # TP+TN/(TP+T+FP+FN)
glm_sensitivity<- glmtable[2,2]/(glmtable[2,2] + glmtable[2,1]) #TP/TP+FN
glm_specificity <- glmtable[1,1]/(glmtable[1,1] + glmtable[1,2])    #TN/TN+FP

glm_accuracy
glm_sensitivity
glm_specificity

#reg_lasso OOS fit
reg_lasso_table <- table(CouponTestData$redemption_status, reg_lasso_prediction)
rownames(reg_lasso_table) <- c("Obs.no redeem","Obs.redeem")
colnames(reg_lasso_table) <- c("Pred.no redeem","Pred.redeeem")
reg_lasso_table
reg_lasso_accuracy <- sum(diag(reg_lasso_table))/sum(reg_lasso_table) # TP+TN/(TP+T+FP+FN)
reg_lasso_sensitivity<- reg_lasso_table[2,2]/(reg_lasso_table[2,2] + reg_lasso_table[2,1]) #TP/TP+FN
reg_lasso_specificity <- reg_lasso_table[1]/(reg_lasso_table[1] + reg_lasso_table[1,2])    #TN/TN+FP


reg_lasso_accuracy
reg_lasso_sensitivity
reg_lasso_specificity

#DT OOS fit
decisiontree_table <- table(CouponTestData$redemption_status, decisiontree_prediction)
rownames(decisiontree_table) <- c("Obs.no redeem","Obs.redeem")
colnames(decisiontree_table) <- c("Pred.no redeem","Pred.redeeem")
decisiontree_table
decisiontree_accuracy <- sum(diag(decisiontree_table))/sum(decisiontree_table) # TP+TN/(TP+T+FP+FN)
decisiontree_sensitivity<- decisiontree_table[2,2]/(decisiontree_table[2,2] + decisiontree_table[2,1]) #TP/TP+FN
decisiontree_specificity <- decisiontree_table[1]/(decisiontree_table[1] + decisiontree_table[1,2])    #TN/TN+FP

decisiontree_accuracy
decisiontree_sensitivity
decisiontree_specificity

#Summary table of prediciton results
summary_results <- matrix(c(glm_accuracy, glm_sensitivity, glm_specificity, reg_lasso_accuracy, 
                            reg_lasso_sensitivity, reg_lasso_specificity, decisiontree_accuracy,
                            decisiontree_sensitivity, decisiontree_specificity), ncol=3, byrow=TRUE)
colnames(summary_results) <- c('Accuracy','Sensitivity','Specificity')
rownames(summary_results) <- c('glm','reg_lasso','decisiontree')
summary_results <- as.table(summary_results)
summary_results

#Plotting results
barplot(summary_results[,1], main="Accuracy", 
        #xlab="Brand Types", ylab="Percentage",
        ylim=c(0,1), 
        col = "lightblue")
barplot(summary_results[,2], main="Sensitivity", 
        #xlab="Brand Types", ylab="Percentage",
        ylim=c(0,1), 
        col = "lightblue")
barplot(summary_results[,3], main="Specificity", 
        #xlab="Brand Types", ylab="Percentage",
        ylim=c(0,1), 
        col = "lightblue")

#####

## ROC is (FPR, TPR)

R.FPR <- 1-reg_lasso_specificity
R.TPR <- reg_lasso_sensitivity
Tr.FPR <- 1-decisiontree_specificity
Tr.TPR <- decisiontree_sensitivity

##ROC for lasso and DT
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( R.FPR,Tr.FPR), c(R.TPR,Tr.TPR))
text( c( R.FPR+0.04,Tr.FPR), c(R.TPR-.01,Tr.TPR-.05), labels=c("Lasso","Tree"))

### Running Chi-squared tests to assess model significance

(TESTsq <- chisq.test(glmtable))
(TESTsq <- chisq.test(reg_lasso_table))
(TESTsq <- chisq.test(decisiontree_table))

# Since we are interested in maximizing customer engagement while maximizing profit,we will choose
# the decision tree model as it has the highest accuracy and classifies more number of customers as redeemers
# Although predicted redeemers are more than the actual redeemers in OOS, we can use targeted marketing to
# offer these customers more relevant coupons and campaigns that will lead to coupon redemption. However,
# since these marketing strategies come with a cost, we can implement a cost-benefit analysis while predicting
# a customer's future worth and repurchase rate to make future decisions.
                                                                                                  