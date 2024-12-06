################################## we have to reset everything because rplot conflict with one of our ppackages so 
#######we have to start over
rm(list=ls()); gc()

require(dplyr)


application.data = read.csv("C:/Users/avasq/OneDrive/Desktop/isds 577/kaggle/application_record.csv/application_record.csv",stringsAsFactors = TRUE)



credit_record_data = read.csv("C:/Users/avasq/OneDrive/Desktop/isds 577/kaggle/credit_record.csv/credit_record.csv",stringsAsFactors = TRUE)
str(credit_record_data)

#check data

#check na/missing data
sum(is.na(application.data))

application.data$OCCUPATION_TYPE
plot(application.data$OCCUPATION_TYPE)
levels(application.data$OCCUPATION_TYPE)
table(application.data$OCCUPATION_TYPE) ####has 134203 cell with empty string



# Filter rows with blank values
blank_result <- application.data %>%
  filter_all(any_vars(grepl("^\\s*$", .)))

# Calculate the percentage of blank values in the 'OCCUPATION_TYPE' column
blank_percentage <- application.data %>%
  summarise(percentage_blank = mean(is.na(application.data$OCCUPATION_TYPE) | application.data$OCCUPATION_TYPE == "")) %>%
  pull(percentage_blank)
# Print the percentage
cat("Percentage of blank values in 'OCCUPATION_TYPE':", blank_percentage * 100, "%\n")





#check duplicates
table(duplicated(application.data))
str(application.data)

# Combine categories after 5 so 5 or more children whill have a category
id = which(application.data$CNT_CHILDREN %in% c("5",'6', '7', '8', '9',"10","11","12","14","19"))
application.data$CNT_CHILDREN[id] = 'fiveOrMore'
table(application.data$CNT_CHILDREN)

# Combine categories after 5 so 5 or more children whill have a category
id = which(application.data$CNT_FAM_MEMBERS %in% c('6', '7', '8', '9',"11","14","15","20"))
application.data$CNT_FAM_MEMBERS[id] = "sixOrMore"
table(application.data$CNT_FAM_MEMBERS)



### remove this colom because every record has a cell phone
application.data$FLAG_MOBIL <- NULL #Remove this column


#transform years employed from negitive day to year
year_function= function(x) abs(x/365)
application.data$YEARS_EMPLOYED<- as.numeric(lapply(application.data$DAYS_EMPLOYED, year_function))

#transform years employed from negitive day to year
application.data$Years_old <- as.numeric(lapply(application.data$DAYS_BIRTH, year_function))

#transformed from AMT_INCOME_TOTAL to thousand scale it was way to big
income_transform_thouds= function(x) x/1000

application.data$AMT_INCOME_TOTAL_thousand <- as.numeric(lapply(application.data$AMT_INCOME_TOTAL,income_transform_thouds))   

#take out days birth, days employed and income because we transformed them to yearly and income in thousands
####Erase occupation be cause it has 134203 cell with empty string
### remove this colom because every record has a cell phone

application.data<-application.data%>%  select(-OCCUPATION_TYPE, -DAYS_BIRTH,-DAYS_EMPLOYED,-AMT_INCOME_TOTAL)
str(application.data)



summary(application.data)



#############################################################################
#Feture engineering to help identify what records are delinquint and lables six_month_OD
####transformation credit_record_data history of customer with unique id
str(credit_record_data)
str(application.data)

cred_hist_ag <- credit_record_data%>%
  group_by(credit_record_data$ID)%>%
  summarize(MONTHS_BALANCE_min= abs(min(MONTHS_BALANCE)-1))


##############################################################################
########### create a data table to piviot data wider, turn it into a data frame  ####################################
credit_status_transformed = table(credit_record_data$ID, credit_record_data$STATUS)
credit_status_transformed= as.data.frame.matrix(credit_status_transformed)
class(credit_status_transformed)

###Combine both files applicationData and Credit History
final_Credi_record_tRANSFORMED= cbind(cred_hist_ag,credit_status_transformed)
##NAMe new colons to refelct how many month id is late
colnames(final_Credi_record_tRANSFORMED)<- c("ID","account_age", "One_month_OD","two_months_OD","three_months_OD","four_months_OD","five_months_OD","six_months_OD","Current","NO_loan")
####################################################################
###########join the credit and aplication data
#########final credit data akil
final_both_transform<- inner_join(application.data,final_Credi_record_tRANSFORMED,by="ID")
# tabl to see how many record are over 6months in default
final_both_transform$One_month_OD

########## Featur engineering of % of late fees
final_both_transform$Percent_1month_late <-(final_both_transform$One_month_OD/final_both_transform$account_age)
hist(final_both_transform$Percent_1month_late)
final_both_transform$Percent_1month_late<- ifelse(final_both_transform$Percent_1month_late <= .4 &final_both_transform$Percent_1month_late >= .1 &  final_both_transform$six_months_OD==0,1,0)
table(final_both_transform$Percent_1month_late)
table(final_both_transform$six_months_OD)
########################################
#### take out non dummy 1 -6month od

########turn 6 month overdue into decision variable dummy however,

final_both_transform$six_months_OD1 <- ifelse(final_both_transform$six_months_OD == "0", 0, 1)

levels(as.factor(final_both_transform$six_months_OD1))
#### make decision variables factor so can be analized
final_both_transform$six_months_OD1<-as.factor(final_both_transform$six_months_OD1)
final_both_transform$Percent_1month_late<-as.factor(final_both_transform$Percent_1month_late)


#######create dummy variables 
library(fastDummies)

final_both_transform =dummy_cols(final_both_transform, select_columns =  c("CODE_GENDER","FLAG_OWN_CAR","FLAG_OWN_REALTY","CNT_CHILDREN","NAME_INCOME_TYPE","NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS","NAME_HOUSING_TYPE","CNT_FAM_MEMBERS"),remove_most_frequent_dummy = T,remove_selected_columns = T)                                                                           

###Final stucture of data with transformation and dummy
str(final_both_transform)

#### remove Unessary coloms
final_both_transform<- final_both_transform%>%
  select(-ID)

final_both_transform
###############count number of 6 months overdue and data visulizations
table(final_both_transform$six_months_OD)

###############sample 







################## undersample  and partician in to traing and test



set.seed(23)
n_desired_new=180
index_sampled=sample(which(final_both_transform$six_months_OD1==0),n_desired_new,replace=F)
new_df1=final_both_transform[c(index_sampled, which(final_both_transform$six_months_OD1==1)),] 

str(new_df1)
table(new_df1$six_months_OD1)  

Under.train = floor( nrow(new_df1)*0.75 )

ind.train.Un = sample(1:nrow(new_df1), Under.train)
ind.test.Un = setdiff(1:nrow(new_df1), ind.train.Un)



under_train = new_df1[ind.train.Un,]
under_test = new_df1[ind.test.Un,]
str(under_test)
under_test$six_months_OD1<-as.factor(under_test$six_months_OD1)

under_train$six_months_OD1<-as.factor(under_train$six_months_OD1)

##################################################  
############## Oversample  and partician data into traning and test

set.seed(2)
n_desired_new=36277
index_sampled=sample(which(final_both_transform$six_months_OD1==1),n_desired_new,replace=T)

new_df=final_both_transform[c(index_sampled, which(final_both_transform$six_months_OD1==0)),] 

table(new_df$six_months_OD1)

Over.train = floor( nrow(new_df)*0.75 )
ind.train.ov = sample(1:nrow(new_df), Over.train)
ind.test.ov = setdiff(1:nrow(new_df), ind.train.ov)

overr_train = new_df1[ind.train.ov,]
over_test = new_df1[ind.test.ov,]


str(overr_train)



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#New training data for percent of customers late one  month akil use this to
#build one model, but also use the above to build anouther
#######################################################################
#######################################################################
#######################################################################
set.seed(1)
final_both_transform$Percent_1month_late<-as.factor(final_both_transform$Percent_1month_late)
n.train.new = floor( nrow(final_both_transform)*0.75 )
ind.train.new = sample(1:nrow(final_both_transform), n.train.new)
ind.test.new = setdiff(1:nrow(final_both_transform), ind.train.new)

New.training = final_both_transform[ind.train.new,]
New.test = final_both_transform[ind.test.new,]
str(New.test)






library(rpart); library(rpart.plot)

require("rpart.plot")
require("rpart")

require(caret)

str(under_train)


######full tree
fit= rpart(six_months_OD1 ~ ., method="class", data=under_train, minsplit=5, cp=.001)
prp(fit)
printcp(fit)
fit$cptable
######## predict full with test
predict_under_train_full= predict(fit,under_test,type="class")
confusionMatrix(predict_under_train_full,under_test$six_months_OD1)
#####prune min error
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

rpart.plot(pfit.me, main = 'Min Error Tree')

##########Min error Predict 
predict_under_train_min= predict(pfit.me,under_train,type="class")
confusionMatrix(predict_under_train_min,under_train$six_months_OD1)
printcp(pfit.me)

##########Min error Predict 
predict_under_test_min= predict(pfit.me,under_test,type="class")
confusionMatrix(predict_under_test_min,under_test$six_months_OD1)
printcp(pfit.me)

under.me <- data.frame(imp = pfit.me$variable.importance)
df2 <- under.me %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()


######## best pruned tree
k<-10
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(k) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')
prp(pfit.bp)

#######predict best pruned with training
predict_under_train_best= predict(pfit.bp,under_train,type="class")
confusionMatrix(predict_under_train_best,under_train$six_months_OD1)
printcp(pfit.bp)

#######predict best pruned with testing
predict_under_test_best= predict(pfit.bp,under_test,type="class")
confusionMatrix(predict_under_test_best,under_test$six_months_OD1)
printcp(pfit.bp)
prp(pfit.bp)


######################xg boost for late fee prediction
#####xgboos of new variable
library(xgboost)
require(rpart)
library(caret)
library(dplyr)
#install.packages("MASS")
library(DiagrammeR)
require(MASS)
require(dplyr)

xg.under <- train( Percent_1month_late ~., data = New.training, method = "xgbTree",  trControl = trainControl("cv", number = 10))


# Best tuning parameter
xg.under$bestTune
# Make predictions on the test data
predicted.classes <- xg.under %>% predict(under_test)

head(predicted.classes)
mean(predicted.classes == under_test$six_months_OD1)
confusionMatrix(predicted.classes,under_test$six_months_OD1)
varImp(xg.under) 

# plot the first tree
xgb.plot.tree(model = xg.under$finalModel, trees = 1)
xg.under$levels
xg.under$modelType
xg.under$levels

var_imp<-varImp(xg.under)
plot(var_imp)



##########################################################################################
#####oversample full tree
hr_base_model= rpart(six_months_OD1 ~ ., method="class", data=overr_train, control = rpart.control(cp = 0))
summary(hr_base_model)
#Plot Decision Tree
plot(hr_base_model)
# Examine the complexity plot
printcp(hr_base_model)
plotcp(hr_base_model)
prp(hr_base_model)

####predict with training
my_predict_under_train= predict(hr_base_model,overr_train,type="class")
confusionMatrix(my_predict_under_train,under_train$six_months_OD1)
#predict with testing
my_predict_under_test= predict(hr_base_model,over_test,type="class")
confusionMatrix(my_predict_under_test,over_test$six_months_OD1)

## How to predict? I am taking best pruned tree as an example.
yhat = predict(hr_base_model, over_test, type = "class") # replace "dat" by validation data if you have it
err.bp = mean(yhat != over_test$six_months_OD1)



##################################################
#######################################
##################CART FOR NEW data Set to identify customers who generate late fees


fit2= rpart( Percent_1month_late ~ ., method="class", data=New.training, minsplit=3)
prp(fit2)
printcp(fit2)
fit2$cptable
plot(fit2)
######## predict full with test
predict_new= predict(fit2,New.test,type="class")
require(caret)
confusionMatrix(predict_new,as.factor(New.test$Percent_1month_late))
str(New.test)
#####prune min error
new.pfit.me = prune(fit2, cp = fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])

prp(new.pfit.me,main = 'Min Error Tree')


##########Min error Predict 
predict_new_one_month= predict(new.pfit.me, New.test,type="class")
confusionMatrix(predict_new_one_month,New.test$Percent_1month_late)
printcp(new.pfit.me)

##########Min error Predict 
predict_new_test_min= predict(new.pfit.me,New.test,type="class")
confusionMatrix(predict_new_test_min,New.test$Percent_1month_late)
printcp(pfit.me)


######## best pruned tree
k<-10
ind = which.min(new.pfit.me$cptable[,"xerror"]) # xerror: cross-validation error
se1 = new.pfit.me$cptable[ind,"xstd"]/sqrt(k) # 1 standard error
xer1 = min(new.pfit.me$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(new.pfit.me$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
new_pfit.bp = prune(new.pfit.me, cp = new.pfit.me$cptable[ind0,"CP"])
rpart.plot(new_pfit.bp, main = 'Best Pruned Tree')
prp(new_pfit.bp, main = 'Best Pruned Tree')
new_pfit.bp$variable.importance
plot(new_pfit.bp$variable.importance)
#######predict best pruned with training
predict_under_train_best= predict(new_pfit.bp,New.training,type="class")
confusionMatrix(predict_under_train_best,New.training$Percent_1month_late)
printcp(new_pfit.bp)

#######predict best pruned with testing
predict_new_one_month= predict(pfit.bp,New.test,type="class")
confusionMatrix(predict_new_one_month,New.test$Percent_1month_late)
printcp(new_pfit.bp)
prp(new_pfit.bp)

varImp(new_pfit.bp) 



#####xgboost of late fees
library(xgboost)
library(caret)
library(dplyr)
#install.packages("MASS")
library(DiagrammeR)
require(MASS)

model <- train( Percent_1month_late ~., data = New.training, method = "xgbTree",  trControl = trainControl("cv", number = 10))


# Best tuning parameter
model$bestTune
# Make predictions on the test data
predicted.classes <- model %>% predict(New.test)

head(predicted.classes)
mean(predicted.classes == New.test$Percent_1month_late)

confusionMatrix(predicted.classes, New.test$Percent_1month_late)

varImp(model) 


# plot var importance
xgb.plot.tree(model = model$finalModel, trees = 1)

df <- data.frame(imp = fit$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()









