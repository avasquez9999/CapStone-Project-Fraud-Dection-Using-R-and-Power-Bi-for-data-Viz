rm(list=ls()); gc()

require(dplyr)
require(fastDummies)


application.data = read.csv("C:/Users/avasq/OneDrive/Desktop/isds 577/kaggle/application_record.csv/application_record.csv",stringsAsFactors = TRUE)



credit_record_data = read.csv("C:/Users/avasq/OneDrive/Desktop/isds 577/kaggle/credit_record.csv/credit_record.csv",stringsAsFactors = TRUE)
str(credit_record_data)

#check data
summary(application.data)
str(application.data)

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

#########viulization
#Check Continuous Variable
hist(application.data$AMT_INCOME_TOTAL)
boxplot(application.data$AMT_INCOME_TOTAL)
summary(application.data$AMT_INCOME_TOTAL)
plot(density(application.data$AMT_INCOME_TOTAL))
table(application.data$AMT_INCOME_TOTAL)





AMT_INCOME_TOTAL_Q1 <- quantile(application.data$AMT_INCOME_TOTAL, 0.25)
AMT_INCOME_TOTAL_Q3 <- quantile(application.data$AMT_INCOME_TOTAL, 0.75)
AMT_INCOME_TOTAL_IQR <- AMT_INCOME_TOTAL_Q3 - AMT_INCOME_TOTAL_Q1
AMT_INCOME_TOTAL_lower_bound <- AMT_INCOME_TOTAL_Q1 - 1.5 * AMT_INCOME_TOTAL_IQR
AMT_INCOME_TOTAL_upper_bound <- AMT_INCOME_TOTAL_Q3 + 1.5 * AMT_INCOME_TOTAL_IQR
#application.data <- subset(application.data, application.data$AMT_INCOME_TOTAL> AMT_INCOME_TOTAL_upper_bound) #??how many shall be removed ## I would remove anything above 400000 
nrow(application.data)

summary(application.data$AMT_INCOME_TOTAL)


hist(application.data$DAYS_BIRTH)
summary(application.data$DAYS_BIRTH)
boxplot(application.data$DAYS_BIRTH)
plot(density(application.data$DAYS_BIRTH))
table(application.data$DAYS_BIRTH)

hist(application.data$DAYS_EMPLOYED)
boxplot(application.data$DAYS_EMPLOYED)

summary(application.data$DAYS_EMPLOYED)
plot(density(application.data$DAYS_EMPLOYED))
table(application.data$DAYS_EMPLOYED)

#check Categorical variable
barplot(table(application.data$CODE_GENDER))
barplot(table(application.data$FLAG_OWN_CAR))
barplot(table(application.data$FLAG_OWN_REALTY))
barplot(table(application.data$CNT_CHILDREN), main="Number of Children")
table(application.data$CNT_CHILDREN)

barplot(table(application.data$NAME_INCOME_TYPE))
barplot(table(application.data$NAME_EDUCATION_TYPE))
barplot(table(application.data$NAME_FAMILY_STATUS))
barplot(table(application.data$NAME_HOUSING_TYPE))
table(application.data$CNT_FAM_MEMBERS)
barplot(table(application.data$FLAG_MOBIL)) #all have mobile

barplot(table(application.data$FLAG_WORK_PHONE))
barplot(table(application.data$FLAG_PHONE))

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

application.data<-application.data%>%  select( -OCCUPATION_TYPE, -DAYS_BIRTH,-DAYS_EMPLOYED,-AMT_INCOME_TOTAL,)
str(application.data)



summary(application.data)

###dat visualization
boxplot(application.data$AMT_INCOME_TOTAL_thousand, main= "Total Income in Thousands" )
summary(application.data$AMT_INCOME_TOTAL_thousand)

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
  select(-six_months_OD,-ID)

final_both_transform
###############count number of 6 months overdue and data visulizations
table(final_both_transform$six_months_OD)

###############sample 

##########6 months dilinquint  count and visualization
length(which(final_both_transform$six_months_OD == 0))
length(which(final_both_transform$six_months_OD == 1))
barplot(table(final_both_transform$six_months_OD1))

###############5 moths visualization
table(final_both_transform$five_months_OD1)

barplot(table(final_both_transform$five_months_OD))


########four months delinquent visalization
table(final_both_transform$four_months_OD)
barplot(table(final_both_transform$four_months_OD))
length(which(final_both_transform$four_months_OD == 1))


##########3 months visuzlization 
table(final_both_transform$three_months_OD)
barplot(table(final_both_transform$three_months_OD))
length(which(final_both_transform$three_months_OD == 1))

#########one month delinquent
barplot(table(final_both_transform$Percent_1month_late))
length(which(final_both_transform$Percent_1month_late == 1))
table(final_both_transform$Percent_1month_late)







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

require(caret)

#############logistic regression for both delqint and late fee generation model
######log for delinquint
min.modelD = glm( six_months_OD1~ 1, data = New.training, family = 'binomial')
max.modelD = glm(six_months_OD1 ~ ., data = New.training, family = 'binomial')
max.formula = formula(max.modelD)



log.foward.delinquint = step(min.modelD, direction='forward', scope=max.formula) # it will print out models in each step
summary(log.foward.delinquint) # it will give you the final model

log.foward.delinquint$Anova
log.foward.delinquint$coefficients

get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}

get.or(summary(min.modelD))
get.or(summary(max.modelD))

get.or(summary(log.foward.delinquint))

under.log.forward <- data.frame(imp = log.foward.delinquint$variable.importance)
df2 <- under.log.forward %>% 
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



get.or(summary(log.foward.delinquint))
summary(log.foward.delinquint)


probabilities <- predict(log.foward.delinquint, under_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.7, 1,0 )

confusionMatrix(as.factor(predicted.classes), as.factor(under_test$six_months_OD1))




####LOG regression
# table(dat$Personal.Loan) # take a look at how many acceptance and rejection first



min.model = glm( Percent_1month_late~ 1, data = New.training, family = 'binomial')
max.model = glm(Percent_1month_late ~ ., data = New.training, family = 'binomial')
max.formula = formula(max.model)

obj.foward = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(obj.foward) # it will give you the final model

get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}

get.or(summary(min.model))
get.or(summary(max.model))

get.or(summary(obj.foward))




# Make predictions cutoff .7

probabilities <- predict(obj.foward, New.training, type = "response")
predicted.classes <- ifelse(probabilities > 0.7, 1,0 )

confusionMatrix(as.factor(predicted.classes), as.factor(New.training$Percent_1month_late))



##############################################################
## what if we want to partition the data and evaluate model ##





# 75% traing, 75 cutoff and late  fee prediction using stepwise dimention reduction 
set.seed(687) 

id.train.glm = sample(1:nrow(final_both_transform), nrow(final_both_transform)*.75)
id.test.glm = setdiff(1:nrow(final_both_transform), id.train.glm) 
dat.train.glm = final_both_transform[id.train.glm,]
dat.test.glm = final_both_transform[id.test.glm,]

min.model2 = glm(Percent_1month_late ~ 1, data = dat.train.glm, family = 'binomial')
max.model2 = glm(Percent_1month_late ~ ., data = dat.train.glm, family = 'binomial')
max.formula = formula(max.model)

obj.steowise.both = step(min.model, direction='both', scope=max.formula) # it will print out models in each step
# it will give you the final stepwise model
summary(obj.steowise.both)
get.or(summary(min.model2))

get.or(summary(max.model2))

get.or(summary(obj.steowise.both))

yhat = predict(obj.steowise.both, newdata = dat.test.glm, type='response')
hist(yhat)

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .1)
err = mean(yhat.class != dat.test.glm$Percent_1month_late) # misclassification error rate
err

table(yhat.class, dat.test.glm$Percent_1month_late)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1) 
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(dat.test.glm$Percent_1month_late, yhat.class)
spe(dat.test.glm$Percent_1month_late, yhat.class)



# Make predictions cutoff .7

probabilities <- predict(obj.steowise.both, dat.test.glm, type = "response")
predicted.classes <- ifelse(probabilities > 0.7, 1,0 )

confusionMatrix(as.factor(predicted.classes), as.factor(dat.test.glm$Percent_1month_late))


# Prediction accuracy
observed.classes <- dat.test.glm$Percent_1month_late
mean(predicted.classes == observed.classes)
