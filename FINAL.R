library(dplyr)
library(readxl)
library(plyr)
library(caret)
library(mlbench)
library(corrplot)
library(moments)
library(funModeling)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(psych)
library(caret)
library(e1071)

# Importing Data Set
file <- read.csv("D:/EXCELR/Project/Insurance Dataset .csv", na.strings = c("NA",""))
View(file)
attach(file)

###################### EDA ######################

# checking for total missing value present in data set
sum(is.na(file))

#summary of dataset 
summary(file)

#structure of dataset
str(file)

#finding missing values for each column
sum(is.na(Area_Service))
sum(is.na(Hospital.County))
sum(is.na(Certificate_num))
sum(is.na(Hospital.Id))
sum(is.na(Hospital.Name))
sum(is.na(Age))
sum(is.na(zip_code_3_digits))
sum(is.na(Gender))
sum(is.na(Cultural_group))
sum(is.na(ethnicity))
sum(is.na(Days_spend_hsptl))
sum(is.na(Admission_type))
sum(is.na(Home.or.self.care.))
sum(is.na(year_discharge))
sum(is.na(ccs_diagnosis_code))
sum(is.na(ccs_diagnosis_description))
sum(is.na(apr_drg_description))
sum(is.na(apr_mdc_description))
sum(is.na(Code_illness))
sum(is.na(Description_illness))
sum(is.na(Mortality.risk))
sum(is.na(Surg_Description))
sum(is.na(Payment_typology_1))
sum(is.na(payment_typology_2))
sum(is.na(payment_typology_3))
sum(is.na(Weight_baby))
sum(is.na(Abortion))
sum(is.na(Emergency.dept_yes.No))
sum(is.na(Tot_charg))
sum(is.na(Tot_cost))
sum(is.na(ratio_of_total_costs_to_total_charges))
sum(is.na(Result))
## function defined for finding mode
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
}

# replacing missing values with respective mode

file$Area_Service[which(is.na(file$Area_Service))] <- getmode(file$Area_Service)

file$Hospital.County[which(is.na(file$Hospital.County))] <- getmode(file$Hospital.County)

file$Certificate_num[which(is.na(file$Certificate_num))] <- getmode(file$Certificate_num)

file$Hospital.Id[which(is.na(file$Hospital.Id))] <- getmode(file$Hospital.Id)

file$zip_code_3_digits[which(is.na(file$zip_code_3_digits))] <- getmode(file$zip_code_3_digits)

file$Description_illness[which(is.na(file$Description_illness))] <- getmode(file$Description_illness)

file$Mortality.risk[which(is.na(file$Mortality.risk))] <- getmode(file$Mortality.risk)

file$payment_typology_2[which(is.na(file$payment_typology_2))] <- getmode(na.omit(file$payment_typology_2))

file$payment_typology_3[which(is.na(file$payment_typology_3))] <- getmode(na.omit(file$payment_typology_3))

sum(is.na(file))


duplicated(file)
###removing duplicated values
data <- unique(file)
attach(data)


#2nd moment business decision
#measures of dispersion
var(Certificate_num)
var(Hospital.Id)
var(year_discharge)
var(ccs_diagnosis_code)
var(ccs_procedure_code)
var(Code_illness)
var(Weight_baby)
var(Tot_charg)
var(Tot_cost)
var(ratio_of_total_costs_to_total_charges)
sd(Certificate_num)
sd(Hospital.Id)
sd(year_discharge)
sd(ccs_diagnosis_code)
sd(ccs_procedure_code)
sd(Code_illness)
sd(Weight_baby)
sd(Tot_charg)
sd(Tot_cost)
sd(ratio_of_total_costs_to_total_charges)
range(Certificate_num)
range(Hospital.Id)
range(year_discharge)
range(ccs_diagnosis_code)
range(ccs_procedure_code)
range(Code_illness)
range(Weight_baby)
range(Tot_charg)
range(Tot_cost)
range(ratio_of_total_costs_to_total_charges)

#3rd moment bussiness decision
skewness(Certificate_num)
skewness(Hospital.Id)
skewness(ccs_diagnosis_code)
skewness(ccs_procedure_code)
skewness(Code_illness)
skewness(Weight_baby)
skewness(Tot_charg)
skewness(Tot_cost)
skewness(ratio_of_total_costs_to_total_charges)

#4th moment business decision
kurtosis(Certificate_num)
kurtosis(Hospital.Id)
kurtosis(ccs_diagnosis_code)
kurtosis(ccs_procedure_code)
kurtosis(Code_illness)
kurtosis(Weight_baby)
kurtosis(Tot_charg)
kurtosis(Tot_cost)
kurtosis(ratio_of_total_costs_to_total_charges)

## Pairplot
plot(Area_Service,Result,xlab= "Area_Service",ylab = "Result", col = "violet")
plot(Hospital.County,Result)
plot(Hospital.Name,Result)
plot(Age,Result)
plot(zip_code_3_digits,Result)
plot(Gender,Result)
plot(Cultural_group,Result)
plot(ethnicity,Result)
plot(Admission_type,Result)
plot(Home.or.self.care.,Result)
plot(ccs_diagnosis_description,Result)
plot(ccs_procedure_description,Result)
plot(apr_drg_description,Result)
plot(apr_mdc_description,Result)
plot(Description_illness,Result)
plot(Mortality.risk,Result)
plot(Surg_Description,Result)
plot(Payment_typology_1,Result)
plot(payment_typology_2,Result)
plot(payment_typology_3,Result)
plot(Abortion,Result)
plot(Emergency.dept_yes.No,Result)

#Graphical Representations
hist(Certificate_num)
hist(Hospital.Id)
hist(year_discharge)
hist(ccs_diagnosis_code)
hist(ccs_procedure_code)
hist(Code_illness)
hist(Weight_baby)
hist(Tot_charg)
hist(Tot_cost)
hist(ratio_of_total_costs_to_total_charges)

boxplot(Certificate_num)
boxplot(Hospital.Id)
boxplot(year_discharge)
boxplot(ccs_diagnosis_code)
boxplot(ccs_procedure_code)
boxplot(Code_illness)
boxplot(Weight_baby)
boxplot(Tot_charg)
boxplot(Tot_cost)
boxplot(ratio_of_total_costs_to_total_charges)
library(funModeling)
library(tidyverse)
library(Hmisc)
windows()

## frequency plot
#freq(file)
freq(file$Area_Service)
freq(file$Hospital.County)
freq(file$Certificate_num)
freq(file$Hospital.Id)
freq(file$Hospital.Name)
freq(file$Age)
freq(file$zip_code_3_digits)
freq(file$Gender)
freq(file$Cultural_group)
freq(file$ethnicity)
freq(file$Days_spend_hsptl)
freq(file$Admission_type)
freq(file$Home.or.self.care.)
freq(file$year_discharge)
freq(file$ccs_diagnosis_code)
freq(file$ccs_diagnosis_description)
freq(file$ccs_procedure_code)
freq(file$ccs_procedure_description)
freq(file$apr_drg_description)
freq(file$apr_mdc_description)
freq(file$Code_illness)
freq(file$Description_illness)
freq(file$Mortality.risk)
freq(file$Surg_Description)
freq(file$Payment_typology_1)
freq(file$payment_typology_2)
freq(file$payment_typology_3)
freq(file$Weight_baby)
freq(file$Abortion)
freq(file$Emergency.dept_yes.No)
freq(file$Tot_charg)
freq(file$Tot_cost)
freq(file$ratio_of_total_costs_to_total_charges)

# comparison graph
HC<- ggplot(file,aes(Hospital.Name))
HC + geom_bar(aes(fill=Result),width = .8) +  theme(axis.text.x = element_text(angle = 50,vjust = 0.5)) + labs(title = "histogram",subtitle = "with age")

table(Area_Service,Result)
table(Hospital.County,Result)
table(Certificate_num,Result)
table(Age,Result)
table(zip_code_3_digits,Result)
table(Gender,Result)
table(Cultural_group,Result)
table(ethnicity,Result)
table(Days_spend_hsptl,Result)
table(Admission_type,Result)
table(Home.or.self.care.,Result)
table(year_discharge,Result)
table(ccs_diagnosis_code,Result)
table(ccs_diagnosis_description,Result)
table(ccs_procedure_code,Result)
table(ccs_procedure_description,Result)
table(apr_drg_description,Result)
table(apr_mdc_description,Result)
table(Code_illness,Result)
table(Description_illness,Result)
table(Mortality.risk,Result)
table(Surg_Description,Result)
table(Payment_typology_1,Result)
table(payment_typology_2,Result)
table(payment_typology_3,Result)
table(Weight_baby,Result)
table(Abortion,Result)
table(Emergency.dept_yes.No,Result)
table(Tot_charg,Result)
table(Tot_cost,Result)
table(ratio_of_total_costs_to_total_charges,Result)

#### Feature Selection###

file_new <- file[,-c(2,3,4,5,6,7,11,14,15,16,17,18,19,20,22,25,26,27,28,32)]

file_new$Area_Service <- as.numeric(file_new$Area_Service)
file_new$Gender <- as.numeric(file_new$Gender)
file_new$Cultural_group <- as.numeric(file_new$Cultural_group)
file_new$ethnicity <- as.numeric(file_new$ethnicity)
file_new$Admission_type <- as.numeric(file_new$Admission_type)
file_new$Home.or.self.care. <- as.numeric(file_new$Home.or.self.care.)
file_new$Mortality.risk <- as.numeric(file_new$Mortality.risk)
file_new$Surg_Description <- as.numeric(file_new$Surg_Description)
file_new$Abortion <- as.numeric(file_new$Abortion)
file_new$Emergency.dept_yes.No <- as.numeric(file_new$Emergency.dept_yes.No)

### normalising 
file_new$Tot_cost <- norm(file_new$Tot_cost)
file_new$ratio_of_total_costs_to_total_charges <- norm(file_new$ratio_of_total_costs_to_total_charges)
file_new$Tot_charg <- norm(test1$Tot_charg)
file_new$ratio_of_total_costs_to_total_charges <- norm(file_new$ratio_of_total_costs_to_total_charges)

#splitting the data into train and test
set.seed(111)
ind <- sample(2,nrow(file_new),replace = T, prob = c(0.7,0.3))
train <- file[ind==1,]
test <- file[ind==2,]

library(ROSE)

#for under sampling
data_balanced_under <- ovun.sample(Result ~ ., data = train, method = "under", p=0.5, seed = 1)$data
table(data_balanced_under$Result)

#for over sampling
data_balanced_over <- ovun.sample(Result ~ ., data = train, method = "over", p=0.5, seed = 1)$data
table(data_balanced_under$Result)

#for both over and under sampling
data_balanced_both <- ovun.sample(Result~., data = train, method = "both", p=0.5, seed = 1)$data
table(data_balanced_under$Result)

library(e1071)

library(naivebayes)# for model building

library(caret)# for confusion matrix

## NB Model##
#model for undersampling techniques
#model for laplace value = 4
model.naive <- naiveBayes(Result~.,data = data_balanced_under,laplace = 4)
model.naive 
model.predict <- predict(model.naive,test)
mean(model.predict==test$Result)
confusionMatrix(model.predict,test$Result)
summary(model.naive)


