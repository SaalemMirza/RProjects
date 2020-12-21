#Read the csv file data
adult <- read.csv(file = 'adult_sal.csv',sep = ',')
head(adult)

#Load the dplyr library
library(dplyr)
#Remove the unnecessary column X
adult <- subset(adult,select = -X)

#Review the structure of the data-frame
str(adult)

table(adult$type_employer)

#-------------------------------------------------#
#---------------DATA CLEANING---------------------#
#-------------------------------------------------#

#A function for filtering grouping Unemployed
unemp <- function(job){
  job <- as.character(job)
  if(job == 'Never-worked'|| job == 'Without-pay'){
    return('Unemployed')
  }
  else
    return(job)
}

#Applying the function to type of employment column
adult$type_employer <- sapply(adult$type_employer,unemp)

#A function for grouping job types
emp_group <- function(job){
  #job <- as.character(job)
  if(job == 'Local-gov'|| job == 'State-gov'){
    return('SL-gov')
  }
  else if (job == 'Self-emp-inc'|| job == 'Self-emp-not-inc'){
    return('Self-Emp')
  }
  else{
    return(job)
  }
}

#Applying the function to type of employment column
adult$type_employer <- sapply(adult$type_employer,emp_group)

#Review the breakdown of marital column
table(adult$marital)

#A function for grouping marital status
marital_func <- function(status){
  status <- as.character(status)
  if(status == 'Divorced' || status == 'Separated' || status == 'Widowed'){
    return('Not Married')
  }
  else if (status == 'Never-married'){
    return('Never Married')
  }
  else
    return('Married')
}

#Applying the function to marital column
adult$marital <- sapply(adult$marital,marital_func)

#Check the levels of country for creating country vectors
levels(adult$country)

#Creating continent vectors
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

#A function for grouping continents
continent <- function(country){
  if(country %in% Asia){
    return('Asia')
  }
  else if (country %in% North.America){
    return('North America')
  }
  else if(country %in% Europe){
    return('Europe')
  }
  else if(country %in% Latin.and.South.America){
    return('Latin & South America')
  }
  else 
    return('Others')
}

#Applying the function to country column
adult$country <- sapply(adult$country,continent)

#Review the structure of the country column
table(adult$country)

str(adult)

#Factorize the columns
adult$type_employer <- sapply(adult$type_employer,factor)
adult$marital <- sapply(adult$marital,factor)
adult$country <- sapply(adult$country,factor)
adult$income <- sapply(adult$income,factor)
adult$occupation <- sapply(adult$occupation,factor)

#Renaming the column
colnames(adult)[colnames(adult)=='country'] <- 'continent'

#Uncomment the below line if Amelia package is not installed
#install.packages('Amelia',repos = 'http://cran.us.r-project.org')
#Load the library
library(Amelia)

#Replace '?' with NA
adult[adult == '?'] <- NA
table(adult$type_employer)

#Plot the map to visualize missing values
missmap(adult)
#Remove the NA values
adult <- na.omit(adult)

#Load the libraries
library(ggplot2)
library(dplyr)

#-------------------------------------------------#
#----DATA EXPLORATORY ANALYSIS BY VISUALIZATION---#
#-------------------------------------------------#

ggplot(adult,aes(x=age))+geom_histogram(aes(fill = income),color = 'black',binwidth = 1)+
  theme_bw()

str(adult)

ggplot(adult,aes(occupation))+geom_bar(aes(fill=income),color= 'black')+
  theme(axis.text.x = element_text(angle=90,hjust=1))

#-------------------------------------------------#
#---------------bUILDING MODEL--------------------#
#-------------------------------------------------#
library(caTools)       

#Set a random seed
set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train <- subset(adult, sample == TRUE)

# Testing Data
test <- subset(adult, sample == FALSE)

# LOGISTIC REGRESSION MODEL
model <- glm(income ~ ., family=binomial(logit), data=train)

summary(model)

# Creating a CONFUSION MATRIX.
test$predicted.income <- predict(model, newdata = test, type = "response")

table <- table(test$income,test$predicted.income>0.5)

confusion.matrix <- as.matrix(table,nrow=2)
print(confusion.matrix)

#Calculating performance measurements

accuracy <- (confusion.matrix[1,1]+confusion.matrix[2,2])/(sum(confusion.matrix))
cat("The accuracy of this model is: ", accuracy*100,"%")

recall <- (confusion.matrix[1,1])/(confusion.matrix[1,1]+confusion.matrix[1,2])
cat("The recall percentage of this model is: ", recall*100,"%")

precision <- (confusion.matrix[1,1])/(confusion.matrix[1,1]+confusion.matrix[2,1])
cat("The precision percentage of this model is: ", precision*100,"%")
