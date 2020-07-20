adult <- read.csv("adult_sal.csv")
library(dplyr)
adult <- select(adult,-X)

# Use Table to check out the frequency of the type_employer Column
table(adult$type_employer)

# Data Cleaning
# Combine the Never-worked & Without-pay employer type
unemp <- function(job){
  job <- as.character(job)
  if(job == 'Never-worked' | job == 'Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

##Applying the function
adult$type_employer <- sapply(adult$type_employer,unemp)

## Combine the Self Employed & State and Local
group_emp <- function(job){
  if(job == 'Local-gov' | job == 'State-gov'){
    return('SL-gov')
  }else if(job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self-emp')
  }else {
    return(job)
  }
}

## Applying the function
adult$type_employer <- sapply(adult$type_employer,group_emp)

# Grouping Marital Status columns
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not Married
  if(mar == 'Separated' | mar == 'Divorced' | mar == 'Widowed'){
    return('Not-Married')
    
    # Never-Married
  }else if (mar == 'Never-married'){
    return(mar)
    
    # Married
  }else {
    return('Married')
  }
}

## Applying the marital function

adult$marital <- sapply(adult$marital,group_marital)

## Reducing the factor level 
# Grouping the country columns by continents

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

### Function to group the countries
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

# Applying the function
adult$country <- sapply(adult$country,group_country)


# Also can be done by calling factor function like
# adult$country <- factor(adult$country)


### Treating Missing Values

library(Amelia)

## To convert all the '?' char in the table to NA 
adult[adult=='?']  <- NA

### Re converting the above grouped columns to factor
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

# Using Amelia library to visualize the missing value
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


# Drop missing data
adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

library(ggplot2)
library(dplyr)

# Performing Exploratory Data Analysis on various parameters

## Graph 1
pl <- ggplot(adult, aes(age)) + geom_histogram(aes(fill=income),color='black', binwidth = 1) + theme_bw()

## Graph 2
pl1 <- ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()

# Renaming the country column to Regions since we grouped the countries
adult <- rename(adult,region = country)

## Graph 3
pl2 <- ggplot(adult, aes(region)) + geom_bar(aes(fill=income),color='black') + theme_bw()



### Logistic Regression Model ###

# We'll do the train - test split
# Import Library
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

model = glm(as.factor(income) ~ ., family = binomial(logit), data = train)

## Since there are lot of features which we dont require we can make use of step()

new.step.model <- step(model)


## Lets create the confusion matrix now
print("Confusion Matrix")
test$predicted.income <- predict(model, newdata = test, type = 'response')
print(table(test$income,test$predicted.income > 0.5))




  








