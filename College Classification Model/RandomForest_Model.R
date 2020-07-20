library(ISLR)

df <- College

## Performing Exploratory Data Analysis

library(ggplot2)

## Graph Plot 1
pl <- ggplot(df, aes(x=Room.Board,y=Grad.Rate)) + geom_point(aes(color=Private), size = 4, alpha = 0.5)

## Graph plot 2
pl1 <- ggplot(df, aes(x=F.Undergrad)) + geom_histogram(aes(fill = Private), color='black', bins = 50, alpha = 0.5) + theme_bw()

## Graph plot 3
pl2 <- ggplot(df, aes(x= Grad.Rate)) + geom_histogram(aes(fill = Private), color='black', bins = 50, alpha = 0.6) + theme_bw()

# On inferring the above graph we could see that one of the colg is having graduation rate more than 100%
## Since this is not possible we need to fix this value

df['Cazenovia College', 'Grad.Rate'] <- 100
subset(df,Grad.Rate > 100)


## Train - Test Split

library(caTools)
set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.70)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)


###############################
### Build the Random Forest ###
###############################

library(randomForest)

rf.model <- randomForest(Private ~ ., data = train, importance = TRUE)

## Lets print the confusion matrix for its own training set
print(rf.model$confusion)


rf.model$importance

## Performing Predictions and displaying Confusion Matrix

rf.preds <- predict(rf.model, test)
print(table(rf.preds,test$Private))





