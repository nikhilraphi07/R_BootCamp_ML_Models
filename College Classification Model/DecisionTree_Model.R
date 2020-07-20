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
### Build the Decision Tree ###
###############################


library(rpart)

# Train the model
tree <- rpart(Private ~ ., method = 'class', data=train)
tree.preds <- predict(tree,test)


### Turn these two columns into one column to match the original Yes/No Label for a Private column.
tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if(x >= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.preds$Private <- sapply(tree.preds$Yes, joiner)
print(head(tree.preds))

## Confusion Matrix
table(tree.preds$Private,test$Private)

## Use the rpart.plot library and the prp() function to plot our tree model.
library(rpart.plot)
prp(tree)



