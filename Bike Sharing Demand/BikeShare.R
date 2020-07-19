# LINEAR REGRESSION PROJECT - SOLUTION

bike <- read.csv('bikeshare.csv')
#print(head(bike))

## Exploratory Data Analysis
library(ggplot2)
library(ggthemes)
library(dplyr)

pl <- ggplot(bike,aes(temp,count)) + geom_point(alpha = 0.3, aes(color=temp)) + theme_bw()
print(pl)

# Convert to Timestamp
bike$datetime <- as.POSIXct(bike$datetime)
pl1 <- ggplot(bike, aes(datetime,count)) + geom_point(aes(color=temp), alpha = 0.5)
pl1 <- pl1 + scale_color_continuous(low='#55D8CE',high='#FF6E2E') + theme_bw()
print(pl)

# To find the correlation between temp and count
cor.bike <- cor(bike[,c('temp','count')])
#print(cor.bike)

# Continuing to explore the seasonal data
# Mapping a boxplot with y axis for the count and x axis for the season

pl2 <- ggplot(bike, aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) + theme_bw()
print(pl2)

# Feature engineering
bike$hour <- sapply(bike$datetime, function(x){format(x,"%H")})
bike$hour <- sapply(bike$hour, as.numeric)

# Scatter plot of count vs hour for Working Day
pl3 <- ggplot(filter(bike,workingday==1),aes(hour,count))
pl3 <- pl3 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp), alpha = 0.5)
pl3 <- pl3 + scale_color_gradientn(colors = c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))
print(pl3 + theme_bw())

# Scatter plot of count vs hour for Non- Working Day
pl3 <- ggplot(filter(bike,workingday==0),aes(hour,count))
pl3 <- pl3 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp), alpha = 0.5)
pl3 <- pl3 + scale_color_gradientn(colors = c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))
print(pl3 + theme_bw())

# Build Model
model <- lm(count ~ . -casual -registered -datetime - atemp ,bike)
print(summary(model))




