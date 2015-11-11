### Code template for data import/cleaning/exploration


## Prepare

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)
#library(sqldf)

# Variables
dirseparator <- "/"
dir.wd <- "~/git/umcu_hackathon"
dir.data <- paste(dir.wd, "data", sep=dirseparator)
dir.output <- paste(dir.wd, "output", sep=dirseparator)
url.data1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
#url.data2 <- "https://"
filepath.data1 <- paste(dir.data, "data1.csv", sep=dirseparator)
#filepath.data2 <- paste(dir.data, "data2.csv", sep=dirseparator)

# Workdir
setwd(dir.wd)


## Import

download.file(url.data1, filepath.data1, method="curl")
#download.file(url.data2, filepath.data2, method="curl")
df.data1.raw <- read.csv(filepath.data1, na.strings=c("","#DIV/0!","NA"))
#df.data2.raw <- read.csv(filepath.data2, na.strings=c("","#DIV/0!","NA"))


## TODO: Data Cleaning

# CURRENT

# df.gdp.raw <- read.csv(file, skip=5, header=FALSE, nrows=190, col.names=c("countrycode", "rank", "V3", "country", "gdp", "V6", "V7", "V8", "V9", "V10"))
# df.gdp <- select(df.gdp.raw, c(1, 2, 4, 5))
# df.gdp$gdp <- as.numeric(sapply(df.gdp$gdp, FUN=function(x) {gsub(x=x, pattern="(,| )", replacement="")}))
# df.prices$Date <- mdy_hms(df.prices$Date)
# names(df.gdp) <- c("countrycode", "ranking", "country", "gdp")
# length(intersect(df.gdp$countrycode, df.edu$CountryCode))
# summarize(group_by(df.merged, Income.Group), mean(ranking))
# table(df.merged.quantiled$rankingquantile, df.merged.quantiled$Income.Group)

# Aggregate mean values for each variable over "activity_name" and "subject" groups
# df.averages <- aggregate(. ~ subject + activity_name, df.selected.set, FUN=mean)
# Sort by "subject" and "activity_name"
# df.averages <- arrange(df.averages, subject, activity_name)

# selected1 <- sqldf("select pwgtp1 from acs where AGEP < 50")
# selected2 <- sqldf("select distinct AGEP from acs")

# Remove columns with only NA values
df.training <- df.training[,colSums(is.na(df.training))<nrow(df.training)]

split_data <- split(clean_data, data_without_na$State)



## TODO: Combine multiple data sets (based on common variable, such as time)

# df.merged <- merge(df.gdp, df.edu, by.x="countrycode", by.y="CountryCode")
# df.merged.sorted <- arrange(df.merged, desc(ranking))


## TODO: Exploratory analysis -> data set, plots

qplot(data=NEI.Baltimore.emmissions.by.type.year, x=year, y=total_emissions, facets=. ~ type, ylab="PM2.5 emissions (tons)", main="Yearly PM2.5 emissions by type in Baltimore, MD") + geom_smooth(method="lm")
qplot(num_window, pitch_belt, data=df.training, colour=classe, pch=user_name)

hist(sample_variances, main=paste("Sample variances (n=", n, ", lambda=", lambda, ")", sep=""), xlab="Sample variance")
boxplot(len ~ dose, data=ToothGrowth, xlab="dose", ylab="tooth length")
qplot(dose, len, data=ToothGrowth, xlab="dose", ylab="tooth length", facets = . ~ supp) + geom_smooth(method="lm")

tapply(ToothGrowth$len, list(supp=ToothGrowth$supp, dose=ToothGrowth$dose), mean)


## TODO: Find correlations between variables

corcoefficient <- cor(nitratedata,sulfatedata)



## Inference

len_OJ <- subset(ToothGrowth, supp=="OJ", len)
len_VC <- subset(ToothGrowth, supp=="VC", len)
t.test(len_VC, len_OJ, paired = FALSE, var.equal = FALSE)$conf


## Regression

anova(fit0, fit1, fit2)
bestmodel <- lm(mpg ~ factor(am) + wt + hp, mtcars)
summary(bestmodel)
par(mfrow=c(1,2))
plot(bestmodel, which=c(1,2))

data(mtcars)
init_model <- lm(mpg ~ am + ., data = mtcars)
best_model <- step(init_model, direction = "both")
summary(best_model)

Multivariate regression:
    * GGally package: ggpairs()

glm(y ~ x, family=..)



## TODO: Machine Learning -> choose variable to predict and apply ML algorithms

training <- subset(segmentationOriginal, Case=='Train', select=-c(Case))
testing <- subset(segmentationOriginal, Case=='Test', select=-c(Case))

set.seed(1234)
modelFit <- train(Class ~ ., method="rpart", data=training)
library(rattle)
fancyRpartPlot(modelFit$finalModel)
modelFit$finalModel


# Divide into training, testing and predicting set
m.train <- createDataPartition(df.train.file$classe, p=p.training, list = FALSE)
df.training <- df.train.file[m.train,]
df.testing <- df.train.file[-m.train,]
df.predicting <- df.predict.file

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
model.rf <- train(factor(y) ~ ., data=vowel.train, method="rf")
model.gbm <- train(factor(y) ~ ., data=vowel.train, method="gbm")
test.predictions.rf <- predict(model.rf, newdata=vowel.test)

# Forecasting
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
bats.model <- bats(tstrain)
forecast <- forecast(bats.model, h=length(testing$visitsTumblr), level=95)
plot(forecast)


# Set selected model and display
model.best <- model.rf.2
model.best
model.best$finalModel

# Confusion table and accuracy for training set
list.training.results <- predict(model.best, newdata=df.training)
table(df.training$classe, list.training.results)
sum(df.training$classe == list.training.results) / length(df.training$classe)

# Confusion table and accuracy for testing set
list.testing.results <- predict(model.best, newdata=df.testing)
table(df.testing$classe, list.testing.results)
sum(df.testing$classe == list.testing.results) / length(df.testing$classe)

# Most important variables
varImp(model.best)

# Save/load model to/from file
saveRDS(model.best, "model_rf.rds")
# model <- readRDS("model_rf.rds")


# Predicting
predict(model.best, df.predicting)