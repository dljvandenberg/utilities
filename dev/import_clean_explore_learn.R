### Code template for data import/cleaning/exploration


## PREPARE

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)
#library(sqldf)
#library(GGally)

# Variables
dirseparator <- "/"
dir.wd <- "~/git/umcu_hackathon"
dir.data <- paste(dir.wd, "data", sep=dirseparator)
dir.output <- paste(dir.wd, "output", sep=dirseparator)
url.data1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
filepath.data1 <- paste(dir.data, "data1.csv", sep=dirseparator)

# Workdir
setwd(dir.wd)


## IMPORT

download.file(url.data1, filepath.data1, method="curl")
df.data1.raw <- read.csv(filepath.data1, na.strings=c("","#DIV/0!","NA"), skip=5, header=FALSE, nrows=190, col.names=c("countrycode", "rank", "V3", "country", "gdp", "V6", "V7", "V8", "V9", "V10"))
#df.data1.raw <- read.csv(filepath.data1, na.strings=c("","#DIV/0!","NA"))


## CLEAN

# Subsetting
df.data1 <- select(df.data1.raw, c(1, 2, 4, 5))
#len_OJ <- subset(ToothGrowth, supp=="OJ", len)
# Convert to numeric
df.data1$gdp <- as.numeric(sapply(df.data1$gdp, FUN=function(x) {gsub(x=x, pattern="(,| )", replacement="")}))
# Convert to date
#df.data1$date <- mdy_hms(df.data1$date)
# Set column names
#names(df.data1) <- c("countrycode", "ranking", "country", "gdp")
# Remove columns with only NA values
#df.training <- df.training[,colSums(is.na(df.training))<nrow(df.training)]
# SQL queries
#selected1 <- sqldf("select pwgtp1 from acs where AGEP < 50")
#selected2 <- sqldf("select distinct AGEP from acs")
# Sort by "subject" (ascending) and "activity_name" (descending)
#df.averages <- arrange(df.averages, subject, desc(activity_name))
# Split into subsets
#split_data <- split(clean_data, data_without_na$State)
# Merge data sets (based on common variable, such as time)
#df.merged <- merge(df.gdp, df.edu, by.x="countrycode", by.y="CountryCode")


## EXPLORE

# Table comparison
#table(df.merged.quantiled$rankingquantile, df.merged.quantiled$Income.Group)
# Group summary
#summarize(group_by(df.data1, group), mean(gdp))
# Aggregate mean values for each variable over "activity_name" and "subject" groups
#df.averages <- aggregate(. ~ subject + activity_name, df.selected.set, FUN=mean)
# Apply a Function to a Data Frame Split by Factors
#tapply(ToothGrowth$len, list(supp=ToothGrowth$supp, dose=ToothGrowth$dose), mean)
# Find correlations between variables
#corcoefficient <- cor(nitratedata,sulfatedata)

# Plots
#hist(sample_variances, main="Sample variances", xlab="Variance")
#boxplot(len ~ dose, data=ToothGrowth, xlab="dose", ylab="tooth length")
#qplot(dose, len, data=ToothGrowth, xlab="dose", ylab="tooth length", facets = . ~ supp, colour=classe, pch=user_name) + geom_smooth(method="lm")

# Pairwise plots of variables
data(mtcars)
ggpairs(mtcars)


## INFERENCE

# T Test
#t.test(len_VC, len_OJ, paired = FALSE, var.equal = FALSE)


## REGRESSION

# Find best multivariate regression model
init_model <- lm(mpg ~ am + ., data = mtcars)
best_model <- step(init_model, direction = "both")

# Anova model selection
fit1 <- lm(mpg ~ am, data = mtcars)
fit2 <- lm(mpg ~ am + cyl, data = mtcars)
fit3 <- lm(mpg ~ am + cyl + hp, data = mtcars)
anova(fit1, fit2, fit3)

# View model details
summary(best_model)
par(mfrow=c(1,2))
plot(best_model, which=c(1,2))

# Generalized Linear Model
glm(y ~ x, family="binomial")


## MACHINE LEARNING

# Divide into training, testing and predicting set
set.seed(1234)
m.train <- createDataPartition(df.train.file$classe, p=.75, list = FALSE)
df.training <- df.train.file[m.train,]
df.testing <- df.train.file[-m.train,]
df.predicting <- df.predict.file


# TODO_CURRENT

model.rf <- train(factor(y) ~ ., data=vowel.train, method="rf")
model.gbm <- train(factor(y) ~ ., data=vowel.train, method="gbm")
test.predictions.rf <- predict(model.rf, newdata=vowel.test)

modelFit <- train(Class ~ ., method="rpart", data=training)
library(rattle)
fancyRpartPlot(modelFit$finalModel)
modelFit$finalModel


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