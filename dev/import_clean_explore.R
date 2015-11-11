### Potentially useful code


## Prepare

# Libraries
library(lubridate)
library(ggplot2)

# Variables
dir.wd <- "~/git/umcu_hackathon"
dir.data <- paste(dir.wd, "/data", sep="")
dir.output <- paste(dir.wd, "/output", sep="")
file.data1 <- "data1.csv"
#file.data2 <- "data2.csv"
#url.data1 <- ""

# Workdir
setwd(dir.wd)


## TODO: Import
df.data1.raw <- read.csv(paste(dir.data, "/", file.data1, sep=""))
#df.data2.raw <- read.csv(paste(dir.data, "/", file.data2, sep=""))



# TODO: Data Cleaning

#df.prices$Date <- mdy_hms(df.prices$Date)

# TODO: Exploratory analysis -> data set, plots
# TODO: Combine multiple data sets (based on common variable, such as time)
# TODO: Find correlations between variables
# TODO: Machine Learning -> choose variable to predict and apply ML algorithms


