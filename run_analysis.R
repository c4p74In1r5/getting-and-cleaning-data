# Gets the data-set from the given URL
getDataset <- function(url) {
    if (!file.exists("UCI HAR Dataset")) {
        download.file(url, destfile = "Dataset.zip")
        unzip("Dataset.zip")
    }
}

# Replaces the activity number with its respective label
mapActivities <- function(activities, labelsFile) {
    labelsTable <- read.table(labelsFile)
    as.character(factor(activities, labelsTable[, 1], labelsTable[,2]))
}

# Merges the training and the test sets to create one data set
getMergedData <- function() {
    xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
    yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
    subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
    
    xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
    yTest <- read.table("UCI HAR Dataset/test/y_test.txt")
    subTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
    
    X <- rbind(xTrain, xTest)
    y <- rbind(yTrain, yTest)
    subjects <- rbind(subTrain, subTest)
    
    activities <- mapActivities(y[, 1], "UCI HAR Dataset/activity_labels.txt")
    X <- cbind(subjects, activities, X)
    
    X
}

# Appropriately labels the data set with descriptive variable names
cleanData <- function(data) {
    features <- read.table("UCI HAR Dataset/features.txt")[, 2]
    names(data) <- c("subject", "activity", as.character(features))
    requiredFeatures <- features[grep(".*(mean|std)[(].*", features)]
    data <- data[, c("subject", "activity", requiredFeatures)]
    requiredFeatures <- gsub("[(][)]", "", requiredFeatures)
    names(data) <- c("subject", "activity", requiredFeatures)
    data
}

# Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject
getTidyMeanData <- function(data) {
    tidy <- aggregate(
        data[, 3:ncol(data)], 
        by = list(data[, 1], data[, 2]), 
        mean
    )
    names(tidy)[c(1, 2)] <- c("subject", "activity")
    tidy <- tidy[order(tidy$subject, tidy$activity), ]
    row.names(tidy) <- NULL
    tidy
}

# Main program

getDataset("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
X <- getMergedData()
X <- cleanData(X)

tidyData <- getTidyMeanData(X)
write(knitr::kable(tidyData, format = "markdown"), "TidyMeanData.md")

