# run_analysis.R

# Input and output files locations.
activity.labels.file <- "raw/unzipped/UCI HAR Dataset/activity_labels.txt"
features.file        <- "raw/unzipped/UCI HAR Dataset/features.txt"
train.subject.file   <- "raw/unzipped/UCI HAR Dataset/train/subject_train.txt"
train.x.file         <- "raw/unzipped/UCI HAR Dataset/train/X_train.txt"
train.y.file         <- "raw/unzipped/UCI HAR Dataset/train/y_train.txt"
test.subject.file    <- "raw/unzipped/UCI HAR Dataset/test/subject_test.txt"
test.x.file          <- "raw/unzipped/UCI HAR Dataset/test/X_test.txt"
test.y.file          <- "raw/unzipped/UCI HAR Dataset/test/y_test.txt"
tidy.data.dir        <- "tidy"
tidy.data.file       <- file.path(tidy.data.dir, "tidy.txt")

# You can run FetchRawData() to get the input files into position.
# source("fetch_raw_data.R")
# FetchRawData()

# Read the activity labels.
# You can visually inspect that the IDs (column 1) runs to 1L to 6L, so we can
# directly use the row number as index to get the label (column 2).
activity.labels <- read.table(activity.labels.file, as.is = T)
# Make the activity names somewhat friendlier to the console.
activity.labels[, 2] <- gsub("_", "-", activity.labels[, 2])  # replace _ with -
activity.labels[, 2] <- tolower(activity.labels[, 2])         # lowercase

# Read the feature names.
# You can verify that the IDs (column 1) is a running sequence from 1L to 561L,
# so we can directly use column 2 as the header for reading x data files. One
# possible method to check the sequence might be: all(1:561 == features[, 1])
features <- read.table(features.file, as.is = T)
# Make the feature names somewhat friendlier to the console.
features[, 2] <- gsub("[()]", "", features[, 2])     # remove ()
features[, 2] <- gsub("^t", "time.", features[, 2])  # spell out time
features[, 2] <- gsub("^f", "freq.", features[, 2])  # spell out freq
features[, 2] <- gsub("[-_,]", ".", features[, 2])   # replace symbols with .
features[, 2] <- tolower(features[, 2])              # lowercase

# Read the data for the training group.
train.subject <- read.table(train.subject.file)
train.y <- read.table(train.y.file)
train.x <- read.table(train.x.file, col.names = features[, 2])
train <- cbind(subject = train.subject[, 1],
               group = "train",
               activity = sapply(train.y[, 1],
                                 function(x) { activity.labels[x, 2] }),
               train.x)

# Read the data for the test group.
test.subject <- read.table(test.subject.file)
test.y <- read.table(test.y.file)
test.x <- read.table(test.x.file, col.names = features[, 2])
test <- cbind(subject = test.subject[, 1],
              group = "test",
              activity = sapply(test.y[, 1],
                                function(x) { activity.labels[x, 2] }),
              test.x)

# Combine the training and test obversations.
combined <- rbind(train, test)

# Convert categorical variables to factors.
combined$group    <- factor(combined$group)
combined$activity <- factor(combined$activity)

# Create a new, independent tidy data set.
# Pick only the mean/std columns and calculate their means by subject/activity.
library(dplyr)
tidy <- combined %>%
  select(subject, activity, matches("\\.(std|mean)(\\.|$)")) %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# Write the tidy data set to disk.
if (!file.exists(tidy.data.dir)) {
  dir.create(tidy.data.dir)
}
write.table(tidy, tidy.data.file, row.names = F)
