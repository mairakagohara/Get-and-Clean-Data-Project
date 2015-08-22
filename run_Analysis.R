## First I am going to read the features.txt into R and transform it in a vector so it can be used to name my columns

setwd("/Users/mairakagohara/datasciencecoursera/Get and Clean Data/Course Project/UCI HAR Dataset")
feat_names <- read.table("features.txt")
feat_names <- feat_names[,2]
feat_names <- as.vector (feat_names, mode = "character")

## Now I am going to read the files from the Test group and merge them into one data set

setwd("/Users/mairakagohara/datasciencecoursera/Get and Clean Data/Course Project/UCI HAR Dataset/test")
test1 <- read.table("X_test.txt", col.names = feat_names) ## first I read the training set and labeled it
test1 <- mutate(test1, id = 1:2947) ##creating an ID column to merge the activity and the tests in a easier way.
activity_test <- read.table("y_test.txt", col.names = c("Activity"))
activity_test <- mutate(activity_test, id = 1:2947)
test2 <- merge(test1, activity_test, by.x = "id", by.y = "id")
subject_test <- read.table("subject_test.txt", col.names = c("Subject"))
subject_test <- mutate (subject_test, id = 1:2947)
test3 <- merge (test2, subject_test, by.x = "id", by.y = "id")
rm(test2)
rm(test1)

## Now I am going to do the same procedure of reading and putting an ID for the Train group
setwd("/Users/mairakagohara/datasciencecoursera/Get and Clean Data/Course Project/UCI HAR Dataset/train")
train1 <- read.table("X_train.txt", col.names = feat_names)
train1 <- mutate(train1, id = 1:7352)
activity_train <- read.table("y_train.txt", col.names = c("Activity"))
activity_train <- mutate (activity_train, id = 1:7352)
train2 <- merge (train1, activity_train, by.x = "id", by.y = "id")
subject_train <- read.table ("subject_train.txt", col.names = c("Subject"))
subject_train <- mutate (subject_train, id = 1:7352)
train3 <- merge (train2, subject_train, by.x = "id", by.y = "id")
rm (train1)
rm(train2)

all <- rbind(train3, test3) ##Add the rows of the two data frames in one
all <- select (all, -id) ## Remove the ID column no longer needed
## That's the end of step 1 and 4 of the instruction

## Step 2: extracting the mean and std deviation measurements. 
## Using the select function and making it select the means and standard deviations along with the subject and the activity
means_stds <- select(all, contains( "std"), contains("mean"), contains("Subject"), contains("Activity"))

##Step 3: creating a function to use lapply to change the number of the activity into the name

rename <- function(x){
  if (x ==1 ){
    "WALKING"
  }
  else if (x == 2){
    "WALKING_UPSTAIRS"
  }
  else if (x == 3){
    "WALKING_DOWNSTAIRS"
  }
  else if (x == 4){
    "SITTING"
  }
  else if (x == 5){
    "STANDING"
  }
  else if (x == 6){
    "LAYING"
  }
}
activity1 <- means_stds [ ,88] ## separating column of activity
activity_name <- sapply(activity1, rename)
Activity <- matrix(data = activity_name, nrow = 10299, ncol = 1, dimnames = list(NULL,"Activity"))
Activity <- as.data.frame(Activity)
means_stds2 <- select(means_stds, -Activity) ##Excluding the Activity number column
means_stds2 <- mutate (means_stds2, id = 1:10299)
Activity <- mutate (Activity, id = 1:10299)
dbcomplete <- merge(means_stds2, Activity, by.x = "id", by.y = "id") ## Database complete with only means and standard deviations and with all Activities labeled.

## Step 5: tidy data set with average of each Activity for each Subject
group <- group_by (dbcomplete, Subject, Activity)
tidy_data <- summarise_each(group, funs(mean))
