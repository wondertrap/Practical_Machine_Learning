# coursera - Practical Machine Learning - Project Writeup

# Libraries
library(caret)
library(rpart)
library(randomForest)

# Preparation
path.dir <- getwd()
path.train <- paste0(path.dir,"/data/pml-training.csv")
path.test <- paste0(path.dir,"/data/pml-testing.csv")
path.results <- paste0(path.dir,"/results/")

# Read Data

# Loading the training data set into my R session replacing all missing with "NA"
train <- read.csv(path.train, na.strings=c("NA","#DIV/0!", ""))
test <- read.csv(path.test, na.strings=c("NA","#DIV/0!", ""))

# Cleaning Data

# There are a lot of missing values in data
# remove all columns with sum(NA)=0
train<-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]

# Some columns are irrelevant for this assigment
# remove col 1-7
train   <-train[,-c(1:7)]
test <-test[,-c(1:7)]

# Check visually training data
train.tab <- table(train$classe)


# Preprocessing

# Partionating traingdata for cross-value validation from training set
#  70% subtrain, 30% subtest on train$class
set.seed(777)
dataPart <- createDataPartition(train$classe, p = 0.7, list = FALSE)
subTrain <- train[dataPart, ]
subTest <- train[-dataPart, ]

# First Prediction: Decision Tree
model1 <- rpart(classe ~ ., data=subTrain, method="class")

# Predicting:
prediction1 <- predict(model1, subTest, type = "class")

# Test results on subTest data set:
cMat1<-confusionMatrix(prediction1, subTest$classe)

# Second Prediction: random Forrests
model2 <- randomForest(classe ~. , data=subTrain, method="class")

# Predicting:
prediction2 <- predict(model2, subTest, type = "class")

# Test results on subTest data set:
cMat2<-confusionMatrix(prediction2, subTest$classe)


# Final prediction with test data set
predictionF < - predict(model2,test, type ="class")

# Create Coursera submission files

pml_write_files = function(x, pathdir){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=file.path(pathdir, filename),quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(predictionF, path.results)

