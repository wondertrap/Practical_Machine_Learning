Goal
=
Predict the classe variable of the testing data set.

Code and Results
=
# Libraries
Loading selected libraries.
    
    library(caret)
    library(rpart)
    library(randomForest)


# Preparation
Get local working directory path and set data path.

    path.dir <- getwd()
    path.train <- paste0(path.dir,"/data/pml-training.csv")
    path.test <- paste0(path.dir,"/data/pml-testing.csv") 
    path.results <- paste0(path.dir,"/results/")
    
# Read Data

Loading the training data set into my R session replacing all missing with "NA"

    train <- read.csv(path.train, na.strings=c("NA","#DIV/0!", ""))
    test <- read.csv(path.test, na.strings=c("NA","#DIV/0!", ""))
    
# Cleaning Data
There are a lot of missing values in data remove all columns with sum(NA)=0

    train<-train[,colSums(is.na(train)) == 0]
    test <-test[,colSums(is.na(test)) == 0]
    
Some columns are irrelevant for this assigment remove col 1-7

    train   <-train[,-c(1:7)]
    test <-test[,-c(1:7)]

# Check visually training data
train.tab <- table(train$classe)

       A	B	C	D	E 
    5580 3797 3422 3216 3607 

# Preprocessing
Partionating traingdata for cross-value validation from training set 70% subtrain, 30% subtest on train$class

    set.seed(777)
    dataPart <- createDataPartition(train$classe, p = 0.7, list = FALSE)

subTrain <- train[dataPart, ]
subTest <- train[-dataPart, ]

# First Prediction: Decision Tree

We starting with Decision Tree from the rpart Package:
    
    model1 <- rpart(classe ~ ., data=subTrain, method="class")
    
Our predicting is :
    
    prediction1 <- predict(model1, subTest, type = "class")
    
With this results on subTest data set:

    cMat1<-confusionMatrix(prediction1, subTest$classe)

    Confusion Matrix and Statistics
    
      Reference
    PredictionABCDE
     A 1394  177   25   76   59
     B  107  727  170   62  166
     C   88   97  726  115   80
     D   72   93  103  697   99
     E   13   45    2   14  678
    
    Overall Statistics
      
               Accuracy : 0.7174  
                 95% CI : (0.7057, 0.7289)
    No Information Rate : 0.2845  
    P-Value [Acc > NIR] : < 2.2e-16   
      
                     Kappa : 0.6423  
    Mcnemar's Test P-Value : < 2.2e-16   
    

# Second Prediction: random Forests
Next model are random Forests:
    
    model2 <- randomForest(classe ~. , data=subTrain, method="class")
    
Our predicting is:
    
    prediction2 <- predict(model2, subTest, type = "class")
    
And our results on subTest data set are:

    cMat2<-confusionMatrix(prediction2, subTest$classe)
 
    Confusion Matrix and Statistics
    
      Reference
    Prediction A    B    C    D    E
     A      1672    6    0    0    0
     B         0 1130    4    0    0
     C         2    3 1022   10    2
     D         0    0    0  954    3
     E         0    0    0    0 1077
    
    Overall Statistics
      
       Accuracy         : 0.9949  
     95% CI             : (0.9927, 0.9966)
    No Information Rate : 0.2845  
    P-Value [Acc > NIR] : < 2.2e-16   
      
      Kappa                 : 0.9936  
     Mcnemar's Test P-Value : NA  
    
#Decision
From our models the random forest algorithm performed much better than decision trees.
Accuracy for RF was 0.09949 and the accurace fpr DT was 0.7174 (both with 95% CI).

For our final prediction we choose the random forest model.

The expected out-of-sample error is estimated at 0.005, or 0.5%. The expected out-of-sample error is calculated as 1 - accuracy for predictions made against the cross-validation set.

#Submission
Final prediction with test data set (part of the assigment):

    predictionF < - predict(model2,test, type ="class")
    
Create Coursera submission files

    pml_write_files = function(x, pathdir){
    n = length(x)
    for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=file.path(pathdir, filename),quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
    }
    pml_write_files(predictionF, path.results)


# Conclusion
The Random Forrest algorithm predict very well our assigment.    