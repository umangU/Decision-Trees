#Importing the required libraries
library(e1071)
library(tree)
library(randomForest)

# Reading the shells dataset
shells <- read_xlsx(file.choose())

# Storing the number of folds
k=10
n=floor(nrow(shells)/k)
i=1

# Creating a vector to store the error values
ts_err=vector(,k)

# Looping through the k-folds
for(i in 1:k)
{
 
 # Spliting the dataset into training and testing dataset
 s1=((i-1)*n+1)
 s2=(i*n)
 train=sample(s1:s2)
 shells.train=shells[train,]
 shells.test=shells[-train,"Age"]
 
 # Fitting the Bagging Model
 rf.shells=randomForest(Age~.,data=shells ,subset=train, mtry=3, ntree=25)
 
 # Making predictions for the testing dataset
 yhat.rf = predict(rf.shells, newdata=shells[-train,])
 
 # Storing the test error for each fold
 ts_err[i] <- mean((yhat.rf-shells.test)^2)
 print(paste("Mean Test Error for fold", i, ":", ts_err[i]))
}

# Printing the overall mean test error
print(paste("Overall Mean Test Error:", mean(ts_err)))
