# Importing the required libraries
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

for(i in 1:k)
{
 
 # Splitting the dataset into training and testing observations
 s1=((i-1)*n+1)
 s2=(i*n)
 train=sample(s1:s2)
 shells.train=shells[train,]
 shells.test=shells[-train,"Age"]
 
 # Fitting the random forest model
 rf.shells=randomForest(Age~.,data=shells ,subset=train, mtry=3, importance
=TRUE)
 yhat.rf = predict(rf.shells, newdata=shells[-train,])
 
 # Storing and printing the test errors for each fold
 ts_err[i] <- mean((yhat.rf-shells.test)^2)
 print(paste("Mean Test Error for fold", i, ":", ts_err[i]))
}

# Printing the overall mean test error
print(paste("Overall Mean Test Error:", mean(ts_err)))
