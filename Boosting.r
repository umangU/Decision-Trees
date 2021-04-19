# Importing the required libraries
library(e1071)
library(tree)
library(gbm)

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
 
 # Splitting the dataset into training and testing observations
 s1=((i-1)*n+1)
 s2=(i*n)
 train=sample(s1:s2)
 shells.train=shells[train,]
 shells.test=shells[-train,"Age"]
 
 # Fitting the Boosting Model
 boost.shells=gbm(Age~.,data=shells[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
 yhat.shells=predict(boost.shells, newdata=shells[-train,], n.trees =5000)
 
 # Storing and Printing the test error for each fold
 ts_err[i] <- mean((yhat.shells-shells.test)^2)
 print(paste("Mean Test Error for fold", i, ":", ts_err[i]))
}

# Printing the overall mean test error
print(paste("Overall Mean Test Error:", mean(ts_err)))

# Printing the relative influential plots and statistics
summary(boost.shells)
