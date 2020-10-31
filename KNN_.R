##################################################### KNN Modeling #######################################################
# Import package
library(class)


# PenDigits data, let's see how well we can predict the handwritten digit using the 16 variables of coordinate information

load("C:/Users/chely/Documents/Fall 2/Data Mining/Data/PenDigits.RData")

#-----------------------------------------------------Prepare Data ------------------------------------------------------#

# Step 1: Determine some form of standardization for each of our variables at the very least (most time-consuming)
# If we were to build our own unique distance function, then lots of exploration and experimentation goes into this step.
# However, this data set is already standardized since digits are all on the exact same scale, from 0 to 100. We can actually go ahead and start applying a model.

#---------------------------------------------Apply Model and Make Predictions ------------------------------------------#
# Step 2:
# Set random seed for reproducibility
set.seed(7515)
# Apply KNN model and make predictions
# Add train/test sets without target and assign target var to cl; k=num of neighbors
pred <- knn(train=train[,1:16], test=test[,1:16], cl=train[,17], k=5)
# Confusion matrix
conf <- table(pred, test[,17])
conf # we did quite well predicting with k=5 neighbors

# Step 3: Compute the actual misclassification rate (sum of off diagonal elements divided by number of test cases)
cat("Misclassification Rate = ", sum(pred!=test[,17])/length(test[,17])) #0.02344197

# Step 4: Optimize the method by choosing a value of k that provides the best results on our test set

# Try values of k from 1 to 20 & create a vector to store the accuracy for each value of k
range = 1:20

accs = rep(0, length(range))

# loop through those values and repeat the steps from above
  for (k in range) {
    pred = knn(train=train[,1:16], test=test[,1:16], cl=train[,17], k=k)
    accs[k] = sum(pred!=test[,17])/length(test[,17])
  }

# Plot the accuracies.
plot(range, accs, xlab = "k", ylab = "Error Rate", type='b', pch=18, 
     main = 'Determining the Optimal k Using Validation Data')
abline(v=which.min(accs), col='red')
text((which.min(accs)+0.5),0.026, paste(which.min(accs)), col='red') # k=3 gives most accurate results

# Step 5: Repeat Step 2 with k=3
pred <- knn(train=train[,1:16], test=test[,1:16], cl=train[,17], k=3)
cat("Misclassification Rate = ", sum(pred!=test[,17])/length(test[,17])) # 0.02144082

# Misclassification rate is slightly better!
