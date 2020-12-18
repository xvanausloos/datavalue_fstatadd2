# tp6_d a faire en premier R reseau de neurones
# source: https://www.datacamp.com/community/tutorials/neural-network-models-r
# version: 18/12/20

# Ce TP est commenté et reprend de façon synthètique les bases théoriques des réseaux de neurones

#Implementation of a Neural Network in R

# install.packages("neuralnet")

# creating training data set
TKS=c(20,10,30,20,80,30)
CSS=c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df=data.frame(TKS,CSS,Placed)
df

# Let's build a NN classifier model using the neuralnet library.
# First, import the neuralnet library and create NN classifier model by passing argument set of label and features, 
# dataset, number of neurons in hidden layers, and error calculation.
# load library
require(neuralnet)

# fit neural network
nn=neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

# - Placed~TKS+CSS, Placed is label annd TKS and CSS are features.
# - df is dataframe,
# - hidden=3: represents single layer with 3 neurons respectively.
# - act.fct = "logistic" used for smoothing the result.
# - linear.ouput=FALSE: set FALSE for apply act.fct otherwise TRUE

# Plotting Neural Network
# Let's plot your neural net model.
plot(nn)

# Create test dataset
# Create test dataset using two features Technical Knowledge Score and Communication Skills Score
TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)

# Predict the results for the test set
# Predict the probability score for the test data using the compute function.
Predict=compute(nn,test)
Predict$net.result

#Now, Convert probabilities into binary classes.
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

