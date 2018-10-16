

rm(list =c())


library(h2o)
library(tidyverse)

h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "8G")  #max mem size is the maximum memory to allocate to H2O


localH2O = h2o.init(ip = 'localhost', port = 54321, nthreads = -1,max_mem_size = "8G")


loan_csv <- "/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UICProject/Workbook3.csv"
datachicago <- h2o.importFile(loan_csv)  
dim(datachicago)

head(datachicago)

h2o.names((datachicago))

str(datachicago)




#==========================================================================================================
# Look at  the  structure of the data with the glimpse function in 
#  dplyr  package
#==========================================================================================================

data=data.table::fread("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UICProject/Workbook3.csv")


#==========================================================================================================
# Look at  the  structure of the data with the glimpse function in 
#  dplyr  package
#==========================================================================================================

str(data)

dplyr::glimpse(data)

summary(data)


#==========================================================================================================
#check the number of missing rows
#==========================================================================================================

colSums(is.na.data.frame(data))


data[!complete.cases(data),]


data[which(data$Asian_ancestry!="."),]


data$Asian_ancestry=ifelse(data$Asian_ancestry==".","<NA>",data$Asian_ancestry)


data=data[complete.cases(data),]

#==========================================================================================================
#NO CODED RESPONSE APPLICABLE (SPECIFY)
#==========================================================================================================

#data%>%dplyr::filter(str_detect(Hypertension, "NO CODED RESPONSE APPLICABLE (SPECIFY)"))

ndata=data%>%dplyr::select(-ID)%>%dplyr::filter(Hypertension!="NO CODED RESPONSE APPLICABLE (SPECIFY)",Employment
                                                !="NO CODED RESPONSE APPLICABLE (LEAVE NOTE FIRST)"
                                                ,Hispanic!="NO CODED RESPONSE APPLICABLE (SPECIFY)",
                                                Education!="NO CODED RESPONSE APPLICABLE (SPECIFY)")





ndata=mutate_if(ndata,is.character,as.factor)

str(ndata)

# ndata=mutate(ndata,Gender=as.factor(Gender),Age=as.numeric(Age),Hypertension=as.factor(Hypertension),Employment=as.factor(Employment),
#              Hispanic=as.factor(Hispanic),Education=as.factor(Education),Hispanic_ancestry=as.factor(Hispanic_ancestry),
#              race_white=as.factor(race_white),race_black=as.factor(race_black),race_Asian=as.factor(race_Asian),
#              race_pacific=as.factor(race_pacific),race_Amer_Indian=as.factor(race_Amer_Indian),race_other=as.factor(race_other),
#              race_dontknow=as.factor(race_dontknow),race_refused=as.factor(race_refused),Asian_ancestry=as.factor(Asian_ancestry) )


#newdata%>%select(-race_pacific )%>%glm(Hypertension~., family = "binomial",data=newdata)
str(ndata)

#==========================================================================================================
#
#==========================================================================================================

#ndata$Hypertension=as.factor(if_else(ndata$Hypertension=="Yes",1,0))
head(ndata)

str(ndata)

### select the variables important for prediction by a stepwise logistic regression

ndata=ndata[,c("Age","Hypertension","race_white","Employment")]
#==========================================================================================================
# import R object to the H2O cloud.
#convert r data to h2o object
#==========================================================================================================

datah20=as.h2o(ndata)

str(datah20)


#==========================================================================================================
# Partition the data into training, validation and test sets
#==========================================================================================================

splits <- h2o.splitFrame(data = datah20, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]



# Identify response and predictor variables
y <- "Hypertension"
x <- setdiff(names(datah20), y)  
print(x)


#==========================================================================================================
#glm/logistic
#similar to R's glm, h2o.glm has the family argument
# 1. Let's start with a basic binomial Generalized Linear Model
# By default, h2o.glm uses a regularized, elastic net model
#==========================================================================================================


glm_fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train,
                    model_id = "glm_fit1",
                    family = "binomial") 




#====================================================================================================================================
# Next we will do some automatic tuning by passing in a validation frame and setting 
# `lambda_search = True`.  Since we are training a GLM with regularization, we should 
# try to find the right amount of regularization (to avoid overfitting).  The model 
# parameter, `lambda`, controls the amount of regularization in a GLM model and we can 
# find the optimal value for `lambda` automatically by setting `lambda_search = TRUE` 
# and passing in a validation frame (which is used to evaluate model performance using a 
# particular value of lambda).
#=====================================================================================================================================


glm_fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train,
                    model_id = "glm_fit2",
                    validation_frame = valid,
                    family = "binomial",
                    lambda_search = TRUE)


#==========================================================================================================
# Let's compare the performance of the two GLMs
#==========================================================================================================


glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = test)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = test)



(glm_perf1)  
(glm_perf2) 



#==========================================================================================================
# Instead of printing the entire model performance metrics object, 
# it is probably easier to print just the metric that you are interested in comparing.
# Retreive test set AUC
#==========================================================================================================


h2o.auc(glm_perf1)  
h2o.auc(glm_perf2)  


#==========================================================================================================
# Compare test AUC to the training AUC and validation AUC
#==========================================================================================================


h2o.auc(glm_fit2, train = TRUE)  
h2o.auc(glm_fit2, valid = TRUE) 

glm_fit2@model$validation_metrics  





#==========================================================================================================
# 2. Random Forest
# H2O's Random Forest (RF) implements a distributed version of the standard 
# Random Forest algorithm and variable importance measures.
# First we will train a basic Random Forest model with default parameters. 
# The Random Forest model will infer the response distribution from the response encoding. 
# A seed is required for reproducibility.
#==========================================================================================================


rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit1",
                            seed = 1)


#==========================================================================================================
# Next we will increase the number of trees used in the forest by setting `ntrees = 100`.  
# The default number of trees in an H2O Random Forest is 50, so this RF will be twice as 
# big as the default.  Usually increasing the number of trees in a RF will increase 
# performance as well.  Unlike Gradient Boosting Machines (GBMs), Random Forests are fairly 
# resistant (although not free from) overfitting.
# See the GBM example below for additional guidance on preventing overfitting using H2O's 
# early stopping functionality.
#==========================================================================================================



rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            ntrees = 100,
                            seed = 1)


#==========================================================================================================
# Let's compare the performance of the two RFs
#==========================================================================================================


rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = test)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = test)


#==========================================================================================================
# Print model performance
#==========================================================================================================


rf_perf1
rf_perf2


#==========================================================================================================
# Retreive test set AUC
#==========================================================================================================


h2o.auc(rf_perf1)  
h2o.auc(rf_perf2)  



#==========================================================================================================
# Cross-validate performance
# Rather than using held-out test set to evaluate model performance, a user may wish 
# to estimate model performance using cross-validation. Using the RF algorithm 
# (with default model parameters) as an example, we demonstrate how to perform k-fold 
# cross-validation using H2O. No custom code or loops are required, you simply specify 
# the number of desired folds in the nfolds argument.
# Since we are not going to use a test set here, we can use the original (full) dataset, 
# which we called data rather than the subsampled `train` dataset. Note that this will 
# take approximately k (nfolds) times longer than training a single RF model, since it 
# will train k models in the cross-validation process (trained on n(k-1)/k rows), in 
# addition to the final model trained on the full training_frame dataset with n rows.

#==========================================================================================================


rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit3",
                            seed = 1,
                            nfolds = 5)

#==========================================================================================================
# To evaluate the cross-validated AUC, do the following:
#==========================================================================================================


h2o.auc(rf_fit3, xval = TRUE)  




#==========================================================================================================
# 3. Gradient Boosting Machine
# H2O's Gradient Boosting Machine (GBM) offers a Stochastic GBM, which can 
# increase performance quite a bit compared to the original GBM implementation.

# Now we will train a basic GBM model
# The GBM model will infer the response distribution from the response encoding if not specified 
# explicitly through the `distribution` argument. A seed is required for reproducibility.
#==========================================================================================================


gbm_fit1 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit1",
                    seed = 1)


#==========================================================================================================
# Next we will increase the number of trees used in the GBM by setting `ntrees=500`.  
# The default number of trees in an H2O GBM is 50, so this GBM will trained using ten times 
# the default.  Increasing the number of trees in a GBM is one way to increase performance 
# of the model, however, you have to be careful not to overfit your model to the training data 
# by using too many trees.  To automatically find the optimal number of trees, you must use 
# H2O's early stopping functionality.  This example will not do that, however, the following 
# example will.
#==========================================================================================================


gbm_fit2 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)



#============================================================================================
# We will again set `ntrees = 500`, however, this time we will use early stopping in order to 
# prevent overfitting (from too many trees).  All of H2O's algorithms have early stopping available, 
# however early stopping is not enabled by default (with the exception of Deep Learning).  
# There are several parameters that should be used to control early stopping.  The three that are 
# common to all the algorithms are: `stopping_rounds`, `stopping_metric` and `stopping_tolerance`.  
# The stopping metric is the metric by which you'd like to measure performance, and so we will choose 
# AUC here.  The `score_tree_interval` is a parameter specific to the Random Forest model and the GBM.  
# Setting `score_tree_interval = 5` will score the model after every five trees.  The parameters we 
# have set below specify that the model will stop training after there have been three scoring intervals 
# where the AUC has not increased more than 0.0005.  Since we have specified a validation frame, 
# the stopping tolerance will be computed on validation AUC rather than training AUC. 
#===============================================================================================


gbm_fit3 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "gbm_fit3",
                    validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUC",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)

#==========================================================================================================
# Let's compare the performance of the two GBMs
#==========================================================================================================


gbm_perf1 <- h2o.performance(model = gbm_fit1,
                             newdata = test)
gbm_perf2 <- h2o.performance(model = gbm_fit2,
                             newdata = test)
gbm_perf3 <- h2o.performance(model = gbm_fit3,
                             newdata = test)

#==========================================================================================================
# Print model performance
#==========================================================================================================


gbm_perf1
gbm_perf2
gbm_perf3

#==========================================================================================================
# Retreive test set AUC
#==========================================================================================================


h2o.auc(gbm_perf1)  
h2o.auc(gbm_perf2)  
h2o.auc(gbm_perf3)  



#==========================================================================================================
# To examine the scoring history, use the `scoring_history` method on a trained model.  
# If `score_tree_interval` is not specified, it will score at various intervals, as we can 
# see for `h2o.scoreHistory()` below.  However, regular 5-tree intervals are used 
# for `h2o.scoreHistory()`.  
# The `gbm_fit2` was trained only using a training set (no validation set), so the scoring 
# history is calculated for training set performance metrics only.

#==========================================================================================================


h2o.scoreHistory(gbm_fit2)


#==========================================================================================================
# When early stopping is used, we see that training stopped at 105 trees instead of the full 500.  
# Since we used a validation set in `gbm_fit3`, both training and validation performance metrics 
# are stored in the scoring history object.  Take a look at the validation AUC to observe that the 
# correct stopping tolerance was enforced.

#==========================================================================================================



h2o.scoreHistory(gbm_fit3)



#==========================================================================================================

# Look at scoring history for third GBM model
#==========================================================================================================

plot(gbm_fit3, 
     timestep = "number_of_trees", 
     metric = "AUC")
plot(gbm_fit3, 
     timestep = "number_of_trees", 
     metric = "logloss")




#==========================================================================================================

# 4. Deep Learning
# H2O's Deep Learning algorithm is a multilayer feed-forward artificial neural network.  
# It can also be used to train an autoencoder. In this example we will train 
# a standard supervised prediction model.

# Train a default DL
# First we will train a basic DL model with default parameters. The DL model will infer the response 
# distribution from the response encoding if it is not specified explicitly through the `distribution` 
# argument.  H2O's DL will not be reproducible if it is run on more than a single core, so in this example, 
# the performance metrics below may vary slightly from what you see on your machine.
# In H2O's DL, early stopping is enabled by default, so below, it will use the training set and 
# default stopping parameters to perform early stopping.
#==========================================================================================================


dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit1",
                            seed = 1)

#==========================================================================================================

# Train a DL with new architecture and more epochs.
# Next we will increase the number of epochs used in the GBM by setting `epochs=20` (the default is 10).  
# Increasing the number of epochs in a deep neural net may increase performance of the model, however, 
# you have to be careful not to overfit your model to your training data.  To automatically find the optimal number of epochs, 
# you must use H2O's early stopping functionality.  Unlike the rest of the H2O algorithms, H2O's DL will 
# use early stopping by default, so for comparison we will first turn off early stopping.  We do this in the next example 
# by setting `stopping_rounds=0`.
#==========================================================================================================


dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            epochs = 20,
                            hidden= c(10,10),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)


#==========================================================================================================

# Train a DL with early stopping
# This example will use the same model parameters as `dl_fit2`. This time, we will turn on 
# early stopping and specify the stopping criterion.  We will also pass a validation set, as is
# recommended for early stopping.
#==========================================================================================================


dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit3",
                            validation_frame = valid,  #in DL, early stopping is on by default
                            epochs = 2,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUC",      #used for early stopping
                            stopping_tolerance = 0.0005,  #used for early stopping
                            seed = 1)


#==========================================================================================================

# Let's compare the performance of the three DL models
#==========================================================================================================


dl_perf1 <- h2o.performance(model = dl_fit1,
                            newdata = test)
dl_perf2 <- h2o.performance(model = dl_fit2,
                            newdata = test)
dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = test)


#==========================================================================================================

# Print model performance
#==========================================================================================================


dl_perf1
dl_perf2
dl_perf3


#==========================================================================================================

# Retreive test set AUC
#==========================================================================================================


h2o.auc(dl_perf1)  
h2o.auc(dl_perf2)  
h2o.auc(dl_perf3)  



#==========================================================================================================

# Scoring history
#==========================================================================================================



h2o.scoreHistory(dl_fit3)

#==========================================================================================================

# confusion matrix
#==========================================================================================================


h2o.confusionMatrix(dl_fit3)


#==========================================================================================================

# model diagnostics
#==========================================================================================================

plot(dl_fit3,
     timestep = "epochs",
     metric = "classification_error")


h2o.scoreHistory(dl_fit3)$epochs
h2o.scoreHistory(dl_fit3)$validation_classification_error
#==========================================================================================================

# Look at scoring history for third DL model
#==========================================================================================================
# The model starts to overfitt as epoch goes beyond 2. The training error continues to decrease whereas the 
# test error begins to increase.

plot(dl_fit3, 
     timestep = "epochs", 
     metric = "AUC")

#==========================================================================================================

# # Get the CV models from the `dl_fit3` object for third DL model
#==========================================================================================================



dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit3",
                            validation_frame = valid,  #in DL, early stopping is on by default
                            epochs = 2,
                            nfolds = 3,
                            stopping_metric = "misclassification", #used for early stopping
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 5,          #used for early stopping
                            #stopping_metric = "AUC",      #used for early stopping
                            stopping_tolerance = 0.0005,  #used for early stopping
                            seed = 1)


cv_models <- sapply(dl_fit3@model$cross_validation_models,
                    function(i) h2o.getModel(i$name))

# Plot the scoring history over time
plot(cv_models[[2]],
     timestep = "epochs",
     metric = "classification_error")

plot(dl_fit3,
     timestep = "epochs",
     metric = "classification_error")


cv_models[[1]]

#==========================================================================================================

# Deep Learning Grid Search third DL model
#==========================================================================================================


# As an alternative to manual tuning, or “hand tuning”, we can use the h2o.grid() function to perform either a
# Cartesian or Randon Grid Search (RGS). Random Grid Search is usually a quicker way to find a good model,
# so we will provide a example of how to use H2O’s Random Grid Search on a DNN.
# One handy feature of RGS is that you can specify how long you would like to execute the grid for – this can be
# based on a time, number of models, or a performance-metric-based stopping criterion. In the example below,
# we will train the DNN grid for 600 seconds (10 minutes).
# First define a grid of Deep Learning hyperparamters and specify the search_criteria .


activation_opt <- c("Rectifier", "Maxout", "Tanh")
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)
hyper_params <- list(activation = activation_opt, l1 = l1_opt, l2 = l2_opt)
search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 600)

# Rather than comparing models by using cross-validation (which is “better” but takes longer), we will simply
# partition our training set into two pieces – one for training and one for validiation.
# This will split the train frame into an 80% and 20% partition of the rows.

splits <- h2o.splitFrame(train, ratios = 0.8, seed = 1)

#Train the random grid. Fixed non-default parameters such as hidden=c(20,20) can be passed directly to
#the h2o.grid() function.


dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    grid_id = "dl_grid",
                    training_frame = splits[[1]],
                    validation_frame = splits[[2]],
                    seed = 1,
                    hidden = c(20,20),
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

#Once we have trained the grid, we can collect the results and sort by our model performance metric of choice.

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid",
                           sort_by = "accuracy",
                           decreasing = TRUE)
print(dl_gridperf)

#Grab the model_id for the top DL model, chosen by validation error.

best_dl_model_id <- dl_gridperf@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)

#Now let’s evaluate the model performance on a test set so we get an honest estimate of top model
#performance.

best_dl_perf <- h2o.performance(model = best_dl, newdata = test)
h2o.mse(best_dl_perf)

#==========================================================================================================

# 5. Naive Bayes model
# The Naive Bayes (NB) algorithm does not usually beat an algorithm like a Random Forest 
# or GBM, however it is still a popular algorithm, especially in the text domain (when your 
# input is text encoded as "Bag of Words", for example).  The Naive Bayes algorithm is for 
# binary or multiclass classification problems only, not regression.  Therefore, your response 
# must be a factor instead of a numeric.

# First we will train a basic NB model with default parameters. 
#==========================================================================================================


nb_fit1 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = train,
                          model_id = "nb_fit1")


#==========================================================================================================

# Train a NB model with Laplace Smoothing
# One of the few tunable model parameters for the Naive Bayes algorithm is the amount of Laplace 
# smoothing. The H2O Naive Bayes model will not use any Laplace smoothing by default.
#==========================================================================================================


nb_fit2 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = train,
                          model_id = "nb_fit2",
                          laplace = 6)

#==========================================================================================================

# Let's compare the performance of the two NB models
#==========================================================================================================


nb_perf1 <- h2o.performance(model = nb_fit1,
                            newdata = test)
nb_perf2 <- h2o.performance(model = nb_fit2,
                            newdata = test)


#==========================================================================================================

# Print model performance
#==========================================================================================================


nb_perf1
nb_perf2


#==========================================================================================================

# Retreive test set AUC
#==========================================================================================================


h2o.auc(nb_perf1) 
h2o.auc(nb_perf2)  



#==========================================================================================================
# AutoML: Automatic Machine Learning
# AutoML Interface
# 
# AutoML Interface
# The AutoML interface is designed to have as few parameters as possible so that all the user needs to do is point to their dataset, identify the response column and optionally specify a time-constraint.
# 
# In both the R and Python API, AutoML uses the same data-related arguments, x, y, training_frame, validation_frame, as the other H2O algorithms.
# 
# The x argument only needs to be specified if the user wants to exclude predictor columns from their data frame. If all columns (other than the response) should be used in prediction, this can be left blank/unspecified.
# The y argument is the name (or index) of the response column. Required.
# The training_frame is the training set. Required.
# The validation_frame argument is optional and will be used for early stopping within the training process of the individual models in the AutoML run.
# The leaderboard_frame argument allows the user to specify a particular data frame to rank the models on the leaderboard. This frame will not be used for anything besides creating the leaderboard.
# To control how long the AutoML run will execute, the user can specify max_runtime_secs, which defaults to 600 seconds (10 minutes).
# If the user doesn’t specify all three frames (training, validation and leaderboard), then the missing frames will be created automatically from what is provided by the user. For reference, here are the rules for auto-generating the missing frames.
# 
# When the user specifies:
#   
#   training: The training_frame is split into training (70%), validation (15%) and leaderboard (15%) sets.
# training + validation: The validation_frame is split into validation (50%) and leaderboard (50%) sets and the original training frame stays as-is.
# training + leaderboard: The training_frame is split into training (70%) and validation (30%) sets and the leaderboard frame stays as-is.
# training + validation + leaderboard: Leave all frames as-is.
#==========================================================================================================
# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_runtime_secs = 30)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

aml <- h2o.automl(x = x, y = y, training_frame = train, 
                  max_runtime_secs = 3600)


#==========================================================================================================

# save and load model
#==========================================================================================================



# build the model
model <- h2o.deeplearning(params)

# save the model
model_path <- h2o.saveModel(object=model, path=getwd(), force=TRUE)

print(model_path)
#/tmp/mymodel/DeepLearning_model_R_1441838096933

# load the model
saved_model <- h2o.loadModel(model_path)