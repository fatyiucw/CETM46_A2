rm(list=ls())

# LOADING LIBRARIES -----------------------------------------------------------

library(tidyverse)
library(ranger)
library(caret)

# LOADING DATA ----------------------------------------------------------------

data <- read.csv("app/data/25_perc_data.csv")

# Selecting Top 15 columns as per information gain (source from the paper in
# reference

cols <- c("Packet.Length.Std",
          "Total.Length.of.Bwd.Packets",
          "Subflow.Bwd.Bytes",
          "Destination.Port",
          "Packet.Length.Variance",
          "Bwd.Packet.Length.Mean",
          "Avg.Bwd.Segment.Size",
          "Bwd.Packet.Length.Max",
          "Init_Win_bytes_backward",
          "Total.Length.of.Fwd.Packets",
          "Subflow.Fwd.Bytes",
          "Init_Win_bytes_forward",
          "Average.Packet.Size",
          "Packet.Length.Mean",
          "Max.Packet.Length",
          "Label")

data <- data[, cols]

colnames(data)[16] <- "y"

data$y <- factor(data$y)

# SPLITTING DATA --------------------------------------------------------------

set.seed(42)   
ix <- createDataPartition(data$y, p=0.8, list=FALSE)

TRAIN <- data[ix, ]
TEST <- data[-ix, ]

# SETTING GRID ----------------------------------------------------------------

tunegrid <- expand.grid(mtry = 2:4,
                        splitrule = "gini",
                        min.node.size = c(2, 4))

# TRAINING RF -----------------------------------------------------------------


rf <- train(y ~ . , data = TRAIN,
            method = "ranger",
            tuneGrid = tunegrid,
            num.trees = 50)


print(rf)

## MODEL SUMMARY --------------------------------------------------------------

# Random Forest 
# 
# 102444 samples
# 15 predictor
# 3 classes: 'BENIGN', 'DDoS', 'PortScan' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 102444, 102444, 102444, 102444, 102444, 102444, ... 
# Resampling results across tuning parameters:
#   
#   mtry  min.node.size  Accuracy   Kappa    
# 2     2              0.9980999  0.9970651
# 2     4              0.9981201  0.9970962
# 3     2              0.9982188  0.9972488
# 3     4              0.9982039  0.9972259
# 4     2              0.9984248  0.9975670
# 4     4              0.9983696  0.9974818
# 
# Tuning parameter 'splitrule' was held constant at a value of gini
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were mtry = 4, splitrule = gini and 
# min.node.size = 2.

plot(rf)

### BEST TUNE -----------------------------------------------------------------

rf$bestTune

# mtry splitrule min.node.size
# 5    4      gini             2

### FINAL MODEL ---------------------------------------------------------------

rf$finalModel

# Ranger result
# 
# Call:
#   ranger::ranger(dependent.variable.name = ".outcome", data = x,
# mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,
# splitrule = as.character(param$splitrule), write.forest = TRUE,
# probability = classProbs, ...) 
# 
# Type:                             Classification 
# Number of trees:                  50 
# Sample size:                      102444 
# Number of independent variables:  15 
# Mtry:                             4 
# Target node size:                 2 
# Variable importance mode:         none 
# Splitrule:                        gini 
# OOB prediction error:             0.14 %

# PRED & TEST -----------------------------------------------------------------

pred <- predict(rf, newdata = TEST)

## SCORE ----------------------------------------------------------------------

confusionMatrix(pred, TEST$y, mode = "everything")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction BENIGN  DDoS PortScan
# BENIGN    11285     5       12
# DDoS          2  6391        0
# PortScan      7     0     7907
# 
# Overall Statistics
# 
# Accuracy : 0.999                
# 95% CI : (0.9985, 0.9993)     
# No Information Rate : 0.441                
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9984               
# 
# Mcnemar's Test P-Value : NA                   
# 
# Statistics by Class:
# 
#                      Class: BENIGN Class: DDoS Class: PortScan
# Sensitivity                 0.9992      0.9992          0.9985
# Specificity                 0.9988      0.9999          0.9996
# Pos Pred Value              0.9985      0.9997          0.9991
# Neg Pred Value              0.9994      0.9997          0.9993
# Precision                   0.9985      0.9997          0.9991
# Recall                      0.9992      0.9992          0.9985
# F1                          0.9988      0.9995          0.9988
# Prevalence                  0.4410      0.2498          0.3092
# Detection Rate              0.4407      0.2496          0.3088
# Detection Prevalence        0.4413      0.2496          0.3090
# Balanced Accuracy           0.9990      0.9996          0.9990

# SAVE MODEL ------------------------------------------------------------------

# saveRDS(rf, "rf.rds")

# LOAD MODEL ------------------------------------------------------------------

# model <- readRDS("rf.rds")
