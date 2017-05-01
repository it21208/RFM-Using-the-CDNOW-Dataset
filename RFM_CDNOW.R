##############  1. GLOBAL CONSTANTS #######################
#
# Application and Run Type constants
RUN_TYPE <- "TRAINING"  # TRAINING , TEST
RUN_TITLE <- paste("CDNOW RFM", RUN_TYPE, "DATASET")
# Data File constants
TransSampleDSFile <- "CDNOW_sample_trans.txt"
ChurnMasterFile <- "CDNOW_master_churn.txt"
CustSampleDSFile <- "CDNOW_sample_cust.txt"
TrainingDSFile <- "CDNOW_training.txt"
TestDSFile <- "CDNOW_test.txt"
# Data Frame Column Name constants
idColName <- "ID"
recencyColName = "Recency (months)"
frequencyColName = "Frequency (times)"
monetaryColName = "Monetary ($)"
timesColName = "Time (months)"
churnColName = "Churn (0/1)"
rScoreColName = "R-Score"
fScoreColName = "F-Score"
mScoreColName = "M-Score"
rfmScoreColName = "RFM-Score"
# RFM Segmentation Analysis & Score Calculation constants
rfmRecencyClusters = 5
rfmFrequencyClusters = 5
rfmMonetaryClusters = 5
RFM_MATRICES_PROVIDED = FALSE # if TRUE, CUSTSEGMMATRICE_LST is used
CUSTSEGMMATRICE_LST <- 
  list(RECENCY = matrix(, nrow = rfmRecencyClusters, ncol = 3, byrow = TRUE),
       FREQUENCY = matrix(, nrow = rfmFrequencyClusters, ncol = 3,  byrow = TRUE),
       MONETARY = matrix(, nrow = rfmMonetaryClusters,  ncol = 3,  byrow = TRUE))
rfmCoefficients = c(1, 1, 1)  # R, F, M coefficients for RFMScore
#
##############  2. FUNCTIONS #######################
#
### 2.1 BUILT CLUSTER MATRIX FOR AN ATTRIBUTE ###
# This function will create a Cluster Matrix for the specified Feature column,
# after having sorted the Feature column accordingly (sortDescending)
# The Cluster Matrix will describe the StartValue and Score for each cluster ID
createFeatureClusterMatrix <-
  function (featureColumn, noOfClusters, sortDesc) {
    # sort the featureColumn in sortDesc order, which orders values in a way
    # that the top most have the highest scores
    featureColumn <- sort(featureColumn, decreasing = sortDesc)
    # create a clusterMatrix noOfClusters rows by 3 columns (ClusterID, StartValue, Score).
    # if skipped clusters are encountered (due to a large number of equal values) these
    # rows will be removed and the clusterMatrix will contains less rows than noOfClusters
    clusterMatrix = matrix(,
                           nrow = noOfClusters,
                           ncol = 3,
                           byrow = TRUE)
    # calculate the ideal number of elements per cluster
    len <- length(featureColumn)
    clusterElements <- round(len / noOfClusters)
    # declare and initialize an index to iterate through featureColumn
    dataIdx = 1
    # declare and initialize a flag to stop processing more clusters
    noMoreClustersToProcess = FALSE
    # create a for loop with an index e.g. clusterID in the range [1:noOfClusters]
    for (clusterID in 1:noOfClusters) {
      # check if we must skip an entire cluster!
      if ((noMoreClustersToProcess) ||
          (dataIdx > clusterID * clusterElements &&
           clusterID < noOfClusters)) {
        clusterMatrix[clusterID, 1] <-
          -1 # it signals that this row should be deleted!
        next
      }
      # define the clusterID of cluster
      clusterMatrix[clusterID, 1] <- clusterID
      # define the start value of cluster
      startValue <- featureColumn[dataIdx]
      # update the dataIdx with the position of the ideal-last element of the clusterID
      if (clusterID < noOfClusters)
        dataIdx <- clusterID * clusterElements
      else
        dataIdx = len
      
      # retrieve the value of the ideal-last element of the clusterID
      endValue <- featureColumn[dataIdx]
      # set the start value for the cluster to endValue or startValue
      if (sortDesc)
        clusterMatrix[clusterID, 2] <- endValue
      else
        clusterMatrix[clusterID, 2] <- startValue
      # define the score value of cluster with clusterID
      clusterMatrix[clusterID, 3] <- noOfClusters - clusterID + 1
      # advance the dataIdx as many places as necessary in order to find the first
      # element that will have a differnt value from the last recorded endValue. This
      # new different value will be the startValue for the following clusterID
      for (duplicatesIndx in dataIdx:len)
        if (featureColumn[duplicatesIndx] != endValue) {
          dataIdx <- duplicatesIndx
          break
        }
      # check if there are no more cluster to process.
      noMoreClustersToProcess = (featureColumn[len] == endValue)
    }
    # remove clusterMatrix rows with negative IDs (they represent skipped clusters)
    clusterMatrix <- clusterMatrix[clusterMatrix[, 1] > 0, ]
    # sort clusterMatrix with column 2 in ascending order (bigger values last, so that featureScore()
    #                                                      can start from the last row always)
    clusterMatrix <-
      clusterMatrix[order(clusterMatrix[, 2], clusterMatrix[, 3], decreasing = FALSE), ]
    return(clusterMatrix)
  }
#
### 2.2 CALCULATE SCORE FOR AN ATTRIBUTE VALUE ###
#
featureScore <- function(featureValue, featureClusterMatrix)
{
  retvalue <- 0
  for (clusterID in nrow(featureClusterMatrix):1)
    if (featureValue >= featureClusterMatrix[clusterID, 2]) {
      retvalue <- featureClusterMatrix[clusterID, 3]
      break
    }
  return(retvalue)
}
#
### 2.3 CALCULATE MOVING AVERAGE  ###
#
calcMovingAverage <- function(v, m) {
  len = length(v)
  ret_v <- rep(0, times = len)
  for (i in 1:len) {
    # find the position of the start and last element to summarize
    if (i <= m) {
      startPos <- 1
      endPos <- i + m
    } else if (i >= len - m) {
      endPos <- len
      startPos <- i - m
    } else {
      startPos <- i - m
      endPos <- i + m
    }
    # accumulate the elements in range [startPos..endPos]
    sum <- 0
    for (j in startPos:endPos)
      sum <- sum + v[j]
    # find average and store it in new vector
    ret_v[i] <- sum / (endPos - startPos + 1)
  }
  return(ret_v)
}
#
### 2.4  PARTITION CUSTOMER DATA SET TO TRAINING & TEST DATA SETS ###
#
partitionCustomerDSToTrainingTestDS <-
  function(CustSampleDSFile,
           TrainingDSFile,
           TestDSFile,
           RunType) {
    # RUN_TYPE is {"TRAINING" | "TEST"}
    # print function display header
    printf("partitionCustomerDSToTrainingTestDS()")
    # read custMasterFile
    custSDF <- read.csv(CustSampleDSFile, header = TRUE)
    # add autonumber column to create user-ids
    len <- nrow(custSDF)
    custSDF <- as.data.frame(cbind(1:len, custSDF))
    names(custSDF)[1] <- idColName
    # split training and test
    splitNo <- round(0.75*len)
    trainDF <- custSDF[custSDF[, idColName] <= splitNo ,]
    testDF <- custSDF[custSDF[, idColName] > splitNo,]
    # store data frames to files
    write.csv(trainDF, TrainingDSFile, row.names = FALSE)
    write.csv(testDF, TestDSFile, row.names = FALSE)
    # set RunFile
    if (RunType == "TRAINING")
      RunFile <- TrainingDSFile
    else
      RunFile <- TestDSFile
    return(RunFile)
  }
#
### 2.5 PRINTF ###
#
printf <- function(...) {
  invisible(print(sprintf(...)))
}
#
### 2.6 RFM DATASET PREPARATION ###
# RunFile schema : (ID, Recency, Frequency, Monetary, Time, Churn)
prepareRFMTCdataset <- function(RunFile) {
  # print function display header
  printf("prepareRFMTCdataset()")
  # read RunFile with header
  rfmtcDF <- read.csv(RunFile, header = TRUE)
  len <- length(rfmtcDF[, 1])
  # add autonumber column to create user-ids and
  # create the rfmtcDF from the rfmtcDF and 4 blank score columns
  rfmtcDF <- as.data.frame(cbind(
    1:nrow(rfmtcDF),
    rfmtcDF[, 2:6],
    rep(0, times = len),
    rep(0, times = len),
    rep(0, times = len),
    rep(0, times = len)
  ))
  # define appropriate column labels
  names(rfmtcDF) <- c(
    idColName,
    recencyColName,
    frequencyColName,
    monetaryColName,
    timesColName,
    churnColName,
    rScoreColName,
    fScoreColName,
    mScoreColName,
    rfmScoreColName
  )
  return(rfmtcDF)
}
#
### 2.7 CONVERT TRANSACTION DATASETS TO RFM-RFMTC CUSTOMER DATASETS ###
#
convertTransactionToRFMTCreadyDataset <-
  function(transSampleFile,
           custSampleFile,
           churnMasterFile) {
    # print function display header
    printf("convertTransactionToRFMTCreadyDataset()")
    # read transSampleFile
    transSDF <- read.table(transSampleFile, header = FALSE)
    # remove ID column (col-2) and CDs column (col-4) from sample data frame
    transSDF <- as.data.frame(cbind(transSDF[, c(1, 3, 5)]))
    # set the column names for data frame
    names(transSDF) <- c("CustID", "Date", "Amount")
    # read churn master file
    churnDF <- read.csv(churnMasterFile, header = TRUE)
    # set the column names for churn data frame
    names(churnDF) <- c("CustID", "Churn")
    # tranform transaction sample data frame to customer sample data frame
    custSDF <- calcAppendRFMTC(transSDF, churnDF)
    # write custSampleFile
    write.csv(custSDF, custSampleFile, row.names = FALSE)
    return(NULL)
  }
#
### 2.8 CALCULATE & APPEND R,F,M,T,C COLUMNS TO CUSTOMER DATASET ###
#  names(transDF) is c("CustID", "Date", "Amount")
#  names(churnDF) is c("CustID", "Churn")
calcAppendRFMTC <- function(transDF, churnDF) {
  # set the max(transaction date)+1
  endDate <- as.Date("19980701", "%Y%m%d")
  # a) convert Date col from numeric to date type
  transDF[, "Date"] <-
    as.Date(as.character(transDF[, "Date"]), "%Y%m%d")
  # b) find the max transaction date per CustID
  t_maxDate <-
    aggregate(transDF[, "Date"], list(transDF[, "CustID"]), max)
  names(t_maxDate) <- c("CustID", "MaxDate")
  t_maxDate <- t_maxDate[order(t_maxDate[, "CustID"]), ]
  # c) find the min transaction date per CustID
  t_minDate <-
    aggregate(transDF[, "Date"], list(transDF[, "CustID"]), min)
  names(t_minDate) <- c("CustID", "MinDate")
  t_minDate <- t_minDate[order(t_minDate[, "CustID"]), ]
  # d) find the average Amount per CustID
  t_AvgAmount <-
    aggregate(transDF[, "Amount"], list(transDF[, "CustID"]), mean)
  names(t_AvgAmount) <- c("CustID", "AvgAmount")
  t_AvgAmount <- t_AvgAmount[order(t_AvgAmount[, "CustID"]), ]
  # e) find the Frequency
  t_Freq <-
    aggregate(transDF[, "CustID"], by = list(transDF[, "CustID"]), length)
  names(t_Freq) <- c("CustID", "Frequency")
  t_Freq <- t_Freq[order(t_Freq[, "CustID"]), ]
  # f) find the Churn
  churnDF <- churnDF[churnDF[, "CustID"] %in% transDF[, "CustID"], ]
  churnDF <- churnDF[order(churnDF[, "CustID"]), ]
  # sort both transDF and churnDF with ID in ascending order
  transDF <- transDF[order(transDF[, "CustID"]), ]
  # g) merge columns and convert Recency and Time to months!
  custDF <-
    as.data.frame(cbind(
      (endDate - t_maxDate[, "MaxDate"]),
      t_Freq[, "Frequency"],
      t_AvgAmount[, "AvgAmount"],
      (endDate - t_minDate[, "MinDate"]),
      churnDF[, "Churn"]
    ))
  names(custDF) <-
    c("Recency", "Frequency", "Monetary", "Time", "Churn")
  custDF[, "Recency"] <- custDF[, "Recency"] / 30
  custDF[, "Time"] <- custDF[, "Time"] / 30
  return(custDF)
}
#
### 2.9 CALCULATE & STORE RFM SEGMENTATION MATRICES ###
#
calcRFMSegmentationMatrices <- function(rfmtcReadyDF) {
  printf("calcRFMSegmentationMatrices()")
  # Logic to create segmentation should be added here
  # If segmentation matrices are provided by customer logic
  # then load them from CUSTSEGMMATRICE_LST
  if (!RFM_MATRICES_PROVIDED) {
    rfmRecencyClusterMatrix <-
      createFeatureClusterMatrix(rfmtcReadyDF[, recencyColName], rfmRecencyClusters, FALSE)
    rfmFrequencyClusterMatrix <-
      createFeatureClusterMatrix(rfmtcReadyDF[, frequencyColName], rfmFrequencyClusters, TRUE)
    rfmMonetaryClusterMatrix <-
      createFeatureClusterMatrix(rfmtcReadyDF[, monetaryColName], rfmMonetaryClusters, TRUE)
  } else {
    rfmRecencyClusterMatrix <- CUSTSEGMMATRICE_LST[["RECENCY"]]
    rfmFrequencyClusterMatrix <- CUSTSEGMMATRICE_LST[["FREQUENCY"]]
    rfmMonetaryClusterMatrix <- CUSTSEGMMATRICE_LST[["MONETARY"]]
  }
  # save the segmentation matrices to external files
  write.csv(rfmRecencyClusterMatrix,
            "RECENCY_SEGMENTATION_MATRIX.csv")
  write.csv(rfmFrequencyClusterMatrix,
            "FREQUENCY_SEGMENTATION_MATRIX.csv")
  write.csv(rfmMonetaryClusterMatrix,
            "MONETARY_SEGMENTATION_MATRIX.csv")
}
#
### 2.10 LOAD RFM SEGMENTATION MATRICES ###
#
loadRFMSegmentationMatrices <- function() {
  printf("loadRFMSegmentationMatrices()")
  # load the segmentation matrices from the external files
  rsm <- read.csv("RECENCY_SEGMENTATION_MATRIX.csv")[2:4]
  fsm <- read.csv("FREQUENCY_SEGMENTATION_MATRIX.csv")[2:4]
  msm <- read.csv("MONETARY_SEGMENTATION_MATRIX.csv")[2:4]
  # define RFM Clustering matrice column and row names
  rownames(rsm) <-
    paste("Cluster-", 1:length(rsm[, 1]), sep = "")
  colnames(rsm) <-
    c("Cluster No", "Start Value", "Score")
  rownames(fsm) <-
    paste("Cluster-", 1:length(fsm[, 1]), sep = "")
  colnames(fsm) <-
    c("Cluster No", "Start Value", "Score")
  rownames(msm) <-
    paste("Cluster-", 1:length(msm[, 1]), sep = "")
  colnames(msm) <-
    c("Cluster No", "Start Value", "Score")
  # store segmentation matrices to a list
  SEGMMATRICE_LST <- 
    list(RECENCY = rsm, FREQUENCY = fsm, MONETARY = msm)
  return(SEGMMATRICE_LST)
}
#
### 2.11 CALCULATE R,F,M SCORES & RFM TOTAL SCORE ###
#
rfmScoreCalculation <- function(rfmtcReadyDF, SEGMMATRICE_LST) {
  printf("rfmScoreCalculation()")
  # calculate R, F, M feature scores
  rfmtcReadyDF[, rScoreColName] <-
    sapply(rfmtcReadyDF[, recencyColName], function(x)
      featureScore(x, SEGMMATRICE_LST[["RECENCY"]]))
  rfmtcReadyDF[, fScoreColName] <-
    sapply(rfmtcReadyDF[, frequencyColName], function(x)
      featureScore(x, SEGMMATRICE_LST[["FREQUENCY"]]))
  rfmtcReadyDF[, mScoreColName] <-
    sapply(rfmtcReadyDF[, monetaryColName], function(x)
      featureScore(x, SEGMMATRICE_LST[["MONETARY"]]))
  # calculate RFM Total score
  rfmtcReadyDF[, rfmScoreColName] <-
    rfmCoefficients[1] * rfmtcReadyDF[, rScoreColName] +
    rfmCoefficients[2] * rfmtcReadyDF[, fScoreColName] +
    rfmCoefficients[3] * rfmtcReadyDF[, mScoreColName]
  # sort data on all feature scores in desc order
  rfmtcReadyDF <-
    rfmtcReadyDF[order(-rfmtcReadyDF[, rScoreColName], 
                       -rfmtcReadyDF[, fScoreColName], 
                       -rfmtcReadyDF[, mScoreColName]), ]
  return(rfmtcReadyDF)
}
#
### 2.12 CALCULATE P[B] & RFM RESPONSE PROBABILITY & STORE RESULTS ###
#
calculatePB_RespProb <- function(rfmtcScoredDF, m) {
  # m = 4 comes from RFMTC training set calculations
  printf("calculatePB_RespProb")
  # sort data frame with the RFM-Score in descending order
  # RFM-Score desc & R desc & F asc & M asc
  rfmtcScoredDF <- rfmtcScoredDF[order(-rfmtcScoredDF$`RFM-Score`, 
                                       -rfmtcScoredDF$`Recency (months)`,
                                       rfmtcScoredDF$`Frequency (times)`,
                                       rfmtcScoredDF$`Monetary ($)`),]
  rfmtcScoredDF <-
    as.data.frame(cbind(rfmtcScoredDF, calcMovingAverage(rfmtcScoredDF[, churnColName], m)))
  colnames(rfmtcScoredDF)[length(colnames(rfmtcScoredDF))] <- "P[B]"
  # Calculate RFM Segment Response Probability
  rfmAggrData <- # select RFM-Score, avg(P[B]) 
    # from rfmtcScoredDF 
    # group by RFM-Score
    aggregate(rfmtcScoredDF$`P[B]`,
              list(rfmtcScoredDF$`RFM-Score`),
              "mean")
  names(rfmAggrData) <- c("Segment", "Response Probability")
  # Lookup RFM Customer Response Probabilities from rfmAggrData data frame
  # and fill them into the Validation data frame
  len <- length(rfmtcScoredDF[,1])
  rfmtcScoredDF <-
    as.data.frame(cbind(rfmtcScoredDF, rep(0, times = len)))
  colnames(rfmtcScoredDF)[length(colnames(rfmtcScoredDF))] <-
    "RFM Resp Prob"
  for (i in 1:len) {
    rfmtcScoredDF$`RFM Resp Prob`[i] <-
      rfmAggrData[rfmAggrData$Segment == rfmtcScoredDF$`RFM-Score`[i], 2]
  }
  # write data to file
  write.csv(rfmtcScoredDF,
            paste("OUR_RFM_", RUN_TYPE, ".csv", sep = ""),
            row.names = FALSE)
  return(rfmtcScoredDF)
}
#
### 3. MAIN PROGRAM ###
printf("%s", RUN_TITLE)
printf("-------------------------------")
## PART A
# convert transaction datasets to RFM-RFMTC customer datasets
convertTransactionToRFMTCreadyDataset(
  TransSampleDSFile,
  CustSampleDSFile,
  ChurnMasterFile
)
## PART B
# partition customer dataset to training & test datasets
RunFileName <-
  partitionCustomerDSToTrainingTestDS(
    CustSampleDSFile,
    TrainingDSFile,
    TestDSFile,
    RUN_TYPE
  )
## PART C
# RFM dataset preparation
rfmtcReadyDF <- prepareRFMTCdataset(RunFileName)
if (RUN_TYPE == "TRAINING") {
  # calculate/load RFM Clustering matrices
  calcRFMSegmentationMatrices(rfmtcReadyDF)
} 
# read the segmentation matrices from external files
SEGMMATRICE_LST <- loadRFMSegmentationMatrices()
# calculate R,F,M scores & RFM total score
rfmtcScoredDF <- rfmScoreCalculation(rfmtcReadyDF, SEGMMATRICE_LST)
# calculate & RFM response probability & store results
rfmtcScoredDF <- calculatePB_RespProb(rfmtcScoredDF, m = 6)