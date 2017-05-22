# Bazowa funkcja zapisująca do pliku w odpowiednim 
# katalogu wartości miar błędów (ERR, MAD, MSE) dla metody knn 
# obliczanej parametrem FUN
generateCSV <- function(datasetName, 
                        FUN, 
                        p = c(1, 2, Inf), 
                        k = c(1,3,5,7,9,11,13,15,17,19), 
                        folderNameSufix="", 
                        standarize_data=FALSE,
                        calculateMean = FALSE)
{
  source("utils.R")  # utils.R contains crossvalidation function

  prefix <- "http://www.gagolewski.com/resources/data/ordinal-regression/"
  sufix <- ".csv"
  d <- as.matrix(read.csv(paste(prefix, datasetName, sufix, sep=""), sep = ","))
  y <- factor(d[,1])
  X <- d[, 2:ncol(d)]
  
  if(standarize_data)
  {
    X <- standarizeData(X)
    folderNameSufix="_standarized_data"
  }
  if(calculateMean)  
  {
    folderNameSufix="_mean"
  }
  
  catalog <- paste(datasetName, folderNameSufix, sep="")
  unlink(catalog, recursive = TRUE)
  dir.create(catalog)
  
  for (i in p)
  {
    # Prepare matrix:
    data <- matrix(crossvalidation(X, y, i, 1, 5, FUN, calculateMean), nrow=1)
    colnames(data) <- c("ERR", "MAD", "MSE")
    
    for (j in k[2:length(k)])
    {
      # Append new row:
      row <- matrix(crossvalidation(X, y, i, j, 5, FUN, calculateMean), nrow=1)
      data <- rbind(data, row)
    }
    
    # Save to csv file:
    write.csv(
      data, 
      file = paste(catalog, "/p_", i, sufix, sep=""),
      row.names=FALSE, 
      sep=",")
  }
}

# Bardziej ograniczona wersja funkcji generateCSV - działa dla randomForest.
generateCSV_randomForest <- function(datasetName, folderNameSufix="_random_forest")
{
  library(randomForest)
 
  prefix <- "http://www.gagolewski.com/resources/data/ordinal-regression/"
  sufix <- ".csv"
  d <- as.matrix(read.csv(paste(prefix, datasetName, sufix, sep=""), sep = ","))
  y <- factor(d[,1])
  X <- d[, 2:ncol(d)]
  
  catalog <- paste(datasetName, folderNameSufix, sep="")
  unlink(catalog, recursive = TRUE)
  dir.create(catalog)
  # Prepare matrix:
  data <- matrix(crossvalidation_randomForest(X, y), nrow=1)
  colnames(data) <- c("ERR", "MAD", "MSE")
    
  # Save to csv file:
  write.csv(
    data, 
    file = paste(catalog, "/randomForest", sufix, sep=""),
    row.names=FALSE, 
    sep=",")
}

# Bardziej ograniczona wersja funkcji generateCSV - działa dla wersji knn z baggingiem
# UWAGA: przyjąłem założenie, że ustawiony jest poprawny katalog roboczy.
#        Można to sprawdzić metodą getwd(), a ustawić setwd()
generateCSV_bagging <- function(datasetName, p = 2, k = 2, folderNameSufix="_bagging_p2_k2")
{
  library(Rcpp)
  Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
  source("utils.R")  # utils.R contains crossvalidation function
  sourceCpp("knn.cpp")
  
  prefix <- "http://www.gagolewski.com/resources/data/ordinal-regression/"
  sufix <- ".csv"
  d <- as.matrix(read.csv(paste(prefix, datasetName, sufix, sep=""), sep = ","))
  y <- factor(d[,1])
  X <- d[, 2:ncol(d)]
  
  catalog <- paste(datasetName, folderNameSufix, sep="")
  unlink(catalog, recursive = TRUE)
  dir.create(catalog)
  
  # Prepare matrix:
  data <- matrix(crossvalidation(X, y, p, k, 5, bagging), nrow=1)
  colnames(data) <- c("ERR", "MAD", "MSE")
  
  # Save to csv file:
  write.csv(
    data, 
    file = paste(catalog, "/knn_bagging_p", p, "k", k, sufix, sep=""),
    row.names=FALSE, 
    sep=",")
}

# Bardziej ograniczona wersja funkcji generateCSV - działa dla funkcji polr
generateCSV_polr <- function(datasetName, folderNameSufix="_polr")
{
  prefix <- "http://www.gagolewski.com/resources/data/ordinal-regression/"
  sufix <- ".csv"
  d <- as.matrix(read.csv(paste(prefix, datasetName, sufix, sep=""), sep = ","))
  y <- factor(d[,1])
  X <- d[, 2:ncol(d)]
  
  catalog <- paste(datasetName, folderNameSufix, sep="")
  unlink(catalog, recursive = TRUE)
  dir.create(catalog)
  
  # Prepare matrix:
  data <- matrix(crossvalidation_polr(X, y, 5), nrow=1)
  colnames(data) <- c("ERR", "MAD", "MSE")

  # Save to csv file:
  write.csv(
    data, 
    file = paste(catalog, "/polr", sufix, sep=""),
    row.names=FALSE, 
    sep=",")
}

# Bardziej ograniczona wersja funkcji generateCSV - działa dla naiveBayesian
generateCSV_naive_bayesian <- function(datasetName, folderNameSufix="_naive_bayesian")
{
  prefix <- "http://www.gagolewski.com/resources/data/ordinal-regression/"
  sufix <- ".csv"
  d <- as.matrix(read.csv(paste(prefix, datasetName, sufix, sep=""), sep = ","))
  y <- factor(d[,1])
  X <- d[, 2:ncol(d)]
  
  catalog <- paste(datasetName, folderNameSufix, sep="")
  unlink(catalog, recursive = TRUE)
  dir.create(catalog)

  # Prepare matrix:
  data <- matrix(crossvalidation_naive_bayesian(X, y, 5), nrow=1)
  colnames(data) <- c("ERR", "MAD", "MSE")
  
  # Save to csv file:
  write.csv(
    data, 
    file = paste(catalog, "/naive_bayesian" ,sufix, sep=""),
    row.names=FALSE, 
    sep=",")
}