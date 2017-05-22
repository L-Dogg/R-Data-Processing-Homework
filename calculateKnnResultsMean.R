# Jest to funkcja, która dla podanych zbiorów oraz metryk 
# oblicza uśrednioną wartość parametrów ERR, MAD, MSE metody knn.
# Dane zapisywane są w pliku formatu csv (nazwa pliku to parametr funkcji).

calculateKnnResultsMean <- function(
  setnames = c("abalone", "skill", "glass", "kinematics"), 
  metrics=c(1,2,Inf),
  filename="knnMean") 
{
  n <- length(setnames)
  err <- 0
  mad <- 0
  mse <- 0
  for(set in setnames)
  {
    for(p in metrics)
    {
      if (!is.infinite(p))
        results <- as.matrix(read.csv(paste("./", set, "/p_", p, ".csv", sep=""), sep = ","))  
      else
        results <- as.matrix(read.csv(paste("./", set, "/p_Inf.csv", sep=""), sep = ",")) 
      
      err <- err + mean(benchmark_results1[,1])
      mad <- mad + mean(benchmark_results1[,2])
      mse <- mse + mean(benchmark_results1[,3])
    }
  }
  err <- err / (n * length(metrics))
  mad <- mad / (n * length(metrics))
  mse <- mse / (n * length(metrics))
  
  data <- matrix(c(err, mad, mse), nrow=1)
  colnames(data) <- c("ERR", "MAD", "MSE")
  
  write.csv(
    data, 
    file = paste(filename, ".csv", sep = ""),
    row.names=FALSE, 
    sep=",")
}