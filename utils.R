ERR <- function(w, y) 
{
  w <- as.numeric(w)
  y <- as.numeric(y)
  sum(w!=y) / length(w)
}
MAD <- function(w, y)
{
  w <- as.numeric(w)
  y <- as.numeric(y)
  sum(abs(w-y)) / length(w)
}
MSE <- function(w, y)
{
  w <- as.numeric(w)
  y <- as.numeric(y)
  sum(abs(w-y)^2) / length(w)
}

# u-krotna kroswalidacja dla knn. Zwracany jest wektor (ERR, MAD, MSE)
crossvalidation <- function(X, y, p, k, u=5, FUN, calculateMean = FALSE) 
{
  n <- nrow(X)
  count <- n / u
  indices <- sample(1:n, n)
  sets <- split(indices, ceiling(seq_along(indices)/count))
  err <- mad <- mse <- rep(0, u)
  for(i in 1:u)
  {
    w <- FUN(X[-sets[[i]],], y[-sets[[i]]], X[sets[[i]],], k, p, calculateMean)
    err[i] <- ERR(w, y[sets[[i]]])
    mad[i] <- MAD(w, y[sets[[i]]])
    mse[i] <- MSE(w, y[sets[[i]]])
  }
  
  return(c(mean(err), mean(mad), mean(mse)))
}

# Okrojona wersja funkcji crossvalidation - działa dla randomForest.
crossvalidation_randomForest <- function(X, y, u=5) 
{
  library(randomForest)
  
  n <- nrow(X)
  count <- n / u
  indices <- sample(1:n, n)
  sets <- split(indices, ceiling(seq_along(indices)/count))
  err <- mad <- mse <- rep(0, u)
  for(i in 1:u)
  {
    w <- predict(randomForest(X[-sets[[i]],], y[-sets[[i]]]), X[sets[[i]],])
    err[i] <- ERR(w, y[sets[[i]]])
    mad[i] <- MAD(w, y[sets[[i]]])
    mse[i] <- MSE(w, y[sets[[i]]])
  }
  
  return(c(mean(err), mean(mad), mean(mse)))
}

# Okrojona wersja funkcji crossvalidation - działa dla polr
crossvalidation_polr <- function(X, y, u=5)
{
  library('MASS')
  
  n <- nrow(X)
  count <- n / u
  indices <- sample(1:n, n)
  sets <- split(indices, ceiling(seq_along(indices)/count))
  err <- mad <- mse <- rep(0, u)
  for(i in 1:u)
  {
    frame <- data.frame(y = factor(y[-sets[[i]]]), predictors = X[-sets[[i]],])
    model <- polr(y ~ ., data = frame)
    w <- predict(model, newdata = data.frame(predictors = X[sets[[i]],]))
    
    err[i] <- ERR(w, y[sets[[i]]])
    mad[i] <- MAD(w, y[sets[[i]]])
    mse[i] <- MSE(w, y[sets[[i]]])
  }
  
  return(c(mean(err), mean(mad), mean(mse)))
}

# Okrojona wersja funkcji crossvalidation - działa dla naiveBayesian
crossvalidation_naive_bayesian <- function(X, y, u=5)
{
  library('e1071')
  
  n <- nrow(X)
  count <- n / u
  indices <- sample(1:n, n)
  sets <- split(indices, ceiling(seq_along(indices)/count))
  err <- mad <- mse <- rep(0, u)
  for(i in 1:u)
  {
    frame <- data.frame(y = factor(y[-sets[[i]]]), predictors = X[-sets[[i]],])
    model <- naiveBayes(y ~ ., data = frame)
    w <- predict(model, newdata = data.frame(predictors = X[sets[[i]],]))
    
    err[i] <- ERR(w, y[sets[[i]]])
    mad[i] <- MAD(w, y[sets[[i]]])
    mse[i] <- MSE(w, y[sets[[i]]])
  }
  
  return(c(mean(err), mean(mad), mean(mse)))
}

# Funkcja standaryzująca zbiór danych X.
# Od każdej kolumny w X odejmujemy jej średnią arytmetyczną, 
# a następnie dzielimy przez odchylenie standardowe.
standarizeData <- function(X) 
{
  scale(X)
}

# Oblicza knn w wersji z baggingiem (opisany w raporcie).
bagging <- function(X, y, Z, k, p, n=100) 
{
  # n - liczba podzbiorów
  m <- nrow(X)
  cols <- ncol(X)
  # minimalny rozmiar podzbioru to k, żeby można było znaleźć k sąsiadów
  subset_sizes <- runif(n, k, m)
  
  indices <- sample(1:m, subset_sizes[1], replace = TRUE)
  w <- knn(matrix(X[indices, ], ncol=cols), y[indices], Z, k, p)
  vals <- matrix(w, nrow=1)
  
  if (n > 1)
  {
    for(i in 2:n)
    {
      indices <- sample(1:m, subset_sizes[i], replace = TRUE)
      w <- knn(matrix(X[indices, ], ncol=cols), y[indices], Z, k, p)
      vals <- rbind(vals, w)
    }
  }
  
  vals <- sapply(1:ncol(vals), function(x) {moda(vals[,x])})
  names(vals) <- NULL
  factor(as.integer(vals), levels=levels(y))
}

# Funkcja pomocnicza dla baggingu: zwraca dominantę wektora x.
moda <- function(x) {
  stopifnot(is.vector(x) & is.atomic(x) & length(x) > 0)
  
  x <- rle(sort(x))
  lengths <- x$lengths;
  values <- x$values;
  indices <- which(lengths == max(lengths))
  modas <- values[indices] 
  return(modas[round(runif(1, 1, length(modas)))])
}