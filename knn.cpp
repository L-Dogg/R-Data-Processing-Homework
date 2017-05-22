#include <Rcpp.h>
#include <cmath>
#include <queue>
#include <vector>
#include <functional>
#include <utility>
#include <numeric>

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

/* 
 * Oblicza odległość między dwoma wektorami x, z 
 * (w metryce Minkowskiego dla danego p).
 */
double metric(NumericVector x, NumericVector z, double p = 2)
{
  if (p == R_PosInf)
  {
    return max(abs(x - z));
  }
  
  int n = x.size();
  double sum = 0;
  for(int i = 0; i < n; i++)
  {
    sum += pow(std::abs(x[i] - z[i]), p);
  }
  return pow(sum, 1/p);
}

/*
 * Oblicza odległość w p-metryce Minkowskiego 
 * między każdym wektorem z macierzy X a wektorem z.
 */
NumericVector calculateDistance(NumericMatrix X, NumericVector z, double p = 2)
{
  int nrow = X.nrow();
  NumericVector distances(nrow);
  
  for(int i = 0; i < nrow; i++)
  {
    distances[i] = metric(X.row(i), z, p);
  }
  
  return distances;
}

/*
 * Na podstawie wektora distances zwraca indeksy k najbliższych sąsiadów.
 * Algorytm opisany jest w raporcie.
 */
NumericVector getNeighbourIndices(NumericVector distances, int k)
{
  std::priority_queue<std::pair<double, int> > q;
  int n = distances.size();
  for(int i = 0; i < n; i++)
  {
    q.push(std::pair<double, int>(-distances[i], i));
  }
  
  NumericVector indices(k);
  for(int i = 0; i < k; i++)
  {
    indices[i] = q.top().second;
    q.pop();
  }
  
  return indices;
}

/*
 * Oblicza dominantę z wektora klas c.
 */
int moda_improved(IntegerVector c)
{
  // Count occurences:
  std::unordered_map<int, int> occurences;
  for (int i = 0; i < c.size(); i++)
  {
    auto search = occurences.find(c[i]);
    if(search != occurences.end())
      search->second++;
    else
      occurences.insert({c[i], 1});
  }
  // Get keys with highest values (numbers with most occurences):
  std::vector<int> vals;
  int max = 0;
  for(const auto& n : occurences)
  {
    if (n.second > max)
    {
      max = n.second;
      vals.clear();
      vals.push_back(n.first);
    }
    else if(n.second == max)
    {
      vals.push_back(n.first);
    }
  }
  // Draw a random index:
  unsigned int ind = round(R::runif(0, 1) * vals.size());
  
  return vals[ind == vals.size() ? ind - 1 : ind];
}

/*
 * Zwraca średnią arytmetyczną elementów z wektora c.
 */
double my_mean(IntegerVector c)
{
  std::vector<int> v(c.begin(), c.end());
  double sum = std::accumulate(v.begin(), v.end(), 0.0);
  return round(sum / v.size());
}

// [[Rcpp::export]]
IntegerVector knn(NumericMatrix X, 
                  IntegerVector y, 
                  NumericMatrix Z, 
                  int k, 
                  double p = 2, 
                  bool calculateMean = false)
{
  if (k < 1)
    Rcpp::stop("K powinno być większe od 1");
  if (p < 1)
    Rcpp::stop("p powinno być większe od 1");
  if (X.ncol() != Z.ncol())
    Rcpp::stop("Liczba kolumn w X i Z nie jest równa");
  if (X.nrow() != y.length())
    Rcpp::stop("Liczba wierszy w X i y nie jest równa");
  if (k > X.nrow())
    Rcpp::stop("Wszystkich elementów X jest mniej niż k");
  
  int m = Z.nrow();
  IntegerVector w(m);
  w.attr("levels") = y.attr("levels");
  w.attr("class") = "factor";
  
  for(int i = 0; i < m; i++)
  {
    NumericVector distances = calculateDistance(X, Z.row(i), p);
    NumericVector indices = getNeighbourIndices(distances, k);
    
    IntegerVector classes(k);
    for (int j = 0; j < k; j++)
    {
      classes[j] = y[indices[j]];
    }
    w[i] = calculateMean ? my_mean(classes) : moda_improved(classes);
  }
  
  return w;
}
