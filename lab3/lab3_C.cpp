#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector SimilarityCplus(Rcpp::NumericVector vector1, Rcpp::NumericVector vector2){
  // Calculate the similarity score between two vectors, based on Jaccard coefficient
  // vector1: a q by q numeric vector
  // vector2: a q by q numeric vector
  // Return jaccard similarity coefficient of vector1 and vector2
  
  // Initialization:
  int N10 = 0;
  int N01 = 0;
  int N11 = 0;
  int Size = vector1.size(); // Size is the size of vector 1
  
  // Write a for loop to iterate over vector 1 and vector 2 using i,j pointers
  for (int i = 0; i < Size; i++){
    for (int j = i+1; j < Size; j++){
      if (vector1[i] == vector1[j] && vector2[i] != vector2[j])
        N01++;
      else if (vector1[i] != vector1[j] && vector2[i] == vector2[j])
        N10++;
      else if (vector1[i] == vector1[j] && vector2[i] == vector2[j])
        N11++;
    }
  }
  // return the jaccard coefficient
  return(Rcpp::NumericVector::create(1.0*N11/(N01+N10+N11)));
}
