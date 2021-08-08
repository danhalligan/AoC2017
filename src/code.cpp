#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int day15p1(long sa, long sb, long n) {
  int count = 0;
  for (int i=0; i<n; i++) {
    sa = (sa * 16807) % 2147483647;
    sb = (sb * 48271) % 2147483647;
    if (sa % 65536 == sb % 65536) count ++;
  }
  return(count);
}

// [[Rcpp::export]]
int day15p2(long sa, long sb, long n) {
  int count = 0;
  for (int i=0; i<n; i++) {
    sa = (sa * 16807) % 2147483647;
    sb = (sb * 48271) % 2147483647;
    while (sa % 4 != 0) sa = (sa * 16807) % 2147483647;
    while (sb % 8 != 0) sb = (sb * 48271) % 2147483647;
    if (sa % 65536 == sb % 65536) count ++;
  }
  return(count);
}

// [[Rcpp::export]]
int day17p2(long offset, long n) {
  long pos = 0;
  int save = 1;
  for (long val = 1; val <= 5e7; val++) {
    pos = (pos + offset) % val + 1;
    if (pos == 1) save = val;
  }
  return(save);
}
