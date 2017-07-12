#include <Rcpp.h>
#include <ctime>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

/* name spaces */
using namespace Rcpp;
using namespace std;

// Thanks: http://stackoverflow.com/questions/4003232/how-to-code-a-modulo-operator-in-c-c-obj-c-that-handles-negative-numbers
int mod (int a, int b) {
  if(b < 0) //you can check for b == 0 separately and do what you want
  return mod(-a, -b);
  int ret = a % b;
  if(ret < 0)
    ret += b;
  return ret;
}
// http://stackoverflow.com/questions/2704521/generate-random-double-numbers-in-c
double frand(double upper) {
  double f = (double)rand() / RAND_MAX;
  return f * upper;
}


// http://stackoverflow.com/questions/1761626/weighted-random-numbers
int rsamp(vector<int>& nums, vector<double>& probs) {

  double sum_of_weight = 0, rnd;
  vector<int>::iterator it_num;
  vector<double>::iterator it_prob;

  // sum weights
  for (it_prob = probs.begin(); it_prob < probs.end(); it_prob++) {
    sum_of_weight += *it_prob;
  }

  rnd = frand(sum_of_weight);

  for(it_prob = probs.begin(), it_num = nums.begin(); it_prob < probs.end(); it_prob++, it_num++) {
    if(rnd < *it_prob)
      return *it_num;
    rnd -= *it_prob;
  }
}

Simulate UD new try
//[[Rcpp::export]]

IntegerVector simulate_udf(int n_steps, int start, int nc, int nr, NumericMatrix mk, NumericMatrix hk) {

 int step, j,
   npot = mk.nrow(); // number of options cells possible move to (-> # cells in the movement kernel)
 int ncell = hk.nrow();

 vector<vector<int> > cells (ncell);  // create vector of pointers
 vector<vector<double> > probs (ncell);

 IntegerVector ud(hk.nrow(), 0);

 int k = start; // current position
 ud(k)++;

 // step is just repeating the simulations n_steps time
 // and not really neeeded in the simulations

 // The variable k hold the current cell number
 for (step = 1; step < n_steps; step++) {
   if (cells[k].empty()) {
     // if a cell was not visisted before, calulate the dispersal kernel for that cell
     // this needs to be done only once per cell

     // first allocate memory to save the neighbouring cells and
     // the probability of moving to one of these neighbouring cells: the prod of the
     // dispersal and movement kernel
     cells[k].resize(npot);
     probs[k].resize(npot);
     for (j = 0; j < npot; j++) {

       // The possilbe cells are: the current cell + cells that fall within the movement
       // kernel

       // row-major order
       // https://stackoverflow.com/questions/5991837/row-major-order-indices
       // index = X + Y * Width;
       // Y = (int)(index / Width)
       // X = index - (Y * Width)

       // to get cell, we use: x % nr + nc * y / nc
       // nc * (y - 1) + x
       // x = k mod nc
       // y = k / nr

       cells[k][j] = mod((k % nc) + mk(j, 0), nc) + nc * mod(((k / nc) + mk(j, 1)), nr);
       probs[k][j] = hk(cells[k][j], 1) * mk(j, 2);
     }
   }
   k = rsamp(cells[k], probs[k]);
   ud(k)++;
 }
 return ud;
}



Simulate SSF
//[[Rcpp::export]]

IntegerVector cpp_simulate_ssf(int n_steps, int start, int nc, int nr, NumericMatrix mk, NumericMatrix hk) {

 int step, j,  npot = mk.nrow(); // no options from the movement kernel (mk)
 int ncell = hk.nrow();

 vector<vector<int> > cells (ncell);  // create vector of pointers
 vector<vector<double> > probs (ncell);

 IntegerVector path(n_steps, 0);

 int k = start; // current position
 path(0) = k;

 for (step = 1; step < n_steps; step++) {

   // this needs to be done only once per cell
   if (cells[k].empty()) { // Allocate memory if needed

     cells[k].resize(npot);
     probs[k].resize(npot);
     for (j = 0; j < npot; j++) {
       cells[k][j] = mod((k % nc) + mk(j, 0), nc) + nc * mod(((k / nr) + mk(j, 1)), nr);
       probs[k][j] = hk(cells[k][j], 1) * mk(j, 2);
     }
   }
   k = rsamp(cells[k], probs[k]);
   path(step) = k;
 }
 return path;
}

