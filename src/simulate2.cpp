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
  return runif(1, 0, upper)(0);
  // Remove C implementation in favor for R to remove check note
  //double f = (double)rand() / RAND_MAX;
  //return f * upper;
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
  return 0;
}

// Simulate UD new try
//[[Rcpp::export]]

NumericMatrix simulate_2(
    int n_steps, int start, int nc, int nr,
    NumericMatrix hab, NumericMatrix dk) {
  // hab: the habitat matrix of dimension: n * p
  //    - n: number of cells
  //    - p: number of predictors
  // dk: the offset of each cell in the movement kernel to the current cell. Dimension: k * 2
  //    - k: number of cells in the dispersal kernel
  //    - columns: offset in x and y direction

  // Todo: - non spatial covariates (that are the same for each step, e.g., time of day)

  int npot = dk.nrow(); // number of options cells possible move to (-> # cells in the movement kernel)

  IntegerVector cells (npot);  // create vector of pointers
  NumericVector probs (npot);

  // define output here
  // IntegerVector ud(hk.nrow(), 0);

  int p = start; // current position
  // ud(k)++;

  // step is just repeating the simulations n_steps time
  // and not really neeeded in the simulations

  // The variable k holds the current cell number
  for (int step = 1; step < n_steps; step++) {
    for (int k = 0; k < npot; k++) {

      // The possilbe cells are: the current cell + cells that fall within the movement
      // kernel

      // row-major order
      // https://stackoverflow.com/questions/5991837/row-major-order-indices
      // index = X + Y * Width
      // Y = (int)(index / Width)
      // X = index - (Y * Width)

      // to get cell, we use: x % nr + nc * y / nc
      // nc * (y - 1) + x
      // x = k mod nc
      // y = k / nr

      // (p % nc): gives the x coord of the current cell
      // dk(k, 0): is the offset of cell in the dispersal kernel in x direction
      // mod(....., nc): translates the x-part back to the cell number

      // (p / nc): gives the y coord of the current cell
      // dk(k, 1): is the offset in y direction of the dispersal kernel
      // nc * mod(...., nr) translates the y-part back to the cell number


      cells[k] = mod((p % nc) + dk(k, 0), nc) + nc * mod(((p / nr) + dk(k, 1)), nr);

      // now calculate for each cell the probability of moving there
      probs[k] = 0;
      for (int j = 0; j < p; j++) {
        probs[k] += coef[j] * hab[cells[k], j];
      }
      // probs[k] = hk(cells[k][j], 1) * mk(j, 2);
    }
  }
  //k = rsamp(cells[k], probs[k]);
  //ud(k)++;

  NumericMatrix out(npot, 2);
  for (int i = 0; i < npot; i++) {
    out(i,0) = cells[i] % nc;
    out(i,1) = cells[i] / nr;
  }

  return out;
}
