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



// Simulate SSF
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


//[[Rcpp::export]]
NumericMatrix dispersal_kernel_cpp (
    int cur_x, int cur_y, int nc, int nr,
    NumericMatrix dk, NumericVector coefs,
    int standardize,
    IntegerVector first_order_terms,
    IntegerVector second_order_terms,
    NumericMatrix hab,
    NumericMatrix other_covars,
    IntegerVector other_covars_indicator,
    int stop
) {
  int p_hab = 0, p_other = 0, p = 0, npot, cell, cell_cur, new_x, new_y;

  if (other_covars_indicator[0] == 1) {
    p_hab = hab.ncol();
  }

  if (other_covars_indicator[1] == 1) {
    p_other = other_covars.ncol();
  }

  // p_hab is used twice, because we store beginning and end of step
  p = 3 + p_hab + p_hab + p_other; // sl, log_l, ta + env * 2 (start and end) + (other)
  npot = dk.nrow();
  NumericMatrix design_matrix(npot, p);
  NumericMatrix res(npot, 3);
  NumericMatrix stop_return(1,1);
  stop_return(0,0) = -9999;

  int cur_cell = cur_x + cur_y * nc;

  // loop over all positions in the dispersal kernel
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

    new_x = cur_x + dk(k, 0);
    new_y = cur_y + dk(k, 1);


    // check if we stepped out of the world
    if (new_x < 0) {
      if (stop == 1) {
        return stop_return;
      } else {
        new_x = mod(new_x, nc);
      }
    }

    if (new_y < 0) {
      if (stop == 1) {
        return stop_return;
      } else {
        new_y = mod(new_y, nr);
      }
    }

    if (new_x > nc) {
      if (stop == 1) {
        return stop_return;
      } else {
        new_x = mod(new_x, nc);
      }
    }

    if (new_y >= nr) {
      if (stop == 1) {
        return stop_return; }
      else {
        new_y = mod(new_y, nr);
      }
    }

    res(k, 0) = new_x;  // x of the dispersal kernel
    res(k, 1) = new_y;  // y of the dispersal kernel

    // The cell of the potential position
    cell = new_x + new_y * nc;

    // prepare design matrix
    design_matrix(k, 0) = dk(k, 2); // sl_
    design_matrix(k, 1) = dk(k, 3); // log_sl_
    design_matrix(k, 2) = dk(k, 4); // cos of ta

    // start
    int col_counter = 3;
    if (p_hab > 0) {
      for (int i = 0; i < p_hab; i++) {
        design_matrix(k, col_counter) = hab(cur_cell, i); // value at current cell (start)
        col_counter++;
        design_matrix(k, col_counter) = hab(cell, i); // value at potential cell (end)
        col_counter++;
      }
    }

    // other
    if (p_other > 0) {
      for (int i = 0; i < p_other; i++) {
        design_matrix(k, col_counter) = other_covars(0, i);
        col_counter++;
      }
    }
  }

  // Multiply design matrix with coefficients
  double sel;
  for (int i = 0; i < npot; i++) {
    for (int j = 0; j < first_order_terms.length(); j++) {
      if (second_order_terms[j] != -1) {
        sel = design_matrix(i, first_order_terms[j]) * design_matrix(i, second_order_terms[j]);
      } else {
        sel = design_matrix(i, first_order_terms[j]) ;
      }
      res(i, 2) += sel * coefs[j]; // Matrix multiplication is done here already
    }
  }

  // Exponiate here
  res(_, 2) = exp(res(_, 2));

  // Standardize return
  if (standardize == 1) {
    res(_, 2) = res(_, 2) / sum(na_omit(res(_, 2)));
  }
  return res;
}

// Calcualte atan2, but 0 is north
//[[Rcpp::export]]
double atan2_north_cpp(double y, double x) {
  double ta = atan2(y, x) - (M_PI / 2);
  if (ta < -M_PI) {
    ta = ta + 2 * M_PI;
  }
  if (ta > M_PI) {
    ta = ta - 2 * M_PI;
  }
  return ta;
}


//[[Rcpp::export]]
NumericVector get_angle_cpp(NumericMatrix xy, double dir = 0) {
  int n = xy.nrow();
  NumericVector res(n);
  double tmp;

  for (int i = 0; i < n; i++) {
    tmp = atan2(xy(i, 1), xy(i, 0)) - (M_PI/2) + dir;
    if (tmp < -M_PI) {
      tmp = tmp + 2 * M_PI;
    }
    if (tmp > M_PI) {
      tmp = tmp - 2 * M_PI;
    }
    res[i] = cos(tmp);
  }
  return res;
}


double cos_t(double x) {
  int fac2 = 2;
  int fac4 = 24;
  int fac6 = 720;
  int fac8 = 40320;

  double xp2 = x * x;
  double xp4 = xp2 * xp2;
  double xp6 = xp2 * xp4;
  double xp8 = xp4 * xp4;

  return 1 - xp2 / fac2 + xp4 / fac4 - xp6 / fac6 + xp8 / fac8;

}

//[[Rcpp::export]]
NumericVector get_angle_cpp1(NumericMatrix xy, double dir = 0) {
  int n = xy.nrow();
  NumericVector res(n);
  double tmp;

  for (int i = 0; i < n; i++) {
    tmp = atan2(xy(i, 1), xy(i, 0)) - (M_PI/2) + dir;

    if (tmp < -M_PI) {
      tmp = tmp + 2 * M_PI;
    }
    if (tmp > M_PI) {
      tmp = tmp - 2 * M_PI;
    }

    res[i] = cos_t(tmp);
  }
  return res;
}

// https://gist.github.com/volkansalma/2972237
float atan2_approximation1(float y, float x)
{
  //http://pubs.opengroup.org/onlinepubs/009695399/functions/atan2.html
  //Volkan SALMA

  const float ONEQTR_PI = M_PI / 4.0;
  const float THRQTR_PI = 3.0 * M_PI / 4.0;
  float r, angle;
  float abs_y = fabs(y) + 1e-10f;      // kludge to prevent 0/0 condition
  if ( x < 0.0f )
  {
    r = (x + abs_y) / (abs_y - x);
    angle = THRQTR_PI;
  }
  else
  {
    r = (x - abs_y) / (x + abs_y);
    angle = ONEQTR_PI;
  }
  angle += (0.1963f * r * r - 0.9817f) * r;
  if ( y < 0.0f )
    return( -angle );     // negate if in quad III or IV
  else
    return( angle );


}

//[[Rcpp::export]]
NumericVector get_angle_cpp2(NumericMatrix xy, double dir = 0) {
  int n = xy.nrow();
  NumericVector res(n);
  double tmp;

  for (int i = 0; i < n; i++) {
    tmp = atan2_approximation1(xy(i, 1), xy(i, 0)) - (M_PI/2) + dir;

    if (tmp < -M_PI) {
      tmp = tmp + 2 * M_PI;
    }
    if (tmp > M_PI) {
      tmp = tmp - 2 * M_PI;
    }

    res[i] = cos_t(tmp);
  }
  return res;
}
