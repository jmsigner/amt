#include <Rcpp.h>
#include <ctime>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <math.h>



/* name spaces */
using namespace Rcpp;
using namespace std;

// Thanks: http://stackoverflow.com/questions/4003232/how-to-code-a-modulo-operator-in-c-c-obj-c-that-handles-negative-numbers
//[[Rcpp::export]]
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
int rsamp(int num, NumericVector probs) {
  double sum_of_weight = 0, rnd;

  // sum weights
  for (int i = 0; i < probs.length(); i++) {
    sum_of_weight += probs[i];
  }

  rnd = frand(sum_of_weight);

  for(int i = 0; i < num; i++) {
    if(rnd < probs[i])
      return i;
    rnd -= probs[i];
  }
  return 0;
}


//[[Rcpp::export]]
NumericMatrix dispersal_kernel  (
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

// This not funcitonal at the moment
/*
 NumericMatrix simulate_track (
     int cur_x, int cur_y, int nc, int nr,
     NumericMatrix dk, NumericVector coefs,
     int standardize,
     IntegerVector first_order_terms,
     IntegerVector second_order_terms,
     NumericMatrix hab,
     NumericMatrix other_covars,
     IntegerVector other_covars_indicator,
     int stop, int n
 ) {

   /*
    * res: is the result matrix
    * The columns are:
    *  - x1
    *  - y1
    *  - x2
    *  - y2
    *  - sl_
    *  - ta_

   NumericMatrix res(n, 6);
   NumericMatrix this_other_covar (1, other_covars.ncol());
   int x = cur_x;
   int y = cur_y;
   double dir;

   // dispersal kernel
   int nc_dk = dk.nrow();
   NumericMatrix cells(nc_dk, 3);

   for (int i = 0; i < n; i++) {

     this_other_covar(0, _) = other_covars(i,_);

     // Save start x and y
     res(i, 0) = x;
     res(i, 1) = y;

     cells = dispersal_kernel(x, y, nc, nr,
       dk, coefs,
       standardize,
       first_order_terms,
       second_order_terms,
       hab,
       this_other_covar,
       other_covars_indicator, stop);

     // Stop if outside the landscape
     if (cells(0,2) == -9999) {
       return res;
     }

    // Sample one cell
    int wc = rsamp(nc_dk, cells(_,2));

    // Update x and y
    x = cells(wc, 0);
    y = cells(wc, 1);

    dir = atan2_north_cpp(y - res(i, 1), x - res(i, 0));
    dk(_,4) = get_angle_cpp2(dk(_, Range(0, 1)));

    // Save updated x and y
    res(i, 2) = x;
    res(i, 3) = y;

    // Save dir and sl_
    res(i, 4) = dir;
    res(i, 5) = dk(wc, 2);
   }
   return res;
 }


*/
