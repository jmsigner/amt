#include <Rcpp.h>
#include <stdlib.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector track_align_cpp(IntegerVector t1, IntegerVector nt, int time_tol, int type) {
  // Create variables
  int burst = 1;  // burst
  // int i = 0;  // obs
  int j = 0;  // new traj

  int n_obs = t1.size(); // number of points
  int n_new = nt.size(); // number of new points
  NumericVector out(n_obs);   // output

  // for each point in t1, find the closest point in nt
  for (int i = 0; i < n_obs; i++) {
    int low = 0;
    int high = n_new - 1;

    while(low < high) {
      int mid = (low + high) / 2;
      int d1 = abs(nt[mid] - t1[i]);
      int d2 = abs(nt[mid + 1] - t1[i]);

      if (d2 <= d1) {
        low = mid + 1;
      } else {
        high = mid;
      }
    }

    if (t1[i] > (nt[high] - time_tol) && t1[i] < (nt[high] + time_tol)) {
      out(i) = high;
    } else {
      out(i) = -1;
    }
  }

  // check each element of the new traj again and only keep the closest (if there are more than 2 points)
  int k = 0;
  int out_k;
  int dis_next;
  while(k < n_obs) {
    if (out[k] > -1) {
      out_k = out[k];
      int l = k;

      while (out[k] == out[l]) {
        if (t1[l] < nt[out_k]) {
          dis_next = abs(t1[l + 1] - nt[out_k]);
        } else {
          dis_next = abs(t1[l - 1] - nt[out_k]);
        }

        int dis_current = abs(t1[l] - nt[out_k]);
        if (dis_next < dis_current) {
          out[l] = -1;
        }
        l++;
      }
      k = l;
    } else {
      k++;
    }
  }

  // case: more than two points
  // case: no points


  // prep output
  if (type == 1) { // type: which
    return out;
  }

  if (type == 2) { // type: diff

    for (int i = 0; i < n_obs; i++) {
      if (out[i] != -1) {
        out[i] = nt[out[i]] - t1[i];
      }
    }
    return out;
  }

  if (type == 3) { // type: burst
    // assign unique bursts
    int diff = nt[1] - nt[0];

    // find first point in out that should be considered again
    int i = 0;
    while (out[i] == -1) i++;
    int old_i = nt[out[i]];
    out[i] = burst;

    for (i++; i < n_obs; i++) {
      if (out[i] > -1) {
        if ((nt[out[i]] - old_i) != diff) {
          burst++;
        }
        old_i = nt[out[i]];
        out[i] = burst;
      }
    }
    return(out);
  }
  return(out);
}



// [[Rcpp::export]]
NumericVector mk_reg(NumericVector t1, int time_dist, int time_tol, int start) {
  // Create variables
  int k = 1;  // burst
  int i = start - 1;  // counter of points
  int n = t1.size(); // number of points
  NumericVector out(n);   // output
  int t_max, t_min;

  // all positions that are left out in the beginning are 0ed
  for (int j = 0; j < i; j++)
    out[j] = -1;

  out[i] = k;
  while(i < (n - 1)) {
    // Rcout << "The value is " << i << std::endl;

    t_min = t1[i] + time_dist - time_tol;
    t_max = t1[i] + time_dist + time_tol;
    // bring j to the right position
    int j = i + 1;
    while((j < (n - 1)) && (t1[j] < t_min)) {
      out[j] = -1;
      j++;
    }

    i = j;

    if (j == (n - 1) && t1[j] < t_min) {
      out[j] = -1;
    } else if (t1[j] >= t_min && t1[j] <= t_max) {
      out[j] = k;
    } else {
      k++;
      out[j] = k;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector mk_reg_old(NumericVector relocs_time, int time_dist, int time_tol, int start) {
  // Create variables
  int k = 1;  // burst
  int i = start - 1;  // counter of points
  int n = relocs_time.size(); // number of points
  NumericVector out(n);   // output
  int t_max, t_min;

  // all positions that are left out in the beginning are 0ed
  for (int j = 0; j < i; j++)
    out[j] = 0;

  out[i] = k;


  while(i < (n - 1)) {

    t_min = relocs_time[i] + time_dist - time_tol;
    t_max = relocs_time[i] + time_dist + time_tol;

    // bring j to the right position
    int j = i + 1;
    while((j < (n-1)) && (relocs_time[j] < t_min)) {
      out[j] = 0;
      j++;
    }

    i = j;

    if (relocs_time[j] >= t_min && relocs_time[j] <= t_max) {
      out[j] = k;
    } else if (j == (n - 1)) {
      out[j] = 0;
    } else {
      k++;
      out[j] = k;
    }
  }
  return out;
}


//// 
//NumericVector duration_acuracy(NumericVector x_, NumericVector y_, NumericVector t_,
//                               NumericVector dop, NumericVector dim, int duration_accuracy) {
//
//  int i, j, k, n = x_.size();
//
//  NumericVector out(n);
//
//  for (i = 1; i < n; i++) {
//    // forward
//    // bring j into the right position
//    for (j = i; (j < n) && std::abs(t_[j] - t_[i]) <= duration_accuracy; j++) ;
//
//    int better = 0; // this indicates a better point
//    int fix_sl = pow(x_[i] - x_[i-1], 2) + pow(y_[i] - y_[i - 1], 2);
//
//
//    if (j > i) {
//      for (k = i + 1; k < j; k++) {
//        if (dop[i] < dop[k]) {
//          better = 1;
//          break;
//        } else if (dim[k] > dim[i]) {
//          better = 1;
//          break;
//        } /*else if (pow(x_[k] - x_[i-1], 2) + pow(y_[k] - y_[i - 1], 2) < fix_sl) {
//          better = 1;
//          break;
//        } */
//      }
//    }
//
//    if (better == 0) {
//      // backward
//      // bring j into the right position
//      for (j = i; (j >= 0) && std::abs(t_[i] - t_[j]) <= duration_accuracy; j--) ;
//      if (j < i) {
//        for (k = i - 1; k > j; k--) {
//          if (dop[i] < dop[k]) {
//            better = 1;
//            break;
//          } else if (dim[k] > dim[i]) {
//            better = 1;
//            break;
//          }
//          /*else if (pow(x_[k] - x_[i-1], 2) + pow(y_[k] - y_[i - 1], 2) < fix_sl) {
//            better =k;
//            break;
//          }
//           */
//        }
//      }
//    }
//    out[i] = better;
//  }
//  return out;
//}

// [[Rcpp::export]]
NumericVector track_immobility(NumericVector t, NumericVector x, NumericVector y, double period, double tol) {

  int n = x.length();
  NumericVector out(n);
  for (int i = 0; i < n; i++) out(i) = -1;
  double immobile_period = 0;
  int immo_started;

  int i = 0;

  while (i < n && immobile_period < period) {
    double dist = sqrt(pow(x[i] - x[i + 1], 2) + pow(y[i] - y[i + 1], 2));
    if (dist < tol) {
      immobile_period += t[i+1] - t[i];
    } else {
      immobile_period = 0;
      immo_started = i + 1;
    }
    out[i] = 1;
    i++;
  }

  for (int k = immo_started; k < i; k++)
    out[k] = -1;

  return out;
}



