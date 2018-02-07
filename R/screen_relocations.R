# #' Screen relocations
# #'
# #' Function to screen relocations for possible errors. The algorithm was first described by Bjorneraas et al. 2010.
# #'
# #' @param x A `track_xyt`
# #' @param median_crit `[numeric(1)]{>0}` \cr Threshold for the median (in m).
# #' @param mean_crit `[numeric(1)]{>0}` \cr Threshold for the mean (in m).
# #' @param spike_speed `[numeric(1)]{>0}` \cr Threshold spike speed in m/s.
# #' @param spike_cos `[numeric(1)]{>0}` \cr Threshold for the cosine.
# #' @param window `[numeric(1)=10]{>2}` \cr Window width.
# #' @export
# #'
# #' @examples
# #' data(deer)
# #' screen_relocations(deer, 100, 100, 0.1, 0.3, 10)


screen_relocations <- function(x, median_crit, mean_crit, spike_speed, spike_cos, window) {

  ##
  ##

  ###get the points that are really far off, by using the median
  #sapply(1:nrow(x), function(i) round_one(i, df = x, win = window, what = "median"))
  #sapply(1:10, function(i) round_one(i, df = x, win = window, what = "median"))
  #x$R1dmed <- sapply(1:nrow(x), round_one, df = x, win = window, what = "median")
#
  ###get the points that are rather far off, by using the mean
  #x$R1dmean <- mean_crit + 1
  #x$R1dmean[x$R1dmed < median_crit] <- sapply(1:nrow(x[x$R1dmed < median_crit, ]),
  #                                        round_one, df = x[x$R1dmed < median_crit, ],
  #                                        win = window, what = "mean")
  #x$R1error <- ifelse(x$R1dmean > mea_ncrit, TRUE, FALSE)
#
  ## round two: find spikes
  #xT <- x[!x$R1error, ]
#
  #x$R2error <- NA
  #spd <- speed(x)
#
  #x$R2error[!x$R1error] <- spd > spike_speed &
  #  c(NA, spd[-nrow(xT)]) > spike_speed &
  #  cos(xT$rel.angle) < spike_cos
#
  #x

}



round_one <- function(i, df, win, what){

  # find point that are within the window
  if (i <= win){
    dfT <- df[1:2 * win,]
  } else if (i > (nrow(df) - win)){
    dfT <- df[(nrow(df) - 2 * win):(nrow(df)), ]
  } else if (i > win & i <= (nrow(df) - win)){
    dfT <- df[c((i - win):(i + win)),]
  }

  if (what == "mean"){
    #calculate the distance between the mean and the focal point
    temp <- sqrt((df$x[i] - mean(dfT$x, na.rm=TRUE))^2 +
                   (df$y[i] - mean(dfT$y, na.rm=TRUE))^2)

  } else if (what == "median"){
    #calculate the distance between the median and the focal point
    temp <- sqrt((df$x[i] - median(dfT$x, na.rm=TRUE))^2 +
                   (df$y[i] - median(dfT$y, na.rm=TRUE))^2)
  }
  temp
}

#library(amt)
#data(deer)
#
#median_crit <- 10000
#
#sqrt((deer$y_[15] - rolling_median(deer$y_[1:30], 5))^2)
#sqrt((deer$x_[15] - rolling_median(deer$x_[1:30], 5))^2)
#
#deer %>% mutate(
#  screening_flag = ifelse(sqrt(((x_ - rolling_median(x_, 5))^2 + (y_ - rolling_median(y_, 5))^2)) > median_crit, 1, 0)) %>%
#  mutate(
#    screening_flag = case_when(
#      screening_flag == 0 ~ ifelse(sqrt((x_ - rolling_mean(x_, 10))^2 + (y_ - rolling_mean(y_, 10))^2) > mean_crit, 2, screening_flag)),
#      TRUE ~ screening_flag
#    )
#  )
#
#
#speed(x) > spike_speed & c(NA, (head(speed, -1)) > spikespeed & cos(direction_rel(.)) < spike_cos
#return(x)
#}
#
