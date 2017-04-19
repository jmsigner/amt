
library(amt)
library(lubridate)

x <- cumsum(rnorm(100))
y <- cumsum(rnorm(100))
t <- ymd_hms("2017-01-01 00:00:00") + minutes(1:100)

xy <- track(x = x, y = y, t = t)

# lets delete some relocations
xy <- xy[-c(5:10, 50, 89:90), ]

# resample to one min sampling rate
trk <- track_resample(xy, rate = minutes(1), tolerance = seconds(5))
trk

# this adds a new column: burst_
table(trk$burst_)

# we cann filter to keep only burst with at least 20 relocations
trk <- filter_min_n_burst(trk, 20)
table(trk$burst_)


# A second way is to align a track to a pre defined time series

n_tr <- ymd_hms("2017-01-01 00:00:00") + minutes(seq(0, 100, 5))
track_align(trk, n_tr, minutes(1))
class(trk)
