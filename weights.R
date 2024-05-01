tot_vol <- function(cadence, weights) {
  vol <- sum(cadence * weights)
  day <<- day + vol
  vol
}