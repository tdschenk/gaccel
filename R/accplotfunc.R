#' Plot x, y, z, accelerometer data w.r.t. time
#'
#' This function generates a plot of accelerometer data 
#' w.r.t. time using ggplot. Options for selecting which
#' variables (x, y, or z) are to be plotted are available.
#'
#' @param dir location of the bin file to be read
#' @param x boolean of whether x is to be plotted
#' @param y boolean of whether y is to be plotted
#' @param z boolean of whether z is to be plotted
#' @export
#' @import GENEAread reshape ggplot2
accplot <- function(dir, x = TRUE, y = TRUE, z = TRUE) {
  # Read the bin
  file <- read.bin(dir)
  
  # Place time, x, y, z into data frames 
  dfx <-data.frame(time = file$time, Acc.x = file$x)
  dfy <-data.frame(time = file$time, Acc.y = file$y)
  dfz <-data.frame(time = file$time, Acc.z = file$z)
  df <- data.frame(time = file$time)
  
  # Merge requested dataframes
  if (x) {
    df <- merge(df, dfx, by = "time")
  }
  if (y) {
    df <- merge(df, dfy, by = "time")
  }
  if (z) {
    df <- merge(df, dfz, by = "time")
  }
  
  # testing GRTime formatting
  df$time <- as.POSIXct(format(df$time, format = "%Y-%m-%d %H:%M:%S"))
  
  # Melt into new dataframe using 'reshape' package
  df.melt <- melt(df, id="time")
  
  # Plot w.r.t. time using 'ggplot2' package
  ggp <- ggplot(df.melt, aes(x=time, y=value, color=variable)) + geom_line() +
    labs(title = "GENEActiv Accelerometer", 
         x = "Time", y = "Acceleration (g)", 
         color = "Direction") +
    scale_color_discrete(breaks=c("Acc.x", "Acc.y", "Acc.z"),
                         labels=c("X-axis", "Y-axis", "Z-axis"))
  
  ggp
}