
# Take discrete time t = 0, 1, ..., N
# Since each time point is a unit apart, we can sample from each t
# with the same standard normal distribution ~ N(O, 1)

library(dplyr)
library(ggplot2)

brownian2D <- function(N = 1000){
    xdis <- cumsum(rnorm(N))    
    ydis <- cumsum(rnorm(N))
    color <- seq(N) / N
    
    trail <- tbl_df(data.frame(xdis, ydis, color))
    
    ggplot(trail, aes(xdis, ydis, color = color)) +
        geom_path(alpha = I(1/2)) + 
        labs(title = "Brownian motion in 2D", 
             x = "x Displacement", 
             y = "y Displacement",
             color = "Time") +
        geom_point(data = trail[seq(1, N, length.out = 4), ],
                   aes(xdis, ydis, color = color),
                   size = 6)
}









