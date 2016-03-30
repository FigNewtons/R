library(dplyr)
library(ggplot2)
library(gridExtra)

set.seed(1337)

x1 <- rnorm(mean = 2, sd = 1, 100)
x2 <- rnorm(mean = 1, sd = 1, 100)

df <- cbind(x = c(x1, x2), class = rep(0:1, each = 100)) %>%
        as.data.frame %>%
        tbl_df

extrema <- round(range(df$x))
thresholds <- seq(extrema[1], extrema[2], by = .25)

get_rates <- function(t){
  
  tp <- sum(df$x < t & df$class) / sum(df$class) 
  fp <- sum(df$x < t & !df$class) / sum(!df$class)
 
  c(fp = fp, tp = tp)
}

rates <- sapply(thresholds, get_rates) %>% 
        t %>% 
        as.data.frame

hist <- ggplot(df, aes(x, fill = factor(class))) + 
            geom_histogram()

roc <- ggplot(rates, aes(fp, tp)) + 
        geom_path() +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")

grid.arrange(hist, roc, ncol = 2)