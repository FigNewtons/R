library(dplyr)
library(ggplot2)
library(dbscan)

set.seed(1337)

group <- function(label, N = 100, mean = 0, sd = 0.1){
    
    grp <- replicate(2, rnorm(N, mean, sd)) %>% 
           cbind(. , rep(label, N))
    
    return(grp)
}


df <- data.frame(
        rbind(group(1, mean = 1), 
              group(2, mean = 0),
              group(3, mean = -1))
        ) %>% tbl_df

colnames(df) <- c("X", "Y", "group")

ggplot(df, aes(X, Y, color = factor(group))) + geom_point()


dbscan.res <- dbscan(select(df, -group), 0.25)
kmeans.res <- kmeans(select(df, -group), 3)


ggplot(df, aes(X, Y, color = factor(dbscan.res$cluster))) + geom_point()
ggplot(df, aes(X, Y, color = factor(kmeans.res$cluster))) + geom_point()

# Compare K-means, dbscan, EM results




