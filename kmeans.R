library(dplyr)
library(ggplot2)
library(GGally)

setwd("~/github/R/img")

# Load iris dataset
data(iris)
iris <- tbl_df(iris)

# Pairs plot
ggpairs(iris, columns = 1:4, mapping = aes(color = Species),
        upper = list(continuous = wrap("points")),
        lower = list(continuous = wrap("cor")),
        title = "Iris Pairs Plot" )

png("pairs.png")

# Euclidean distance
dist <- function(x, y){
    return (sqrt(sum((x - y)^2)))
}

# Compute which cluster to assign row
cluster <- function(row, centroids){
    means <- apply(centroids, 1, function(c){ dist(c, row)})
    return(which.min(means))
}

# Returns the sum of squared error
SSE <- function(df, centers){
    df %>% 
        rowwise %>%
        mutate(d = dist(c(Petal.Length, Petal.Width), centers[Species, ])) %>%
        select(d) %>%
        sum
}

#---------------------------------------------------
# Let's try K-means on petal length vs petal width

set.seed(26)
K <- 3
epsilon <- 0.0001

# Shuffle so that Species are more evenly distributed across observation order
permute <- sample(1:nrow(iris))
petal <- select(iris, Petal.Length, Petal.Width)[permute, ]

# Initial values for centers
centroids <- sample_n(petal, K) %>% mutate(Species = factor(1:K))
not.optimum <- TRUE
iteration <- 1

while(not.optimum){

    # Assign and label observations based on distance from centers
    groups <- apply(petal, 1, function(row){ cluster(row, centroids[, 1:2]) })
    petal.grp <- mutate(petal, Species = factor(groups))

    # Plot clustering
    df <-rbind(mutate(petal.grp, center = FALSE), 
               mutate(centroids, center = TRUE))

    ggplot(data = df, 
            aes(x = Petal.Length, y = Petal.Width, 
            color = Species, shape = center, size = center)) +
        geom_point()

    ggsave(paste("iteration", iteration, ".png", sep=""), width = 6, height = 6)
    
    # Update centroid means and preserve column order
    update <- petal.grp %>%
                group_by(Species) %>%
                summarise_each(funs(mean)) %>%
                select(Petal.Length, Petal.Width, groups)

    
    # Check if centers converged
    not.optimum <- FALSE    
    for(i in 1:K){
         if(dist(update[i, 1:2], centroids[i, 1:2]) > epsilon) {
             not.optimum <- TRUE
             centroids <- update
             break;
         }
             
    }
    
    iteration <- iteration + 1
}

# Compare SSE of actual data vs our result
centers <- centroids %>% 
                arrange(Species) %>% 
                select(-Species)

real_centers <- iris %>%
                    group_by(Species) %>%
                    summarise_each(funs(mean)) %>%
                    select(Petal.Length, Petal.Width)


sprintf("SSE for k-Means chosen group: %.4f", SSE(petal.grp, centers))
sprintf("SSE for original data: %.4f", 
        SSE(mutate(iris, Species = as.numeric(Species)), real_centers))



