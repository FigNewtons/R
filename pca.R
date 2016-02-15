library(ISLR)
library(dplyr)
library(ggplot2)

# TODO: Finish exploring this dataset
runNCI <- function(){

    labs <- NCI60$labs
    data <- NCI60$data
    
    # Cross-tabulation / count cancer types
    table(labs)

    # 64 cancer cell lines x 6830 genes
    dim(data)
    apply(data, 1, range)

    # Run PCA 
    pca <- prcomp(data, scale = TRUE)
    distances <- data %>% scale %>% dist

    # Plot dendograms
    par(mfrow = c(1, 3))
    plot(hclust(distances), labels = labs, main = "Complete linkage")
    plot(hclust(distances, method = "average"), 
                labels = labs, main = "Average linkage")
    plot(hclust(distances, method = "single"), 
                labels = labs, main = "Single Linkage")

}

runIris <- function(){

    iris <- tbl_df(iris)
    data <- select(iris, -Species)

    pca <- prcomp(data, scale = TRUE)

    pca$center == apply(data, 2, mean)
    pca$scale  == apply(data, 2, sd)

    projected <- data.frame(pca$x, Species = iris$Species)

    ggplot(projected, aes(x = PC1, y = PC2, color = Species)) + 
        geom_point()

}

runSample <- function(){

    set.seed(137)
    N <- 20

    df <- data.frame(x = rnorm(N), y = rnorm(N), z = 1:N)

    closeness <- df %>% select(x, y) %>% dist
    tree <- hclust(closeness)

    # Side-by-side comparison
    par(mfrow = c(1,2))
    plot(df$x, df$y, type = "n")
    text(df$x, df$y, label = df$z)
    plot(tree)

    # Cut tree to form 3 clusters
    clusters <- cutree(tree, k = 3)

    df <- cbind(df, clusters)

    ggplot(df, aes(x, y, label = z, color = factor(clusters))) + 
        geom_point() + 
        geom_text(hjust = 0, nudge_x = 0.05, size = 5, fontface = "bold")

}



