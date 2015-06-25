library(graphics)
library(caTools)

# Creates a gif of the Mandelbrot set

createMandelbrot <- function(n = 1200, iter = 20, delay = 50){

    colors <- c("#00007F", "#0000FF", "#007FFF", "#00FFFF", "#7FFF7F", 
                "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")

    # Color Interpolation
    jet.colors <- colorRampPalette(colors)

    re <- rep(seq(-1.8, 0.6, length.out = n), each = n)
    im <- rep(seq(-1.2, 1.2, length.out = n), times = n)

    # Create n by n matrix of complex numbers
    C <- matrix(complex(real = re, imag = im), n, n)

    Z <- 0
    output <- array(0, c(n, n, iter))

    for(index in 1:iter){
        Z <- Z^2 + C
        output[, , index] <- exp(-abs(Z))
    }
    
    write.gif(output, "mandelbrot.gif", col = jet.colors, delay = delay)
}
