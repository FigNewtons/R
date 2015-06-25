

# Plotting in 2D and 3D Examples


# Draw a torus/sphere (using spherical coordinates)
drawTorus <- function(r = 1, R = 0){

    # Set paramater for "zoom"
    par(mar = c(0.5,0.5,0.5,0.5))

    x <- seq(0, 2 * pi, length.out = 50)
    y <- seq(0, pi, length.out = 50)

    M <- mesh(x,y)
    
    alpha <- M$x
    beta <- M$y

    surf3D( x = (R + r * cos(alpha)) * cos(beta),
            y = (R + r * cos(alpha)) * sin(beta),
            z = r * sin(alpha),
            colkey = FALSE,
            bty = "b2")
}