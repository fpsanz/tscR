#' @noRd
#'
fcolor <- function(x) {
    f <- colorRamp(brewer.pal(11, "Spectral"))
    rr <- range(x)
    svals <- (x - rr[1])/diff(rr)
    colorClust <- rgb(f(svals)/255)
    return(colorClust)
}
