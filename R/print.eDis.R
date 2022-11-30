#' A print method for `eDis` objects to avoid printing the tree and metadata

print.eDis <- function(x, ...) {
    print(x$phy.full)
    print(x$phy.sub)
    print(x$intersect)
}