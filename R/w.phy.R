#' Calculate phylogenetic distinctiveness
#'
#'
#' @param phy `phylo` object
#' @param useSqrt `boolean` - return sqrt?
#' @import ape
#' @import magrittr
#' @export
w.phy <- function(phy, useSqrt = TRUE) {
  require(magrittr)
  out <- ape::vcv.phylo(phy) %>%
    solve %>% colSums
  if(useSqrt) out <- sqrt(out)
  return(out)
}
