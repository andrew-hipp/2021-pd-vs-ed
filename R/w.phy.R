#' Calculate phylogenetic distinctiveness
#'
#' 
#' @param phy `phylo` object
#' @import ape
#' @import magrittr
#' @export
w.phy <- function(phy) {
  require(magrittr)
  out <- ape::vcv.phylo(phy) %>%
    solve %>% colSums
  return(out)
}
