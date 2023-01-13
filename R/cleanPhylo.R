#' Clean a phylogeny for single-tip-per-species analyses
#' Note that adding dataset to phylogeny is a separate step
#'
#' @import ape
## #' @import TreeSim
## #' @import phyloregion
#' @import picante
#'
#' @param tr A \code{phylo} object
#' @param delim Character delimiting species names from other info
#' @param tipElem Integer indicating which element of the tip name contains the species
#' @param uniquesOnly Boolean: retain only unique tip labels? Generally this is what you want
#' @param strFixed Boolean: use \code{fixed = TRUE} in \code{strsplit}?
#' @param gsubFromTo Character, \code{length = 2}; or \code{NA}: if character, use with \code{gsub} to clean tip labels
#' @param gsubFixed Boolean: use \code{fixed = TRUE} in \code{gsub}?
#' @param trimWhiteSpace Boolean: trim leading and trailing whitespace?
#'
#' @examples
#' data(malus_tr)
#' data(quercus_tr)
#' data(tilia_tr)
#' data(ulmus_tr)
#' malus2 <- cleanPhylo(malus, delim = '_|_')
#' quercus2 <- cleanPhylo(quercus)
#' tilia2 <- cleanPhylo(tilia)
#' ulmus2 <- ulmus
#' ulmus2$tip.label <- paste('Ulmus', ulmus2$tip.label)
#' @export
cleanPhylo <- function(tr, delim = '|', tipElem = 1,
                        uniquesOnly = TRUE, strFixed = TRUE,
                        gsubFromTo = c('_', ' '),
                        gsubFixed = TRUE,
                        trimWhiteSpace = TRUE) {
  labsDecomp <- strsplit(tr$tip.label, delim, fixed = strFixed)
  labsNew <- sapply(labsDecomp, '[', tipElem)
  if(uniquesOnly) {
    labsOut <- unique(labsNew)
    tr <- drop.tip(tr, which(duplicated(labsNew)))
    tr$tip.label <- labsOut
  } else {
    labsOut <- make.unique(labsNew)
    tr$tip.label <- labsOut
  }
  if(!is.na(gsubFromTo[1]))
    tr$tip.label <- gsub(gsubFromTo[1], gsubFromTo[2], tr$tip.label, fixed = gsubFixed)
  if(trimWhiteSpace) tr$tip.label <- trimws(tr$tip.label)
  return(tr)
}
