#' Make a `consInt` object
#'
#' `make.consInt` welds together a `data.frame` with a `phylo` object to yield
#'   an object that can be analyzed for most analyses. The builder keeps
#'   the whole tree plus a tree subsetted to the table provided; and the whole
#'   table plus a table subsetted to the rows in the tree
#'
#' @param phy a `phylo` object
#' @param dat a `data.frame` object
#' @param matchCol the column in `dat` to match to the tree; if not provided,
#'          `make.consInt` will use row names
#' @param imposeTidy `boolean` -- forces trees to match data using tidyName
#' @param ... extra parameters to pass to `tidyName`
#' @import magrittr
#' @examples
#' ### 1st block redundant with cleanPhylo example
#' data(malus_tr)
#' data(quercus_tr)
#' data(tilia_tr)
#' data(ulmus_tr)
#' malus2 <- cleanPhylo(malus, delim = '_|_')
#' quercus2 <- cleanPhylo(quercus)
#' tilia2 <- cleanPhylo(tilia)
#' ulmus2 <- ulmus
#' ulmus2$tip.label <- paste('Ulmus', ulmus2$tip.label)
#'
#' data(accessions.mor) # read MOR accessions data
#' data(desiderata.mor) # read MOR desiderata list
#' temp <- c(accessions.mor$Taxon, desiderata.mor$taxon_name) %>%
#'    unique %>% sort
#' dat.mor <- data.frame(inGarden = temp %in% accessions.mor$Taxon,
#'                       wanted = temp %in% desiderata.mor$taxon_name,
#'                       row.names = temp)
#' rm(temp)
#' # combo.malus <- make.consInt(malus2, dat.mor) ## not working correctly yet
#' combo.quercus <- make.consInt(quercus2, dat.mor)
#' # combo.tilia <- make.consInt(tilia2, dat.mor) ## not working correctly yet
#' combo.ulmus <- make.consInt(ulmus2, dat.mor)
#'
#' @export
make.consInt <- function(phy, dat, matchCol = NA, imposeTidy = TRUE, ...) {
  dat.working <- dat
  if(!is.na(matchCol)) row.names(dat.working) <- dat[[matchCol]]
  if(imposeTidy) {
    whichRows <- match(tidyName(row.names(dat.working), ...),
                       tidyName(phy$tip.label, ...))
    row.names(dat.working)[!is.na(whichRows)] <-
      phy$tip.label[whichRows[!is.na(whichRows)]]
  }
  if(any(duplicated(phy$tip.label)))
    phy <- drop.tip(which(duplicated(phy$tip.label)))
  phy.sub <- drop.tip(phy, which(!phy$tip.label %in% row.names(dat.working)))
  dat.sub <- dat[phy.sub$tip.label, ]
  out <- list(phy.full = phy, phy.sub = phy.sub,
             dat.full = dat.working, dat.sub = dat.sub)
  class(out) <- 'consInt'
  return(out)
}
