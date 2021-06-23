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
#' @export
#' @import magrittr
#' @examples
data(accessions.mor) # read MOR accessions data
data(desiderata.mor) # read MOR desiderata list
temp <- c(accessions.mor$Taxon, desiderata.mor$taxon_name) %>%
   unique %>% sort
dat.mor <- data.frame(inGarden = temp %in% accessions.mor$Taxon,
                      wanted = temp %in% desiderata.mor$taxon_name,
                      row.names = temp)
rm(temp)
combo.malus <- make.consInt(malus, dat.mor)
combo.quercus <- make.consInt(quercus, dat.mor)
combo.tilia <- make.consInt(tilia, dat.mor)
combo.ulmus <- make.consInt(ulmus, dat.mor)

make.consInt <- function(phy, dat, matchCol = NA, imposeTidy = TRUE) {

}
