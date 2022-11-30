#' Calculate evolutionary distinctiveness

#' Calculates evolutionary distinctiveness for trees in a `consInt` object
#' @param x a `consInt` object
#' @import phyloregion
#' @examples
#' example(make.consInt)
#' temp <- eDisCalc(combo.quercus)
#' plot(all.ed.equalSplits ~ sub.ed.equalSplits, temp$intersect)
#' @export
eDisCalc <- function(x) {
  if(class(x) != 'consInt') warning('Expecting a consInt object')
  intNames <- intersect(x$phy.full$tip.label, x$phy.sub$tip.label)
  out <- list(
    phy.full = data.frame(
      ed.equalSplits = phyloregion::evol_distinct(x$phy.full, 'equal.splits'),
      ed.fairProportion = phyloregion::evol_distinct(x$phy.full, 'fair.proportion'),
      w = w.phy(x$phy.full)
    ),
    phy.sub = data.frame(
      ed.equalSplits = phyloregion::evol_distinct(x$phy.sub, 'equal.splits'),
      ed.fairProportion = phyloregion::evol_distinct(x$phy.sub, 'fair.proportion'),
      w = w.phy(x$phy.sub)
    )
  )
  out$intersect <- cbind(out$phy.full[intNames, ], out$phy.sub[intNames, ])
  out$x <- x
  class(out) <- 'eDis'
  return(out)
}
