#' Calculate evolutionary distinctiveness

#' Calculates evolutionary distinctiveness for trees in a `consInt` object
#' @param x a `consInt` object
#' @import phyloregion
#' @export
ed.calc <- function(x) {
  if(class(x) != 'consInt') warning('Expecting a consInt object')
  intNames <- intersect(x$phy.full$tip.label, x$phy.sub$tip.label)
  out <- list(
    all = data.frame(
      all.ed.equalSplits = phyloregion::evol_distinct(x$phy.full, 'equal.splits'),
      all.ed.fairProportion = phyloregion::evol_distinct(x$phy.full, 'fair.proportion'),
      all.w = w.phy(x$phy.full)
    ),
    sub = data.frame(
      sub.ed.equalSplits = phyloregion::evol_distinct(x$phy.sub, 'equal.splits'),
      sub.ed.fairProportion = phyloregion::evol_distinct(x$phy.sub, 'fair.proportion'),
      sub.w = w.phy(x$phy.sub)
    )
  )
  out$intersect <- cbind(out$all[intNames, ], out$sub[intNames, ])
  out
}
