#' Plot output from `eDisCalc`
#' 
#' @import(ggplot2)
#' @import(ggtree)
#' 
plot.eDis <- function(x, whichPhy = 'phy.full', whichDat = 'dat.full', whichDis = 'w',
                      tSize = 3, tFill = 'gray', tPad = unit(0.1, "lines"), tR = unit(0.1, "lines"), tCol = 'black',
                      hWidth = 0.1, hOff = 1, legPos = c(0.15, 0.95)) {
  if(!'x' %in% names(x)) stop('please pass me an eDis object')
  phy <- x$x[[whichPhy]]
  dat <- x$x[[whichDat]]
  dis <- x[[whichPhy]][,whichDis, drop = F]
  p <- ggtree(phy, layout = 'rectangular', size = 0.25)
  p <- p + geom_tiplab(fontface='italic',
                       size = tSize,
                       color = ifelse(phy$tip.label %in% row.names(dat), "black", "gray")
                       )
  if("node.label" %in% names(phy)) {
    p <- p + geom_label(aes(x=branch),
                        label = phy$node.label,
                        size = t,
                        fill = tFill,
                        label.padding = tPad,
                        label.r = tR,
                        color = tCol)
  }
  p2 <- gheatmap(p, dis, width = hWidth,
                font.size = 0, offset = hOff)
  p2 <- p2 + scale_fill_gradient(
                paste("Phylogenetic distinctiveness -", whichDis),
                low = "#000080",
                high = "#FFFF00")
  p2 <- p2 + theme(legend.position = legPos,
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 7),
                  legend.key.size = unit(0.30, 'cm'),
                  legend.box.background = element_rect(color = NA)
                  )
}