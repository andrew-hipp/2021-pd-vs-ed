#' Plot output from `eDisCalc`
#' 
#' @import(ggplot2)
#' @import(ggtree)
#' 
plot.eDis <- function(x, whichPhy = 'phy.full', whichDat = 'dat.full', whichDis = 'w') {
  if(class(x) == 'phylo') stop('please pass me an eDis object')
  phy <- x$x[[whichPhy]]
  dat <- x$x[[whichDat]]
  dis <- x[[whichPhy]][,whichDis, drop = F]
  p <- ggtree(phy, layout = 'rectangular', size = 0.25)
  p <- p + geom_tiplab(fontface='italic',
                       size = 1.7,
                       color = ifelse(phy$tip.label %in% row.names(dat), "black", "gray")
                       )
  if("node.label" %in% names(phy)) {
    p <- p + geom_label(aes(x=branch),
                        label = phy$node.label,
                        size = 2.5,
                        fill = 'gray',
                        label.padding = unit(0.18, "lines"),
                        label.r = unit(0.1, "lines"),
                        color = 'black')
  }
  p2 <- gheatmap(p, dis, width = 0.01,
                font.size = 0, offset = 0.08)
  p2 <- p2 + scale_fill_gradient(paste("Phylogenetic distinctiveness -", whichDis),
                low = "#000080",
                high = "#FFFF00")
  p2 <- p2 + theme(legend.position = c(0.1, 0.9),
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 7),
                  legend.key.size = unit(0.30, 'cm'),
                  legend.box.background = element_rect(color = NA)
                  )
}