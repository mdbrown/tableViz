#' The length of a string (in characters).
#'
#' @param string input character vector
#' @return numeric vector giving number of characters in each element of the 
#'   character vector.  Missing string have missing length.
#' @keywords character
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' str_length(letters)
#' str_length(c("i", "like", "programming", NA))
#' 
#' 

tableViz <- function(x, colNames, rowNames, type){
  
  x <- matrix(c(rnorm(10, mean = 1), 
                rnorm(10, mean = 0), 
                rnorm(10, mean = -1)), nrow = 5)
  
  
  colNames <- c("Pneg", "Theta", "Mean(SE)", "Emprical SE", "se", "mean")
  rowNames <- c("AA","BB","CC","DD", "EE")
  
  # Create the data frame
  df <- expand.grid(colNames, rowNames)
  df$value <- round(c(t(x)), 2)
  df$pos <- rep(0, nrow(df))
  
  #Plot the Data
  g <- ggplot(df, aes(x = Var1, y =Var2)) + 
       geom_point(aes(size = value), color = "grey80") + 
       theme_bw() + xlab("") + ylab("")
  g<- g + scale_size_continuous(range=c(10,25)) +
    geom_text(aes(label = value, size=), size = 6) + theme(legend.position ="none") 
  #+ facet_grid(Var2~Var1, scales="fixed")
  g <- g + theme(axis.ticks = element_blank(), 
            axis.line = element_blank(), 
            line = element_blank(), 
            panel.grid.minor = element_blank(), 
            text= element_text(size = 18, face="bold"), 
            strip.text.y = element_text(angle = 0), 
            strip.text.x = element_blank(), 
            panel.border=element_blank())
  
  

 g = g + annotation_custom(grob = linesGrob(c(0,1), c(1,1))) +
         annotation_custom(grob = linesGrob(c(0,0), c(0,1)))
  
  
}



library(gridExtra)





switch_facet_strip <- function(p, switch = c("x", "y")) {
  
  require(gtable)
  rbind_gtable <- gtable:::rbind_gtable
  cbind_gtable <- gtable:::cbind_gtable
  
  if ("y" %in% switch)
    p <- p + theme(strip.text.y = element_text(vjust = 0.5, angle = 0))
  
  g <- ggplotGrob(p)
  browser()
  gdim <- as.numeric(g$layout[g$layout$name == "background", c("b", "r")])
  tpos <- g$layout[g$layout$name == "strip-top", "b"][1]
  rpos <- g$layout[g$layout$name == "strip-right", "r"][1]
  new_tpos <- g$layout[g$layout$name == "axis-b", "b"][1] + 1
  new_rpos <- g$layout[g$layout$name == "axis-l", "r"][1] - 1
  
  if ("x" %in% switch) {
    g <- rbind_gtable(
      rbind_gtable(
        gtable_add_rows(
          rbind_gtable(g[1:tpos-1, ] , g[(tpos+1):(new_tpos-1), ], "first"),
          unit(5, units = "mm")),
        g[tpos, ], "first"),
      g[new_tpos:gdim[1], ], "first")
  }
  
  if ("y" %in% switch) {
    g <- cbind_gtable(
      cbind_gtable(
        gtable_add_cols(
          cbind_gtable(g[, 1:new_rpos], g[, rpos], "first"),
          unit(5, units = "mm")),
        g[, (new_rpos+2):rpos-1], "first"),
      g[, (rpos+1):gdim[2]], "first")
  }
  
  grid.newpage()
  grid.draw(g)
}