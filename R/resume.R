
#' Suppression and Information Loss Metrics
#' 
#' @param dat \code{eclass} object
#' @param kanon k of k-Anonymity
#' @param maxsup threshold of k
#' @param ...
#' @return Suppression and Information Loss Metrics. Information Loss Metrics are Discernbility Metric (DM) and Entropy
resume <- function(dat, kanon = 3, maxsup = 0.01, ...){
  # Description : Suppression  & Information Loss Computation
  #
  # Arguments
  # dat : equiClass Data Object
  # kanon : k of k-Anonymity (default: 3)
  # maxsup : default (0.01), 0.05 or 0.1 
  
  # 필요 패키지 불러오기
  require(pbapply)
  require(plyr)
  
  # Suppression
  supp.res <- unlist(pblapply(1:length(dat), function(x){
    supp <- sum(dat[[x]]$equi.size < kanon) / sum(dat[[x]]$equi.size)
    supp
  }))
  
  # Information Loss
  loss.res <- pblapply(1:length(dat), function(x){
    # Discernibility Metric
    DM <- sum(dat[[x]]$equi.size[which(dat[[x]]$equi.size >= 3)]^2) +
      sum(sum(dat[[x]]$equi.size) * dat[[x]]$equi.size[which(dat[[x]]$equi.size < 3)])
    
    # Entropy
    ENTROPY <- -sum(dat[[x]]$equi.size * log2(dat[[x]]$equi.size / sum(dat[[x]]$equi.size)))
    
    c(DM, ENTROPY)
  })
  
  loss.res <- ldply(loss.res) # list to data.frame
  names(loss.res) <- c('DM', 'ENTROPY') # column names
  
  # 
  resume.res <- data.frame(supp = supp.res, 
                           supp_rank = factor(rank(supp.res)), 
                           optimal = ifelse(supp.res < maxsup, '*', ''),
                           DM = loss.res$DM,
                           DM_rank = factor(rank(loss.res$DM)),
                           Entropy = loss.res$ENTROPY,
                           Ent_rank = factor(rank(loss.res$ENTROPY)))
  
  # Class 지정
  class(resume.res) <- c('resume', 'data.frame')
  
  # Attributes 추가
  attr(resume.res, 'kanon') <- which(resume.res$optimal == '*') # k-anonymous를 만족하는 노드의 row number
  attr(resume.res, 'n_supprank') <- unique(resume.res$supp_rank) # suppression rank의 unique()
  attr(resume.res, 'n_dmrank') <- unique(resume.res$DM_rank) # DM rank의 unique()
  attr(resume.res, 'n_entrank') <- unique(resume.res$Ent_rank) # Entropy rank의 unique()
  
  return(resume.res)
}



#' Plot
#' 
#' @param obj \code{resume} object
#' @param ...
#' @return Plot
plot.resume <- function(obj, ...){
  
  require(ggplot2)
  
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  } 
  
  # suppression
  s <- ggplot(data = obj, aes(x = supp_rank, y = supp)) + geom_line(aes(group=1)) + geom_point()
  # DM
  d <- ggplot(data = obj, aes(x = DM_rank, y = DM)) + geom_line(aes(group=1)) + geom_point()
  # Entropy
  e <- ggplot(data = obj, aes(x = Ent_rank, y = Entropy)) + geom_line(aes(group=1)) + geom_point()
  
  multiplot(s, d, e)  
}
