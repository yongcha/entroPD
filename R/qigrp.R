#' Quasi-Identifiers Grouping
#' 
#' @param dat \code{parda} object
#' @param obj \code{lanode} object
#' @param ...
#' @return Grouped Data by each lattice node
qigrp <- function(dat, obj, mcs = 4, ...){
  # Description : QI grouping!
  #
  # Arguments
  # dat : (QI, TA) subest data
  # obj : lanode object
  
  require(mgcv) ## uniquecombs() : find the unique rows in a matrix
  
  # mclapply.hack.R sourcing
  source('http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R')
  
  # Data to be grouped
  data.QI.GH <- dat
  
  
  # QI Class 확인
  # Numeric : TRUE, Factor : FALSE
  qi.class <- unlist(lapply(names(data.QI.GH[, -ncol(data.QI.GH)]), 
                            function(x) is.numeric(data.QI.GH[, x])))
  
  #### QI grouping
  data.QI.GH.res <- mclapply(1:length(obj), function(x){
    for(i in 1:length(qi.class)){
      
      # CASE I : Numeric QI
      if(qi.class[i] == T){
        
        # rpart result : EXIST
        if(class(obj[[x]][[i]]) == 'rpart' && class(obj[[x]][[i]]$splits) == 'matrix'){
          split.point <- obj[[x]][[i]]$splits[, 4]
          # rpart object의 splits value를 통해 classing되는 구간을 찾음
          data.QI.GH[, i] <- cut(data.QI.GH[, i], c(0, split.point, Inf))
          # cut() function을 통해 위에 split.point 결과를 가지고 grouping
        }
        
        # rpart result : NOT ("None", "root[1]")
        # 특히, `root[1]`로 나타나는 경우에는 TA에 대한 설명력이 없다는 의미임
        else{
          data.QI.GH[, i] <- as.numeric(data.QI.GH[, i])
        }
        cat('\n Numeric QI generalization >> \n')
      }
      
      # CASE II : Factor QI
      if(qi.class[i] != T){
        data.QI.GH[, i] <- as.character(data.QI.GH[, i])
        
        # rpart result : EXIST
        if(class(obj[[x]][[i]]) == 'rpart' && class(obj[[x]][[i]]$csplit) == 'matrix'){
          
          split.point <- t(obj[[x]][[i]]$csplit) 
          # terminal node로 향하는 split 방향에 대한 matrix
          grp.mat <- uniquecombs(split.point) # terminal node
          grp.ind <- attr(grp.mat, 'index') # terminal node index
          
          grp.ind.nlevels <- nrow(grp.mat) # terminal node level 갯수
          grp.ind.levels <- levels(as.factor(grp.ind)) # terminal node factor level
          
          # grouping levels
          dat.levels <- levels(as.factor(data.QI.GH[, i]))
          
          # 위 결과에 의해 factor level의 QI를 grouping하는 과정
          for(j in 1:grp.ind.nlevels){
            data.QI.GH[data.QI.GH[, i] %in% dat.levels[grp.ind == j], i] <- j
          }
          data.QI.GH[, i] <- as.factor(data.QI.GH[, i])
        }
        
        # rpart result : NOT ("None", "root[1]")
        else{
          data.QI.GH[, i] <- as.factor(data.QI.GH[, i])
        }
        cat('\n Factor QI generalization >> \n')
      }
      cat(paste('\n\n\n QI - ', i, ' : Completed >> \n\n\n'))
    }
    data.QI.GH
  })
  
  # class 지정
  class(data.QI.GH.res) <- c('qigrp', 'list')
  
  # attributes 추가
  attr(data.QI.GH.res, 'QI') <- attr(obj, 'QI') # QI
  attr(data.QI.GH.res, 'cp') <- attr(obj, 'cp') # cp values
  attr(data.QI.GH.res, 'nnode') <- attr(obj, 'nnode') # 노드의 갯수
  
  return(data.QI.GH.res)
}