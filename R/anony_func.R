#' Partition Data
#' 
#' @param dat Original data
#' @param QI Quasi-Identifiers
#' @param TA Target Attribute
#' @param ...
#' @return Partitioning Data by Quasi-Identifiers and Target Attribute
#' @examples
#' data(AdultUCI, package='arules')
#' # TA (Target)
#' TA <- c('income')
#' # QI (Quasi-Identifiers)
#' QI <- c('age', 'workclass', 'education', 'marital.status', 'occupation', 'race', 'sex', 'native.country')
#' #QI <- c('age', 'workclass', 'education', 'marital.status')
#' # SA (Sensitive Attributes)
#' SA <- c('capital.gain', 'capital.loss')
#' # IS (Insensitive)
#' IS <- c('relationship', 'hours.per.week', 'education.num')
#' # Remove
#' RM <- c('fnlwgt')
#' 
#' parda(AdultUCI, QI, TA) 
parda <- function(dat, QI, TA, ...){
  # Description : Partition Data (only QI, TA)
  #
  # Arguments
  # dat : Original data
  # QI : Quasi-identifiers
  # TA : Target Attribute
  
  # Partition data
  data.QI <- subset(dat, select = c(QI, TA))
  
  class(data.QI) <- c('parda', 'data.frame')
    
  # Attributes
  attr(data.QI, 'QI') <- QI # Quasi-Identifiers
  attr(data.QI, 'TA') <- TA # Target Attribute
  attr(data.QI, 'nQI') <- length(QI) # number of QI
  attr(data.QI, 'nobs') <- nrow(data.QI) # number of obs.
  
  return(data.QI)
}



#' Select a CP values
#' 
#' @param n number of cp intervals
#' @param len number of samples
#' @param m number of cp sample matrix rows (less than )
#' @param ...
#' @return Selected CP values
cpselec <- function(n = 10, len = 100, m = 2, ...){
  # Description : Select cp
  #
  # Arguments
  # n : # of cp intervals
  # len : # of samples
  # m : # of cp sample matrix rows
  
  # cp값을 랜덤하게 뽑는 과정
  cp.vec <- unlist(lapply(seq_len(n), function(x){
    # uniform 분포를 이용하여 0부터 10^-x까지의 구간을 len개 생성
    # 구간은 n개가 생성이 됨
    rx <- runif(n = len, min = 0, max = 10^-x)
    # 각 구간마다 1개를 랜덤하게 추출
    sample(rx, size = 1)
  }))
  # 각 구간별 (n개의 구간) (으)로 1개씩 생성해서 n개의 cp값을 리턴
  
  # 위에서 뽑은 cp값에 대해서 m개의 cp값을 임의로 추출
  cp.val <- sample(cp.vec, size = m)
  
  class(cp.val) <- class('cpselec')
  
  # Attributes
  attr(cp.val, 'ncp') <- length(cp.val)
  
  return(cp.val)
}



#' Classing QI by \code{rpart} function
#' 
#' @param dat \code{parda} object
#' @param cp Cost Complexity Parameter. \code{cpselec} object
#' @param loss.mat Loss matrix
#' @param cval number of cross-validations
#' @param mc if TRUE \code{mclapply} applied, FALse \code{lapply} applied (default).
#' @param mcs number of cores
#' @param ...
#' @return Classed a Quasi-Identifiers by CP using Entropy
rpaclass <- function(dat, cp = 0.01, loss.mat = matrix(c(0,1,2,0), ncol=2), cval=10, mc = FALSE, mcs = NULL, ...){
  # Description : QI classing by cp
  #
  # Arguments
  # dat : parda Data Object
  # cp : cp value (matrix) - results of `cpselec`
  # loss.mat : loss matrix
  # cval : # of cross-validation
  # mc : if TRUE (use mclapply), FALSE (default, use lapply)
  # mcs : # of cores
  
  # 필요 패키지 불러오기
  require(rpart)
  # Recursive Partitioning and Regression Trees
  require(pbapply)
  # Adding Progress Bar to '*apply' Functions
  
  # mclapply.hack.R sourcing
  source('../mclapply.hack.R')
  
  # `parda`에서 return된 object의 attr을 가져옴
  ta <- attr(dat, 'TA') # TA
  qi <- attr(dat, 'QI') # QI
  
  # lapply (mc = FALSE)
  if(mc == FALSE){
    # rpart result by cp by QI 
    # cp값 마다 function 수행
    cp.res <- pblapply(cp, function(x){
      # qi 마다 function 수행
      pblapply(qi,
               function(var){
                 # Formula
                 Fmla <- as.formula(paste(ta, '~', var))
                 
                 # factor level에 따라서 rpart를 통해 classing을 할 것인지 말 것인지 판단
                 # QI factor level >= 5 
                 if(nlevels(as.factor(dat[, var])) >= 5){
                   # rpart 함수 적용하여 QI classing 수행
                   rpart(Fmla,
                         data = dat,
                         method = 'class',
                         parms = list(loss = loss.mat, split = 'information'),
                         control = rpart.control(cp = x, xval = cval))
                 }
                 
                 else {'None'}
                 # QI factor level < 5
                 # 이 때는 'None'이라고 결과값을 리턴
               })
    })
  }
  
  # mclapply (mc = TRUE)
  if(mc == TRUE){
    # rpart result by cp by QI
    # cp값 마다 function 수행
    cp.res <- mclapply(cp, function(x){
      # qi 마다 function 수행
      mclapply(qi,
               function(var){
                 # Formula
                 Fmla <- as.formula(paste(ta, '~', var))
                 
                 # factor level에 따라서 rpart를 통해 classing을 할 것인지 말 것인지 판단
                 # QI factor level >= 5
                 if(nlevels(as.factor(dat[, var])) >= 5){
                   # rpart 함수 적용하여 QI classing 수행
                   rpart(Fmla,
                         data = dat,
                         method = 'class',
                         parms = list(loss = loss.mat, split = 'information'),
                         control = rpart.control(cp = x, xval = cval))
                 }
                 
                 else {'None'}
                 # QI factor level < 5
                 # 이 때는 'None'이라고 결과값을 리턴
               }, num = mcs)
    }, num = mcs)
  }
  
  class(cp.res) <- c('rpaclass', 'list')
  
  # Attributes 추가
  attr(cp.res, 'Call') <- match.call()
  attr(cp.res, 'n') <- attr(dat, 'nobs') # nobs.
  attr(cp.res, 'cp') <- cp # cp values
  attr(cp.res, 'QI') <- qi # QI
  attr(cp.res, 'nQI') <- attr(dat, 'nQI') # QI 갯수
  attr(cp.res, 'ncp') <- attr(cp, 'ncp') # cp값 갯수
  
  return(cp.res)
}


#' Create a Lattice Node
#' 
#' @param obj \code{rpaclass} object
#' @param mc if TRUE \code{mclapply} applied, FALse \code{lapply} applied (default).
#' @param mcs number of cores
#' @param ...
#' @return Created Lattice Node using result of QI Classing
lanode <- function(obj, mc = FALSE, mcs = NULL, ...){
  # Description : Lattice Node
  # 
  # Arguments
  # obj : cpctrl object
  # mc : if TRUE (use mclapply), FALSE (default, use lapply)
  # mcs : # of Cores
  
  require(pbapply)
  # Adding Progress Bar to '*apply' Functions
  source('./mclapply.hack.R')
  # mclapply.hack.R sourcing
  
  
  qi <- attr(obj, 'QI')
  cp <- attr(obj, 'cp')
  n.qi <- attr(obj, 'nQI')
  n.cp <- attr(obj, 'ncp')
  
  
  # cp값에 따른 QI별로 가능한 조합 수
  # cp값 개수 ^ QI 개수
  lattice.pairs <- expand.grid(rep(list(seq_len(n.cp)), n.qi))
  # expand.grid : Create a Data Frame from All Combinations of Factors
  
  lattice.node <- list()
  # 비어있는 list를 만듦
  length(lattice.node) <- nrow(lattice.pairs)
  # list의 length를 lattice.pairs의 nrow로 지정
  
  # lapply (mc = FALSE)
  if(mc == FALSE){
    lattice.node <- pblapply(seq_len(nrow(lattice.pairs)), function(lat){
      pblapply(seq_len(ncol(lattice.pairs)), function(no){
        ind <- lattice.pairs[lat, no]
        obj[[ind]][[no]]})
    })
  }
  
  # mclapply (mc = TRUE)
  if(mc == TRUE){
    lattice.node <- mclapply(seq_len(nrow(lattice.pairs)), function(lat){
      mclapply(seq_len(ncol(lattice.pairs)), function(no){
        ind <- lattice.pairs[lat, no]
        obj[[ind]][[no]]
      }, num = mcs)
      
    }, num = mcs)    
  }
  
  # class 지정
  class(lattice.node) <- c('lanode', 'list')
  
  attr(lattice.node, 'lattice_pairs') <- lattice.pairs
  # lattice node의 generalization hierarchy
  
  # Attributes 추가
  attr(lattice.node, 'QI') <- qi # QI
  attr(lattice.node, 'cp') <- cp # cp values
  attr(lattice.node, 'nQI') <- attr(obj, 'nQI') # QI 갯수
  attr(lattice.node, 'ncp') <- attr(obj, 'ncp') # cp 값의 갯수
  attr(lattice.node, 'nnode') <- length(lattice.node) # lattice node의 갯수
  
  return(lattice.node)
}


#' Quasi-Identifiers Grouping
#' 
#' @param dat \code{parda} object
#' @param obj \code{lanode} object
#' @param mcs number of cores
#' @param ...
#' @return Grouped Data by each lattice node 
qigrp <- function(dat, obj, mcs = 4, ...){
  # Description : QI grouping!
  #
  # Arguments
  # dat : (QI, TA) subest data
  # obj : lanode object
  # mcs : # of Cores
  
  require(mgcv) ## uniquecombs() : find the unique rows in a matrix
  source('./mclapply.hack.R')
  
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
  }, 
  num = mcs)
  
  # class 지정
  class(data.QI.GH.res) <- c('qigrp', 'list')
  
  # attributes 추가
  attr(data.QI.GH.res, 'QI') <- attr(obj, 'QI') # QI
  attr(data.QI.GH.res, 'cp') <- attr(obj, 'cp') # cp values
  attr(data.QI.GH.res, 'nnode') <- attr(obj, 'nnode') # 노드의 갯수
  
  return(data.QI.GH.res)
}



#' Equivalence Class
#' 
#' @param dat \code{qigrp} object
#' @param QI Quasi-Identifiers
#' @param TA Target Attribute
#' @param ...
#' @return Equivalence Class and Size
eclass <- function(dat, QI, TA, ...){
  # Description : Equivalence Class and Size
  #
  # Arguments
  # dat : QIgrouping Data Object
  # QI : QI
  # TA : TA
  
  # 필요 패키지 불러오기
  require(reshape2)
  require(pbapply)
  
  equi.res <- pblapply(1:length(dat), function(x){
    data.QI.sort <- dat[[x]][order(dat[[x]][, QI]), ]
    # QI grouping 결과 데이터를 QI마다 ordering을 시킴
    # 즉, equivalence class끼리 볼 수 있도록 하는 것임
    rownames(data.QI.sort) <- NULL
    
    # equivalence class size 산출
    equi.class <- melt(data.QI.sort,
                       id.vars = QI,
                       measure.vars = TA)
    equi.class <- dcast(equi.class, ... ~ variable, length)
    colnames(equi.class)[ncol(equi.class)] <- 'equi.size'
    
    attr(equi.class, 'NequiClass') <- nrow(equi.class)
    attr(equi.class, 'equi.size') <- equi.class$equi.size
    
    equi.class
  })
  
  # class 지정
  class(equi.res) <- c('eclass', 'list')
  
  # Attributes 추가
  attr(equi.res, 'n') <- length(equi.res) # equivalence class의 갯수
  
  return(equi.res)
}


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
  s <- ggplot(data = obj, aes(x = supp_rank, y = supp)) + geom_line() + geom_point()
  # DM
  d <- ggplot(data = obj, aes(x = DM_rank, y = DM)) + geom_line() + geom_point()
  # Entropy
  e <- ggplot(data = obj, aes(x = Ent_rank, y = Entropy)) + geom_line() + geom_point()
  
  multiplot(s, d, e)  
}
