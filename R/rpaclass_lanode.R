# rpaclass function ----------------------------------------

#' Classing QI by \code{rpart} function
#' 
#' @param dat \code{parda} object
#' @param cp Cost Complexity Parameter. \code{cpselec} object
#' @param loss.mat Loss matrix
#' @param cval number of cross-validations
#' @param mc if TRUE \code{mclapply} applied, FALSE \code{lapply} applied (default).
#' @param ...
#' @return Classed a Quasi-Identifiers by CP using Entropy
rpaclass <- function(dat, cp = 0.01, loss.mat = matrix(c(0,1,2,0), ncol=2), cval=10, mc = FALSE, ...){
  # Description : QI classing by cp
  #
  # Arguments
  # dat : parda Data Object
  # cp : cp value (matrix) - results of `cpselec`
  # loss.mat : loss matrix
  # cval : # of cross-validation
  # mc : if TRUE (use mclapply), FALSE (default, use lapply)
  
  # 필요 패키지 불러오기
  require(rpart)
  # Recursive Partitioning and Regression Trees
  require(pbapply)
  # Adding Progress Bar to '*apply' Functions
  
  # mclapply.hack.R sourcing
  source('http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R')
  
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
               })
    })
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



# lanode function ----------------------------------------

#' Create a Lattice Node
#' 
#' @param obj \code{rpaclass} object
#' @param mc if TRUE \code{mclapply} applied, FALSE \code{lapply} applied (default).
#' @param ...
#' @return Created Lattice Node using result of QI Classing
lanode <- function(obj, mc = FALSE, ...){
  # Description : Lattice Node
  # 
  # Arguments
  # obj : cpctrl object
  # mc : if TRUE (use mclapply), FALSE (default, use lapply)
  
  require(pbapply)
  # Adding Progress Bar to '*apply' Functions
  
  # mclapply.hack.R sourcing
  source('http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R')
  
  
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
      
    })    
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


