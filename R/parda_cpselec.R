# parda function ----------------------------------------


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




# cpselec function ----------------------------------------

#' Select a CP values
#' 
#' @param n number of cp intervals
#' @param len number of samples
#' @param m number of cp sample matrix rows
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
  
  # Attributes
  attr(cp.val, 'ncp') <- length(cp.val)
  
  return(cp.val)
}
