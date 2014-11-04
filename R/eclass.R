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