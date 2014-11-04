#' Create a Generalization Table
#' 
#' @param dat \code{qigrp} object
#' @param ori.dat Original Data
#' @param QI Quasi-Identifiers
#' @param TA Target Attribute
#' @param ...
#' @return Generalization Table
cregt <- function(dat, ori.dat, QI, TA, ...){
  # Description : create GT (Generalization Table)
  #
  # Arguments
  # dat : QI grouping Data
  # ori.dat : Original Data
  # QI : QI
  # TA : TA
  
  
  ori.dat[, c(QI, TA)] <- dat
  
  ori.dat[, RM] <- NULL
  
  attr(ori.dat, 'TA') <- attr(dat, 'TA')
  attr(ori.dat, 'QI') <- attr(dat, 'QI')
  attr(ori.dat, 'Vars') <- names(ori.dat)[!(names(ori.dat) %in% TA)]
  
  return(ori.dat)
}