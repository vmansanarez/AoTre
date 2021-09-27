

######################################################################
#' @title Mann-Kendall trend analysis
#' @description Apply the GeneralMannKendall function from BFunk package to the serie X.
#' @param X data (vector). IMPORTANT: it assumes that X is regularly-spaced.
#' @param level real, between 0 and 1, level of the test (default: 0.1).
#' @param dep.option dependency option, option for handling temporal dependence (default 'INDE' for independence).
#' @param DoDetrending logical, only used for dep.option == 'LTP' (default: TRUE).
#' @return a dataframe, with the different statistics and values of interest of the test.
#' @examples
#' GeneralMannKendall.wrap(X=data.Var$value)
#' @export
GeneralMannKendall.wrap=function(X,level=0.1,dep.option='INDE',DoDetrending=TRUE){
  ### Assume that the package BFunk is installed on the machine
  require(BFunk)
  ### Apply the Mann Kendall test on vector X
  res.test=generalMannKendall(X = X,level=level,dep.option=dep.option,DoDetrending=DoDetrending)
  ### Results to be returned, remove H and Dep as can be computed back from p, the p-value
  data.tmp=data.frame(p=res.test$P,stat=res.test$STAT,trend=res.test$TREND)
  return(list(data.tmp))
}



######################################################################
#' @title Perform a specified staticitcal trend analysis
#' @description Applies a staticitcal trend analysis to an extracted variable, result from the function 'extract.Var()'
#' @param data.extract data.tibble (tbl). IMPORTANT: results from function extract.Var()'.
#' @param funct.stat function, function to aggregate on data.extract. Apply by default the function
#' 'GeneralMannKendall.wrap' to perform a Mann-Kendall trend analysis on data.extract.
#' @param list.stats vector, optional argument, information to add to the returned results.
#' @return a dataframe, with the different results of the trend analysis.
#' @examples
#' GeneralMannKendall.wrap(X=data.Var$value)
#' @export
Estimate.stats=function(data.extract
                        ,funct.stat=GeneralMannKendall.wrap
                        ,list.stats=NULL
                        ,...){
  require(dplyr)
  ### [WARNING]: aggregation to be modified to account for more groups
  ### Aggregation of function 'funct.stat' on data value column from data.extract.
  if("datetime" %in% colnames(data.extract)){
    ### Data already agregated by Date in the previous step (extraction of variable)
    group.names=setdiff(colnames(data.extract),c("datetime","values"))
  }else{
    group.names=setdiff(colnames(data.extract),"values")
  }
  ### Group data accordingly to group.names
  data.Y.extract=group_by_at(.tbl=data.extract,.vars = group.names)

  ### Aggregate function 'funst.stat' on each group of data.Y.extract
  data.extract.fin=summarise_each(tbl = select(data.Y.extract,c(group.names,"values")),funs = funct.stat)
  ### catch names of the results from funct.stat
  colnames.funct=colnames(data.extract.fin$values[[1]])

  ### Create data.frame for the returned results
  data.final=cbind.data.frame(group=select(data.extract.fin,group.names),
                              as.data.frame(t(matrix(unlist(data.extract.fin$values,use.names = TRUE)
                                                     ,nrow=length(data.extract.fin$values[[1]])))))

  colnames(data.final)=c(group.names,colnames.funct)

  if(!is.null(list.stats)){
    data.final$station=list.stats
  }

  return(data.final)
}


