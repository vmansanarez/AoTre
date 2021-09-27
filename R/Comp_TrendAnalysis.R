

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
  ### Apply the Mann Kendall test on vector X
  res.test=GeneralMannKendall(X = X,level=level,dep.option=dep.option,DoDetrending=DoDetrending)
  ### Results to be returned
  data.tmp=data.frame(H=res.test$H,p=res.test$P,stat=res.test$STAT,trend=res.test$TREND,dep=res.test$DEP)
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
  ### [WARNING]: aggregation to be modified to account for more groups
  ### Aggregation of function 'funct.stat' on data value column from data.extract.
  data.Y.extract=aggregate.data.frame(x=data.extract$value,
                                      by = list(group=data.extract$group),
                                      FUN = funct.stat,...)
  ### Create data.frame for the returned results
  data.final=cbind.data.frame(group=data.Y.extract$group,
                              as.data.frame(t(matrix(unlist(data.Y.extract$x,use.names = TRUE),nrow=5))))

  colnames(data.final)=c("group","H","p","stat","trend","dep")

  ### Code line to be [2VERIFIED]. Seems specific to Mann-kendall function
  data.final=data.final[,-c(2,6)]

  if(!is.null(list.stats)){
    data.final$station=list.stats
  }

  return(data.final)
}


