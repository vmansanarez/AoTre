

################################################################################
#' @title Mann-Kendall trend analysis 
#' @description Apply the generalMannKendall function to the serie X.
#' @param X data (vector). IMPORTANT: it assumes that X is regularly-spaced.
#' @param level numeric, between 0 and 1, level of the test (default: 0.1).
#' @param dep.option dependency option, option for handling temporal dependence
#' (default 'INDE' for independence).
#' @param DoDetrending logical, only used for when dep.option == 'LTP'
#' (default: TRUE).
#' @return a dataframe, with the different statistics and values of interest of
#' the test.
#' @examples
#' GeneralMannKendall.wrap(X=1:100)
#' GeneralMannKendall.wrap(X=rep(1,100))
#' @export
GeneralMannKendall.wrap=function(X,level=0.1
                                 ,dep.option='INDE'
                                 ,DoDetrending=TRUE){
  ### Assume that the package BFunk is installed on the machine
  ### Apply the Mann Kendall test on vector X
  res.test=generalMannKendall(X = X,level=level,dep.option=dep.option,DoDetrending=DoDetrending)
  ### Results to be returned, remove H and Dep as can be computed back from p, the p-value
  data.tmp=data.frame(p=res.test$P,stat=res.test$STAT,trend=res.test$TREND)
  return(list(data.tmp))
}



################################################################################
#' @title Perform a specified staticitcal trend analysis
#' @description Applies a staticitcal trend analysis to an extracted variable,
#' result from the function 'extract.Var()'
#' @param data.extract data.tibble (tbl). IMPORTANT: results from function
#' 'extract.Var()'.
#' @param funct.stat function, function to aggregate on data.extract. Apply by
#' default the function 'GeneralMannKendall.wrap' to perform a Mann-Kendall
#' trend analysis on data.extract.
#' @param list.stats vector, optional argument, information to add to the
#' returned results.
#' @param do.pval_FDR logical, TRUE if the p-value accounts for the field
#' significance using the false detection rate (FDR) approach.
#' @param level.FDR numeric between 0 and 1, level used in the FDR approach
#' @param ... other arguments passed to function \code{funct.stat}.
#' @return a dataframe, with the different results of the trend analysis.
#' @export
Estimate.stats=function(data.extract
                        ,funct.stat=GeneralMannKendall.wrap
                        ,list.stats=NULL
                        ,do.pval_FDR=FALSE
                        ,level.FDR=0.1
                        ,...){

  ### [WARNING]: aggregation to be modified to account for more groups
  ### Aggregation of function 'funct.stat' on data value column from
  # data.extract.
  if("datetime" %in% colnames(data.extract)){
    ### Data already agregated by Date in the previous step (extraction of
    # variable)
    group.names=dplyr::setdiff(colnames(data.extract),c("datetime","values"))
  }else{
    group.names=dplyr::setdiff(colnames(data.extract),"values")
  }
  ### Group data accordingly to group.names
  data.Y.extract=dplyr::group_by_at(.tbl=data.extract,.vars = group.names)

  ### Aggregate function 'funst.stat' on each group of data.Y.extract
  # Select the groups
  data.extract.select=dplyr::select(data.Y.extract,dplyr::all_of(c(group.names,
                                                                   "values")))
  # apply function on values accounting for grouping variables 'group.names"
  data.extract.fin=dplyr::summarise(.data = data.extract.select,
                                    dplyr::across(.cols = "values",
                                                  .fns = funct.stat,
                                                  ...))
  # Remove object 'data.extract.select' to free memory
  rm(data.extract.select)

  ### catch names of the results from funct.stat
  colnames.funct=colnames(data.extract.fin$values[[1]])

  ### Create data.frame for the returned results
  data.final=cbind.data.frame(group=dplyr::select(data.extract.fin,
                                                  dplyr::all_of(group.names)),
                              as.data.frame(t(matrix(unlist(data.extract.fin$values,
                                                            use.names = TRUE),
                                                     nrow=length(data.extract.fin$values[[1]])))))

  colnames(data.final)=c(group.names,colnames.funct)

  if(!is.null(list.stats)){
    data.final$station=list.stats
  }

  if(do.pval_FDR){
    data.final$p.FDR=fieldSignificance_FDR(data.final$p,level=level.FDR)
  }

  return(data.final)
}


